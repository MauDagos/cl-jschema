(in-package :cl-jschema)


;;; TODO: switch to JSON Pointer
;;; Keeping track of JSON path while validating

(defvar *json-path*)


(defun call-with-registered-json-path (body-fn &key key index)
  (let* ((path (cond
                 (key (format nil ".~a" key))
                 (index (format nil "[~d]" index))))
         (*json-path* (cons path *json-path*)))
    (funcall body-fn)))


(defmacro with-registered-json-path ((&key key index) &body body)
  (assert (alexandria:xor key index))
  `(call-with-registered-json-path (lambda () ,@body)
                                   :key ,key
                                   :index ,index))


;;; Handling conditions

(define-condition invalid-json (error)
  ((error-message :initarg :error-message
                  :reader invalid-json-error-message)
   (json-path :initarg :json-path
              :reader invalid-json-path)
   (errors :initarg :errors
           :initform nil
           :accessor invalid-json-errors))
  (:report (lambda (condition stream)
             (alexandria:if-let ((errors (invalid-json-errors condition)))
               (format stream "JSON Schema validation found these errors:~
                               ~{~2%~a~^~}"
                       errors)
               (format stream "~a : ~a"
                       (invalid-json-path condition)
                       (invalid-json-error-message condition))))))


(defmethod track-invalid-json-error ((self invalid-json) error)
  (push error (invalid-json-errors self)))


(defun raise-invalid-json (keyword &optional value)
  (let ((format-message (keyword-validation-message
                         ;; Keywords from 'CHECK-TYPE-PROPERTY are arriving as
                         ;; Lisp keywords, so convert them back to strings.
                         (string keyword))))
    (with-simple-restart (continue-validating "Continue validating the JSON")
      (error 'invalid-json
             :error-message (format nil format-message value)
             :json-path (apply 'concatenate 'string (reverse *json-path*))))))


(defvar *inside-call-with-valid-json-p* nil)


(defun call-with-valid-json-p (body-fn)
  (let ((*inside-call-with-valid-json-p* t))
    (handler-bind ((invalid-json (lambda (e)
                                   (declare (ignore e))
                                   (return-from call-with-valid-json-p nil))))
      (funcall body-fn)
      t)))


(defmacro with-valid-json-p (&body body)
  "Make BODY return T if no errors were triggered or return NIL if an
INVALID-JSON was triggered."
  `(call-with-valid-json-p (lambda () ,@body)))


;;; Keeping track of validated properties in a JSON object, for being able to
;;; validate unevaluatedProperties.
;;;
;;; Use a hash table for mapping json objects to cons cells whose car is the
;;; list of evaluated properties and cdr is the list of evaluated invalid
;;; properties.
(defvar *evaluated-properties-map*)


(defun call-with-tracked-property (json-object keyword body-fn)
  (let ((properties (alexandria:ensure-gethash json-object
                                               *evaluated-properties-map*
                                               (cons nil nil))))
    ;; Track the property being evaluated for this json object
    (pushnew keyword (car properties) :test 'equal)
    (handler-bind ((invalid-json (lambda (e)
                                   (declare (ignore e))
                                   ;; Track the property as being invalid
                                   (pushnew keyword (cdr properties)
                                            :test 'equal))))
      (funcall body-fn))))


(defmacro with-tracked-property (json-object keyword &body body)
  `(call-with-tracked-property ,json-object ,keyword (lambda () ,@body)))


;;; Validating

(defmethod check-type-property ((keyword (eql :|type|))
                                type value)
  (flet ((%check (type)
           (let ((spec (type-spec type)))
             (or (typep value (type-spec-lisp-type spec))
                 (raise-invalid-json keyword type)))))
    (etypecase type
      (string (%check type))
      (list
       (or (some (lambda (type*)
                   (with-valid-json-p (%check type*)))
                 type)
           (raise-invalid-json "types" type))))))


(defmethod check-type-property ((keyword (eql :|minLength|))
                                min-length value)
  (when (stringp value)
    (or (>= (length value) min-length)
        (raise-invalid-json keyword min-length))))


(defmethod check-type-property ((keyword (eql :|maxLength|))
                                max-length value)
  (when (stringp value)
    (or (<= (length value) max-length)
        (raise-invalid-json keyword max-length))))


(defmethod check-type-property ((keyword (eql :|pattern|))
                                pattern value)
  (when (stringp value)
    (or (ppcre:scan (regex pattern) value)
        (raise-invalid-json keyword (regex-string pattern)))))


(defmethod check-type-property ((keyword (eql :|multipleOf|))
                                multiple-of value)
  (when (numberp value)
    (or (zerop (mod value multiple-of))
        (raise-invalid-json keyword multiple-of))))


(defmethod check-type-property ((keyword (eql :|minimum|))
                                minimum value)
  (when (numberp value)
    (or (>= value minimum)
        (raise-invalid-json keyword minimum))))


(defmethod check-type-property ((keyword (eql :|exclusiveMinimum|))
                                exclusive-minimum value)
  (when (numberp value)
    (or (> value exclusive-minimum)
        (raise-invalid-json keyword exclusive-minimum))))


(defmethod check-type-property ((keyword (eql :|maximum|))
                                maximum value)
  (when (numberp value)
    (or (<= value maximum)
        (raise-invalid-json keyword maximum))))


(defmethod check-type-property ((keyword (eql :|exclusiveMaximum|))
                                exclusive-maximum value)
  (when (numberp value)
    (or (< value exclusive-maximum)
        (raise-invalid-json keyword exclusive-maximum))))


(defmethod check-type-property ((keyword (eql :|properties|))
                                properties value)
  (when (typep value 'hash-table)
    (maphash (lambda (key json-schema)
               (alexandria:when-let ((object-value (gethash key value)))
                 (with-registered-json-path (:key key)
                   (with-tracked-property value key
                     (check-schema json-schema object-value)))))
             properties)))


(defmethod check-type-property ((keyword (eql :|propertyNames|))
                                property-names value)
  (when (typep value 'hash-table)
    (maphash (lambda (object-key object-value)
               (declare (ignore object-value))
               (with-registered-json-path (:key object-key)
                 (with-tracked-property value object-key
                   (check-schema property-names object-key))))
             value)))


(defmethod check-type-property ((keyword (eql :|patternProperties|))
                                pattern-properties value)
  (when (typep value 'hash-table)
    (maphash (lambda (regex-box schema)
               (maphash (lambda (object-key object-value)
                          (when (ppcre:scan (regex regex-box) object-key)
                            (with-registered-json-path (:key object-key)
                              (with-tracked-property value object-key
                                (check-schema schema object-value)))))
                        value))
             pattern-properties)))


(defmethod check-type-property ((keyword (eql :|required|))
                                required value)
  (when (and (typep value 'hash-table) required)
    (loop
      for field in required
      always (or (gethash field value)
                 (raise-invalid-json keyword field)))))


(defmethod check-type-property ((keyword (eql :|dependentRequired|))
                                dependent-required value)
  (when (typep value 'hash-table)
    (maphash (lambda (dependent required)
               (when (gethash dependent value)
                 (check-type-property :|required| required value)))
             dependent-required)))


(defmethod check-type-property ((keyword (eql :|dependentSchemas|))
                                dependent-schemas value)
  (when (typep value 'hash-table)
    (maphash (lambda (dependent schema)
               (when (gethash dependent value)
                 (check-schema schema value)))
             dependent-schemas)))


(defmethod check-type-property ((keyword (eql :|minProperties|))
                                min-properties value)
  (when (typep value 'hash-table)
    (or (>= (hash-table-count value) min-properties)
        (raise-invalid-json keyword min-properties))))


(defmethod check-type-property ((keyword (eql :|maxProperties|))
                                max-properties value)
  (when (typep value 'hash-table)
    (or (<= (hash-table-count value) max-properties)
        (raise-invalid-json keyword max-properties))))


(defmethod check-type-property ((keyword (eql :|prefixItems|))
                                prefix-items value)
  (when (typep value 'array)
    (loop
      for item across value
      for index from 0
      for json-schema in prefix-items
      do (with-registered-json-path (:index index)
           (check-schema json-schema item)))))


(defmethod check-type-property ((keyword (eql :|minItems|))
                                min-items value)
  (when (typep value 'array)
    (or (>= (length value) min-items)
        (raise-invalid-json keyword min-items))))


(defmethod check-type-property ((keyword (eql :|maxItems|))
                                max-items value)
  (when (typep value 'array)
    (or (<= (length value) max-items)
        (raise-invalid-json keyword max-items))))


(defmethod check-type-property ((keyword (eql :|uniqueItems|))
                                unique-items value)
  (when (and (typep value 'array)
             (json-true-p unique-items))
    (loop
      with items = value
      for item across items
      for index from 0
      when (loop
             for item* across items
             for index* from 0
             thereis (unless (= index index*)
                       (json-equal item item*)))
        return (raise-invalid-json keyword))))


(defun check-type-properties (type-schema value)
  (alexandria:when-let ((type-properties (type-properties type-schema)))
    (maphash (lambda (prop-keyword schema-prop)
               (check-type-property prop-keyword
                                    (value schema-prop)
                                    value))
             type-properties)))


(defun check-additional-properties (object-schema json-object)
  ;; At this point, the value must've already been validated to be a JSON
  ;; object, so no need to gaurd against it not being one.
  (let ((properties (get-type-property object-schema "properties"))
        (additional-properties (additional-properties object-schema)))
    (etypecase additional-properties
      ;; True: allow everything.
      (json-true-schema)
      ;; False: don't allow any additional.
      (json-false-schema
       (let ((available-props (alexandria:hash-table-keys properties)))
         (maphash (lambda (key val)
                    (declare (ignore val))
                    (or (member key available-props :test 'equal)
                        (raise-invalid-json "additionalProperties" key)))
                  json-object)))
      ;; Schema: only allow additional if they're valid.
      (json-schema
       (let ((available-props (alexandria:hash-table-keys properties)))
         (maphash (lambda (key val)
                    (or (member key available-props :test 'equal)
                        (with-registered-json-path (:key key)
                          (with-tracked-property json-object key
                            (check-schema additional-properties val)))))
                  json-object))))))


(defun check-unevaluated-properties (object-schema json-object)
  ;; At this point, the value must've already been validated to be a JSON
  ;; object, so no need to gaurd against it not being one.
  (let* ((unevaluated-properties (unevaluated-properties object-schema))
         (evaluated-properties (gethash json-object *evaluated-properties-map*))
         (validated-properties (set-difference (car evaluated-properties)
                                               (cdr evaluated-properties)
                                               :test 'equal)))
    (etypecase unevaluated-properties
      ;; True: allow all.
      (json-true-schema)
      ;; False: don't allow any unevaluated.
      (json-false-schema
       (maphash (lambda (key val)
                  (declare (ignore val))
                  (or (member key validated-properties :test 'equal)
                      (raise-invalid-json "additionalProperties" key)))
                json-object))
      ;; Schema: only allow unevaluated if they're valid.
      (json-schema
       (maphash (lambda (key val)
                  (or (member key validated-properties :test 'equal)
                      (with-registered-json-path (:key key)
                        (check-schema unevaluated-properties val))))
                json-object)))))


(defun check-items (array-schema json-array)
  ;; At this point, the value must've already been validated to be a JSON
  ;; array, so no need to gaurd against it not being one.
  (let* ((prefix-items (get-type-property array-schema "prefixItems"))
         (items (items array-schema)))
    (etypecase items
      ;; True: allow everything.
      (json-true-schema)
      ;; False: don't allow any additional if prefixItems was set.
      (json-false-schema
       (when (and prefix-items
                  (> (length json-array)
                     (length prefix-items)))
         (raise-invalid-json "items")))
      ;; Schema has two cases:
      ;; If prefixItems was set: check that additional items are valid.
      ;; Else: check that all items are valid
      (json-schema
       (let* ((start (if prefix-items
                         (length prefix-items)
                         0))
              (index start))
         (maparray (lambda (item)
                     (with-registered-json-path (:index index)
                       (check-schema items item))
                     (incf index))
                   json-array
                   :start start))))))


(defun check-contains (array-schema json-array)
  ;; At this point, the value must've already been validated to be a JSON
  ;; array, so no need to gaurd against it not being one.
  (alexandria:when-let ((contains (contains array-schema)))
    (let ((min-contains (or (min-contains array-schema) 1))
          (max-contains (max-contains array-schema))
          (valid-count 0))
      (maparray (lambda (item)
                  (when (with-valid-json-p (check-schema contains item))
                    (incf valid-count)
                    (when (and max-contains (> valid-count max-contains))
                      (raise-invalid-json "maxContains" max-contains))))
                json-array)
      (when (< valid-count min-contains)
        (raise-invalid-json "minContains" min-contains)))))


(defmethod check-type-schema ((const-schema const-schema) value)
  (or (json-equal value (const const-schema))
      (raise-invalid-json "const")))


(defmethod check-type-schema ((enum-schema enum-schema) value)
  (or (member value (items enum-schema) :test 'json-equal)
      (raise-invalid-json "enum")))


(defmethod check-type-schema ((type-schema json-type-schema) value)
  (check-type-properties type-schema value))


(defmethod check-type-schema :after ((object-schema json-object-schema) value)
  (check-additional-properties object-schema value)
  (check-unevaluated-properties object-schema value))


(defmethod check-type-schema :after ((array-schema json-array-schema) value)
  (check-items array-schema value)
  (check-contains array-schema value))


(defun check-condition-schemas (condition-schemas value)
  (let ((if-schema   (gethash "if"   condition-schemas))
        (then-schema (gethash "then" condition-schemas))
        (else-schema (gethash "else" condition-schemas)))
    (when if-schema
      (if (with-valid-json-p (check-schema if-schema value))
          (when then-schema
            (check-schema then-schema value))
          (when else-schema
            (check-schema else-schema value))))))


(defmethod check-logical-schema ((logical-schema json-logical-schema) value)
  (let* ((operator (operator logical-schema))
         (schemas (schemas logical-schema))
         (fn (alexandria:switch (operator :test 'equal)
               ("allOf" 'every)
               ("anyOf" 'some)
               ("oneOf" 'just-once)
               ("not"   'notany))))
    (or (funcall fn
                 (lambda (schema)
                   (with-valid-json-p (check-schema schema value)))
                 schemas)
        (raise-invalid-json operator))))


(defun check-logical-schemas (logical-schemas value)
  (dolist (logical-schema logical-schemas)
    (check-logical-schema logical-schema value)))


(defun check-schema-spec (schema-spec value)
  (etypecase schema-spec
    (json-true)
    (json-false
     (raise-invalid-json "false-schema"))
    (json-schema-spec
     ;; The order of these steps doesn't seem to matter, but leave the type
     ;; schema step the last for being able to validate unevaluatedProperties.
     (alexandria:when-let ((logical-schemas (logical-schemas schema-spec)))
       (check-logical-schemas logical-schemas value))
     (alexandria:when-let ((condition-schemas (condition-schemas schema-spec)))
       (check-condition-schemas condition-schemas value))
     (alexandria:when-let ((type-schema (type-schema schema-spec)))
       (check-type-schema type-schema value)))))


(defun check-schema-ref (json-schema value)
  (alexandria:when-let ((ref (ref json-schema)))
    (let* ((base-uri (base-uri json-schema))
           (ref-schema (or
                        ;; $ref might be resolvable on its own
                        (get-schema ref)
                        ;; if not, then resolve against the base URI, if any
                        (when base-uri
                          ;; Get the reference URI by setting $ref as the
                          ;; path in the base URI
                          (let ((ref-uri (puri:copy-uri base-uri)))
                            (setf (puri:uri-path ref-uri) ref)
                            (get-schema (puri:render-uri ref-uri nil)))))))
      (when ref-schema
        (check-schema ref-schema value)))))


(defun check-schema (json-schema value)
  (check-schema-ref json-schema value)
  (check-schema-spec (schema-spec json-schema) value))


;;; Entrypoints

(defmethod validate ((json-schema json-schema) value)
  (let ((*json-path* '("$"))
        (*evaluated-properties-map* (make-hash-table :test 'eq))
        (invalid-json-condition (make-condition 'invalid-json)))
    (handler-bind ((invalid-json
                     (lambda (e)
                       (unless *inside-call-with-valid-json-p*
                         (track-invalid-json-error invalid-json-condition e)
                         (invoke-restart 'continue-validating)))))
      (check-schema json-schema value))
    (when (invalid-json-errors invalid-json-condition)
      (error invalid-json-condition))
    t))
