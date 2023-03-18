(in-package :cl-jschema)


;;; Handling conditions

(define-condition invalid-json-value (error)
  ((error-message :initarg :error-message
                  :reader invalid-json-value-error-message)
   (json-pointer :initarg :json-pointer
                 :reader invalid-json-value-json-pointer))
  (:report (lambda (condition stream)
             (format stream "~s : ~a"
                     (invalid-json-value-json-pointer condition)
                     (invalid-json-value-error-message condition)))))


(defun raise-invalid-json-value (keyword &optional value)
  "Raise an 'INVALID-JSON-VALUE condition.

Also create an restart named 'CONTINUE-VALIDATING."
  (let ((format-message (keyword-validation-format-string
                         ;; Keywords from 'CHECK-TYPE-PROPERTY are arriving as
                         ;; Lisp keywords, so convert them back to strings.
                         (string keyword))))
    (with-simple-restart (continue-validating "Continue validating the JSON")
      (error 'invalid-json-value
             :error-message (format nil format-message value)
             :json-pointer (tracked-json-pointer)))))


(define-condition unresolvable-ref (invalid-json-value) ())


(defun raise-unresolvable-ref (ref)
  "Raise an 'UNRESOLVABLE-REF condition.

Also create an restart named 'CONTINUE-VALIDATING."
  (with-simple-restart (continue-validating "Continue validating the JSON")
    (error 'unresolvable-ref
           :error-message (format nil (keyword-validation-format-string
                                       "unresolvable-ref")
                                  ref)
           :json-pointer (tracked-json-pointer))))


(define-condition invalid-json (error)
  ((errors :initform nil
           :accessor invalid-json-errors))
  (:report (lambda (condition stream)
             (format stream "JSON Schema validation found these errors:~
                             ~{~2%~a~^~}"
                     (sort (copy-list (invalid-json-errors condition))
                           'string<
                           :key 'invalid-json-value-json-pointer)))))


(defmethod track-invalid-json-error ((self invalid-json) error)
  (push error (invalid-json-errors self)))


;;; Validation helpers

(defvar *inside-call-with-valid-json-p* nil
  "Helper variable for knowing if we're inside a usage of WITH-VALID-JSON-P.")


(defun call-with-valid-json-p (body-fn)
  (let ((*inside-call-with-valid-json-p* t))
    (handler-bind ((invalid-json-value
                     (lambda (e)
                       (declare (ignore e))
                       (return-from call-with-valid-json-p nil))))
      (funcall body-fn)
      t)))


(defmacro with-valid-json-p (&body body)
  "Make BODY return T if no errors were triggered or return NIL if a condition of
type 'INVALID-JSON-VALUE was triggered.

Any 'INVALID-JSON-VALUE thrown by BODY is not propagated upwards."
  `(call-with-valid-json-p (lambda () ,@body)))


;;; Keeping track of validated properties in a JSON object, for being able to
;;; validate 'unevaluatedProperties'.
;;;
;;; Use a hash-table for mapping JSON objects to CONS cells whose CAR is the
;;; list of evaluated properties and CDR is the list of evaluated invalid
;;; properties.
(defvar *evaluated-properties-map*)


(defun call-with-tracked-property (json-object keyword body-fn)
  (let ((properties (alexandria:ensure-gethash json-object
                                               *evaluated-properties-map*
                                               (cons nil nil))))
    ;; Track the property being evaluated for this json object
    (pushnew keyword (car properties) :test 'equal)
    (handler-bind ((invalid-json-value
                     (lambda (e)
                       (declare (ignore e))
                       ;; Track the property as being invalid
                       (pushnew keyword (cdr properties)
                                :test 'equal))))
      (funcall body-fn))))


(defmacro with-tracked-property (json-object keyword &body body)
  "Track KEYWORD as having been evaluated for JSON-OBJECT. Also tracks if
KEYWORD is somehow invalid."
  `(call-with-tracked-property ,json-object ,keyword (lambda () ,@body)))


;;; Validation

(defmethod check-type-property ((keyword (eql :|type|))
                                type value)
  "Check VALUE by 'type' for TYPE."
  (flet ((%check (type)
           (let ((spec (type-spec type)))
             (or (typep value (type-spec-lisp-type spec))
                 (raise-invalid-json-value keyword type)))))
    (etypecase type
      (string
       (%check type))
      (list
       (or (some (lambda (type*)
                   (with-valid-json-p (%check type*)))
                 type)
           (raise-invalid-json-value "types" type))))))


(defmethod check-type-property ((keyword (eql :|minLength|))
                                min-length value)
  "Check VALUE by 'minLength' for MIN-LENGTH."
  (when (stringp value)
    (or (>= (length value) min-length)
        (raise-invalid-json-value keyword min-length))))


(defmethod check-type-property ((keyword (eql :|maxLength|))
                                max-length value)
  "Check VALUE by 'maxLength' for MAX-LENGTH."
  (when (stringp value)
    (or (<= (length value) max-length)
        (raise-invalid-json-value keyword max-length))))


(defmethod check-type-property ((keyword (eql :|pattern|))
                                pattern value)
  "Check VALUE by 'pattern' for PATTERN."
  (when (stringp value)
    (or (ppcre:scan (regex pattern) value)
        (raise-invalid-json-value keyword (regex-string pattern)))))


(defmethod check-type-property ((keyword (eql :|multipleOf|))
                                multiple-of value)
  "Check VALUE by 'multipleOf' for MULTIPLE-OF."
  (when (numberp value)
    (or (zerop (mod value multiple-of))
        (raise-invalid-json-value keyword multiple-of))))


(defmethod check-type-property ((keyword (eql :|minimum|))
                                minimum value)
  "Check VALUE by 'minimum' by MINIMUM."
  (when (numberp value)
    (or (>= value minimum)
        (raise-invalid-json-value keyword minimum))))


(defmethod check-type-property ((keyword (eql :|exclusiveMinimum|))
                                exclusive-minimum value)
  "Check VALUE by 'exclusiveMinimum' by EXCLUSIVE-MINIMUM."
  (when (numberp value)
    (or (> value exclusive-minimum)
        (raise-invalid-json-value keyword exclusive-minimum))))


(defmethod check-type-property ((keyword (eql :|maximum|))
                                maximum value)
  "Check VALUE by 'maximum' by MAXIMUM."
  (when (numberp value)
    (or (<= value maximum)
        (raise-invalid-json-value keyword maximum))))


(defmethod check-type-property ((keyword (eql :|exclusiveMaximum|))
                                exclusive-maximum value)
  "Check VALUE by 'exclusiveMaximum' by EXCLUSIVE-MAXIMUM."
  (when (numberp value)
    (or (< value exclusive-maximum)
        (raise-invalid-json-value keyword exclusive-maximum))))


(defmethod check-type-property ((keyword (eql :|properties|))
                                properties value)
  "Check VALUE by 'properties' for JSON Schemas in PROPERTIES."
  (when (typep value 'hash-table)
    (maphash (lambda (key json-schema)
               (alexandria:when-let ((object-value (gethash key value)))
                 (with-tracked-json-pointer key
                   (with-tracked-property value key
                     (check-schema json-schema object-value)))))
             properties)))


(defmethod check-type-property ((keyword (eql :|propertyNames|))
                                property-names value)
  "Check VALUE by 'propertyNames' for JSON Schemas in PROPERTY-NAMES."
  (when (typep value 'hash-table)
    (maphash (lambda (object-key object-value)
               (declare (ignore object-value))
               (with-tracked-json-pointer object-key
                 (with-tracked-property value object-key
                   (check-schema property-names object-key))))
             value)))


(defmethod check-type-property ((keyword (eql :|patternProperties|))
                                pattern-properties value)
  "Check VALUE by 'patternProperties' for regexs and JSON Schemas in
PATTERN-PROPERTIES."
  (when (typep value 'hash-table)
    (maphash (lambda (regex-box schema)
               (maphash (lambda (object-key object-value)
                          (when (ppcre:scan (regex regex-box) object-key)
                            (with-tracked-json-pointer object-key
                              (with-tracked-property value object-key
                                (check-schema schema object-value)))))
                        value))
             pattern-properties)))


(defmethod check-type-property ((keyword (eql :|required|))
                                required value)
  "Check VALUE by 'required' for keys in REQUIRED."
  (when (and (typep value 'hash-table) required)
    (loop
      for field in required
      always (or (gethash field value)
                 (raise-invalid-json-value keyword field)))))


(defmethod check-type-property ((keyword (eql :|dependentRequired|))
                                dependent-required value)
  "Check VALUE by 'dependentRequired' for DEPENDENT-REQUIRED."
  (when (typep value 'hash-table)
    (maphash (lambda (dependent required)
               (when (gethash dependent value)
                 (check-type-property :|required| required value)))
             dependent-required)))


(defmethod check-type-property ((keyword (eql :|dependentSchemas|))
                                dependent-schemas value)
  "Check VALUE by 'dependentSchemas' for JSON Schemas in DEPENDENT-SCHEMAS."
  (when (typep value 'hash-table)
    (maphash (lambda (dependent schema)
               (when (gethash dependent value)
                 (check-schema schema value)))
             dependent-schemas)))


(defmethod check-type-property ((keyword (eql :|minProperties|))
                                min-properties value)
  "Check VALUE by 'minProperties' for MIN-PROPERTIES."
  (when (typep value 'hash-table)
    (or (>= (hash-table-count value) min-properties)
        (raise-invalid-json-value keyword min-properties))))


(defmethod check-type-property ((keyword (eql :|maxProperties|))
                                max-properties value)
  "Check VALUE by 'maxProperties' for MAX-PROPERTIES."
  (when (typep value 'hash-table)
    (or (<= (hash-table-count value) max-properties)
        (raise-invalid-json-value keyword max-properties))))


(defmethod check-type-property ((keyword (eql :|prefixItems|))
                                prefix-items value)
  "Check VALUE by 'prefixItems' for JSON Schemas in PREFIX-ITEMS."
  (when (typep value 'array)
    (loop
      for item across value
      for index from 0
      for json-schema in prefix-items
      do (with-tracked-json-pointer index
           (check-schema json-schema item)))))


(defmethod check-type-property ((keyword (eql :|minItems|))
                                min-items value)
  "Check VALUE by 'minItems' for MIN-ITEMS."
  (when (typep value 'array)
    (or (>= (length value) min-items)
        (raise-invalid-json-value keyword min-items))))


(defmethod check-type-property ((keyword (eql :|maxItems|))
                                max-items value)
  "Check VALUE by 'maxItems' for MAX-ITEMS."
  (when (typep value 'array)
    (or (<= (length value) max-items)
        (raise-invalid-json-value keyword max-items))))


(defmethod check-type-property ((keyword (eql :|uniqueItems|))
                                unique-items value)
  "Check VALUE by 'uniqueItems', if UNIQUE-ITEMS is 'true'"
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
        return (raise-invalid-json-value keyword))))


(defun check-additional-properties (object-schema json-object)
  "Check if JSON-OBJECT fulfills the 'additionalProperties' JSON Schema in
OBJECT-SCHEMA."
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
                        (raise-invalid-json-value "additionalProperties" key)))
                  json-object)))
      ;; Schema: only allow additional if they're valid.
      (json-schema
       (let ((available-props (alexandria:hash-table-keys properties)))
         (maphash (lambda (key val)
                    (or (member key available-props :test 'equal)
                        (with-tracked-json-pointer key
                          (with-tracked-property json-object key
                            (check-schema additional-properties val)))))
                  json-object))))))


(defun check-unevaluated-properties (object-schema json-object)
  "Check if JSON-OBJECT fulfills the 'unevaluatedProperties' JSON Schema in
OBJECT-SCHEMA."
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
                      (raise-invalid-json-value "additionalProperties" key)))
                json-object))
      ;; Schema: only allow unevaluated if they're valid.
      (json-schema
       (maphash (lambda (key val)
                  (or (member key validated-properties :test 'equal)
                      (with-tracked-json-pointer key
                        (check-schema unevaluated-properties val))))
                json-object)))))


(defun check-items (array-schema json-array)
  "Check if JSON-ARRAY fulfills the 'items' JSON Schema in ARRAY-SCHEMA.

If 'prefixItems' was specified, then we only check items not covered by it."
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
         (raise-invalid-json-value "items")))
      ;; Schema has two cases:
      ;; If prefixItems was set: check that additional items are valid.
      ;; Else: check that all items are valid
      (json-schema
       (let* ((start (if prefix-items
                         (length prefix-items)
                         0))
              (index start))
         (maparray (lambda (item)
                     (with-tracked-json-pointer index
                       (check-schema items item))
                     (incf index))
                   json-array
                   :start start))))))


(defun check-contains (array-schema json-array)
  "Check if JSON-ARRAY fulfills the 'contains' JSON Schema in ARRAY-SCHEMA.

The values of 'minContains' and 'maxContains' are also respected."
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
                      (raise-invalid-json-value "maxContains" max-contains))))
                json-array)
      (when (< valid-count min-contains)
        (raise-invalid-json-value "minContains" min-contains)))))


(defmethod check-type-schema ((const-schema const-schema) value)
  "Check if VALUE is the item from CONST-SCHEMA."
  (or (json-equal value (const const-schema))
      (raise-invalid-json-value "const")))


(defmethod check-type-schema ((enum-schema enum-schema) value)
  "Check if VALUE is any item in ENUM-SCHEMA."
  (or (member value (items enum-schema) :test 'json-equal)
      (raise-invalid-json-value "enum")))


(defmethod check-type-schema ((type-schema json-type-schema) value)
  "Check if VALUE fulfills TYPE-SCHEMA by validating VALUE against all of the
TYPE-SCHEMA properties."
  (alexandria:when-let ((type-properties (type-properties type-schema)))
    (maphash (lambda (prop-keyword schema-prop)
               (check-type-property prop-keyword
                                    (value schema-prop)
                                    value))
             type-properties)))


(defmethod check-type-schema :after ((object-schema json-object-schema) value)
  "Check if VALUE fulfills the additional object-type only validations."
  (check-additional-properties object-schema value)
  (check-unevaluated-properties object-schema value))


(defmethod check-type-schema :after ((array-schema json-array-schema) value)
  "Check if VALUE fulfills the additional array-type only validations."
  (check-items array-schema value)
  (check-contains array-schema value))


(defun check-condition-schemas (condition-schemas value)
  "Check if VALUE fulfills the conditional JSON Schemas, if there's an 'if'
JSON Schema."
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
  "Check if VALUE fulfills LOGICAL-SCHEMA."
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
        (raise-invalid-json-value operator))))


(defun check-logical-schemas (logical-schemas value)
  (dolist (logical-schema logical-schemas)
    (check-logical-schema logical-schema value)))


(defun check-schema-spec (schema-spec value)
  "Check if VALUE fulfills the actual JSON Schema spec."
  (etypecase schema-spec
    (json-true)
    (json-false
     (raise-invalid-json-value "false-schema"))
    (json-schema-spec
     ;; The order of these steps doesn't seem to matter, but leave the type
     ;; schema step the last for being able to validate 'unevaluatedProperties'.
     (alexandria:when-let ((logical-schemas (logical-schemas schema-spec)))
       (check-logical-schemas logical-schemas value))
     (alexandria:when-let ((condition-schemas (condition-schemas schema-spec)))
       (check-condition-schemas condition-schemas value))
     (alexandria:when-let ((type-schema (type-schema schema-spec)))
       (check-type-schema type-schema value)))))


;;; Bound by 'VALIDATE. See docstring there.
(defvar *ignore-unresolvable-refs*)

(defun check-schema-ref (json-schema value)
  "Check if VALUE fulfills the JSON Schema referenced by $ref, if any."
  (alexandria:when-let ((ref (ref json-schema)))
    (let ((ref-schema (get-schema-from-ref json-schema))
          (current-json-pointer *tracked-json-pointer*)
          ref-error-raised-p)
      (cond
        (ref-schema
         ;; If the value doesn't satisfy the schema found, also raise an error
         ;; to be clear that it happened while checking $ref.
         ;;
         ;; Since the restart is being invoked by 'VALIDATE, we must ensure here
         ;; that the $ref error is raised only once.
         (handler-bind ((invalid-json-value
                          (lambda (e)
                            (declare (ignore e))
                            (unless ref-error-raised-p
                              (let ((*tracked-json-pointer* current-json-pointer))
                                (raise-invalid-json-value "$ref" ref)
                                (setq ref-error-raised-p t))))))
           (check-schema ref-schema value)))
        ((not *ignore-unresolvable-refs*)
         (raise-unresolvable-ref ref))))))


(defun check-schema (json-schema value)
  "Check if VALUE fulfills the JSON Schema."
  ;; NOTE: the spec on $ref doesn't say that it disallows validating with other
  ;; keywords, so consider it possible
  (check-schema-ref json-schema value)
  (check-schema-spec (schema-spec json-schema) value))


;;; Entrypoints

(defmethod validate ((json-schema json-schema) value
                     &key ignore-unresolvable-refs)
  "Validate VALUE with JSON-SCHEMA.

Return T if valid or throw an 'INVALID-JSON condition if not.

If IGNORE-UNRESOLVABLE-REFS is NIL, then when we don't find a JSON Schema with
$ref, the value being validated will also be considered invalid. Set the keyarg
to non-NIL to avoid these errors."
  (let ((*ignore-unresolvable-refs* ignore-unresolvable-refs)
        (*evaluated-properties-map* (make-hash-table :test 'eq))
        (invalid-json-condition (make-condition 'invalid-json)))
    (handler-bind ((invalid-json-value
                     (lambda (e)
                       (track-invalid-json-error invalid-json-condition e)
                       (invoke-restart 'continue-validating))))
      (check-schema json-schema value))
    (when (invalid-json-errors invalid-json-condition)
      (error invalid-json-condition))
    t))
