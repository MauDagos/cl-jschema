(in-package :cl-jschema)


;;; For keeping track of the base URI of the JSON Schema
(defvar *base-uri*)


;;; Conditions

(define-condition invalid-schema (error)
  ((error-message :initarg :error-message
                  :reader invalid-schema-error-message)
   (base-uri     :initarg :base-uri
                 :reader invalid-schema-base-uri)
   (json-pointer :initarg :json-pointer
                 :reader invalid-schema-json-pointer))
  (:report (lambda (condition stream)
             (let ((base-uri (invalid-schema-base-uri condition))
                   (json-pointer (invalid-schema-json-pointer condition)))
               (format stream "~s : ~a"
                       (if base-uri
                           (let ((uri-copy (puri:copy-uri base-uri)))
                             (setf (puri:uri-fragment uri-copy) json-pointer)
                             (puri:render-uri uri-copy nil))
                           json-pointer)
                       (invalid-schema-error-message condition))))))


(defun %raise-invalid-schema (condition-type format-control format-arguments)
  (error condition-type
         :error-message (format nil "~?" format-control format-arguments)
         :base-uri (when (boundp '*base-uri*) *base-uri*)
         :json-pointer (tracked-json-pointer)))


(defun raise-invalid-schema (format-control &rest format-arguments)
  (%raise-invalid-schema 'invalid-schema format-control format-arguments))


(define-condition not-implemented (invalid-schema) ())


(defun raise-not-implemented (format-control &rest format-arguments)
  (%raise-invalid-schema 'not-implemented format-control format-arguments))


(define-condition unparsable-json (invalid-schema)
  ((json-error :initarg :json-error
               :reader unparsable-json-error)))


(defun raise-unparsable-json (json-error)
  (error 'unparsable-json
         :format-control "Unable to parse JSON Schema due to unparsable JSON: ~a"
         :format-arguments (list json-error)
         :json-error json-error))


;;; JSON Schema type checking

(defun check-type-value (value)
  "Check if the value for 'type' is allowed."
  (flet ((%check (val)
           (or (type-spec val)
               (raise-invalid-schema "Type ~s is not allowed" val))))
    (typecase value
      (string (%check value))
      (array (maparray (lambda (item) (%check item))
                       value))
      (t (raise-invalid-schema "The value for \"type\" must be a string or an ~
                                array")))))


(defun check-keyword-value-type (keyword value)
  "Check if VALUE fulfills KEYWORD's Lisp type. Return T if so."
  (alexandria:when-let ((type (keyword-type keyword)))
    (cond
      ((equal keyword "type")
       (check-type-value value))
      ((typep value type))
      (t
       (raise-invalid-schema (type-error-format-string type) keyword)))
    t))


;;; Parsing JSON Schema based on expected Lisp type

(defun parse-object-of-arrays (json-object)
  (let ((table (make-hash-table :test 'equal)))
    (maphash (lambda (key value)
               (setf (gethash key table) (coerce value 'list)))
             json-object)
    (when (plusp (hash-table-count table))
      table)))


(defun parse-object-of-json-schemas (json-object)
  (let ((schemas (make-hash-table :test 'equal))) ; Case sensitive
    (maphash (lambda (key value)
               (setf (gethash key schemas)
                     (with-tracked-json-pointer key
                       (make-json-schema value))))
             json-object)
    (when (plusp (hash-table-count schemas))
      schemas)))


(defun parse-array-of-json-schemas (json-array)
  (let ((json-array-length (length json-array)))
    (when (plusp json-array-length)
      (loop
        for index from 0
        for item across json-array
        collect (with-tracked-json-pointer index
                  (make-json-schema item))))))


(defun parse-regex (string)
  (handler-case
      (make-instance 'regex-box
                     :regex-string string
                     :regex (ppcre:create-scanner string))
    (ppcre:ppcre-syntax-error ()
      (raise-invalid-schema "This regex is not valid: ~a" string))))


(defun parse-pattern-properties (json-object)
  "Parse 'patternProperties'. In this case, the keywords are regexs and the
values of JSON Schemas."
  (let ((properties (make-hash-table :test 'eq)))
    (maphash (lambda (key value)
               (let ((regex (parse-regex key)))
                 (setf (gethash regex properties)
                       (with-tracked-json-pointer key
                         (make-json-schema value)))))
             json-object)
    (when (plusp (hash-table-count properties))
      properties)))


(defun parse-keyword-value (keyword value json-object)
  "Validate and parse KEYWORD's VALUE based on it's type.

If successful, also remove the KEYWORD from the JSON-OBJECT."
  (with-tracked-json-pointer keyword
    (when (check-keyword-value-type keyword value)
      (prog1
          (case (keyword-type keyword)
            ;; JSON arrays
            ((array
              non-empty-array
              array-of-strings)
             (coerce value 'list))
            (non-empty-array-of-schema-likes
             (parse-array-of-json-schemas value))
            ;; JSON objects
            (hash-table-of-array-of-strings
             (parse-object-of-arrays value))
            (hash-table-of-schema-likes
             ;; Special case for patternProperties
             (if (equal keyword "patternProperties")
                 (parse-pattern-properties value)
                 (parse-object-of-json-schemas value)))
            ;; Schema
            (schema-like
             (make-json-schema value))
            ;; Others
            (string-or-array-of-strings
             (etypecase value
               (string value)
               (array (coerce value 'list))))
            (regex
             (parse-regex value))
            (t value))
        (remhash keyword json-object)))))


;;; Creating Lisp instances

(defun make-const-schema (json-object)
  (multiple-value-bind (const existsp)
      (gethash "const" json-object)
    (when existsp
      (make-instance 'const-schema
                     :const (parse-keyword-value "const" const json-object)))))


(defun make-enum-schema (json-object)
  (alexandria:when-let ((enum (gethash "enum" json-object)))
    (make-instance 'enum-schema
                   :items (parse-keyword-value "enum" enum json-object))))


(defun check-colliding-type-keywords (type-properties)
  "Check if any of the keywords in TYPE-PROPERTIES refer to different types of
JSON values."
  (let* ((props (alexandria:hash-table-values type-properties))
         (type-prop (find "type" props :key 'key :test 'equal))
         (type (when type-prop (value type-prop)))
         (keywords (mapcar 'key
                           (if type-prop (remove type-prop props) props))))
    (if type
        (dolist (keyword keywords)
          (unless (equal keyword "type")
            (let ((kw-type (type-for-keyword keyword)))
              (when (and kw-type
                         (not (equal type kw-type)))
                (raise-invalid-schema "Keyword ~a is for type ~a, not for type ~a"
                                      keyword kw-type type)))))
        (dolist (keyword keywords)
          (alexandria:when-let ((kw-type (type-for-keyword keyword)))
            (dolist (other-keyword keywords)
              (unless (or (equal keyword other-keyword)
                          (equal kw-type (type-for-keyword other-keyword)))
                ;; Sort the keywords for easier testing
                (raise-invalid-schema "~?"
                                      "Keywords ~a and ~a refer to different ~
                                       types"
                                      (sort (list keyword other-keyword)
                                            'string<)))))))))


(defun make-type-schema (json-object)
  (let ((type-properties (make-hash-table :test 'equal)))
    ;; Populate the type-properties
    (dolist (keyword *type-keywords*)
      (multiple-value-bind (value existsp)
          (gethash keyword json-object)
        (when existsp
          (add-to-type-properites keyword
                                  (parse-keyword-value keyword value json-object)
                                  type-properties))))
    ;; Validate
    (check-colliding-type-keywords type-properties)
    ;; Return the correct instance
    (let* ((type-prop (gethash :|type| type-properties))
           (type (or (when type-prop (value type-prop))
                     ;; If 'type' was unspecified, then infer it from other
                     ;; keywords
                     (loop
                       for keyword-prop being the hash-values in type-properties
                       thereis (type-for-keyword (key keyword-prop))))))
      (when type
        (make-instance (alexandria:switch (type :test 'equal)
                         ("object" 'json-object-schema)
                         ("array"  'json-array-schema)
                         (t        'json-basic-type-schema))
                       :type-properties type-properties)))))


(defun make-annotations (json-object)
  (let ((annotations (make-hash-table :test 'equal)))
    (dolist (keyword *annotation-keywords*)
      (alexandria:when-let ((value (gethash keyword json-object)))
        (setf (gethash keyword annotations)
              (parse-keyword-value keyword value json-object))))
    (when (plusp (hash-table-count annotations))
      annotations)))


(defun make-condition-schemas (json-object)
  (let ((conditions (make-hash-table :test 'equal)))
    (dolist (keyword *conditional-keywords*)
      (alexandria:when-let ((value (gethash keyword json-object)))
        (setf (gethash keyword conditions)
              (parse-keyword-value keyword value json-object))))
    (when (plusp (hash-table-count conditions))
      conditions)))


(defun make-logical-schemas (json-object)
  (loop
    for keyword in *logical-keywords*
    for value = (gethash keyword json-object)
    when value
      collect (make-instance 'json-logical-schema
                             :operator keyword
                             ;; 'not' expects a JSON schema. Let's ensure we
                             ;; use a list here for easier validation.
                             :schemas (alexandria:ensure-list
                                       (parse-keyword-value keyword value
                                                            json-object)))))


(defun make-metadata (json-object)
  ;; Just return whatever is remaining
  (when (plusp (hash-table-count json-object))
    json-object))


(defun make-schema-spec (json)
  (etypecase json
    (json-boolean
     json)
    (hash-table
     (if (plusp (hash-table-count json))
         (make-instance 'json-schema-spec
                        :annotations (make-annotations json)
                        :condition-schemas (make-condition-schemas json)
                        :logical-schemas (make-logical-schemas json)
                        ;; Giving precedence to stricter types of schemas
                        :type-schema (or (make-const-schema json)
                                         (make-enum-schema json)
                                         (make-type-schema json))
                        :metadata (make-metadata json))
         ;; A JSON Schema like '{}' is the same as 'true'
         t))))


(defun check-ref-to-ref (json-schema)
  "Check if JSON-SCHEMA refers to another JSON-SCHEMA using $ref.

This is explicitly disallowed, according to the official JSON Schema
documentation."
  (let ((ref-schema (get-schema-from-ref json-schema)))
    (when (and ref-schema (ref ref-schema))
      (raise-invalid-schema "$ref ~s refers to another JSON Schema using $ref"
                            (ref json-schema)))))


(defun register-schema-resource (json-schema)
  "Register JSON-SCHEMA as a Schema Resource by resolving $id against the base
URI."
  (let ((base-uri (base-uri json-schema))
        (id (id json-schema)))
    (when base-uri
      (let ((uri-copy (puri:copy-uri base-uri)))
        (setf (puri:uri-path uri-copy) id)
        (register-schema (puri:render-uri uri-copy nil) json-schema)))))


(defun register-self (json-schema)
  "Register the current JSON Pointer in the self registry.

Additionally register $anchor or $defs."
  (let ((self-registry (self-registry json-schema))
        (json-pointer (tracked-json-pointer))
        (anchor (anchor json-schema))
        (defs (defs json-schema)))
    ;; Register this
    (setf (gethash json-pointer self-registry) json-schema)
    ;; Register $anchor
    (when anchor
      (setf (gethash anchor self-registry) json-schema))
    ;; Register $defs
    (when defs
      (maphash (lambda (def-name def-json-schema)
                 (setf (gethash (format nil "~a/$defs/~a"
                                        json-pointer def-name)
                                self-registry)
                       def-json-schema))
               defs))))


;;; For keeping track of all JSON Schemas seen while parsing the root JSON
;;; Schema
(defvar *self-registry*)

(defun make-json-schema (json)
  (typecase json
    ((or json-boolean hash-table)
     (multiple-value-bind (schema id anchor ref defs)
         (when (typep json 'hash-table)
           (values (gethash "$schema" json)
                   (gethash "$id" json)
                   (gethash "$anchor" json)
                   (gethash "$ref" json)
                   (gethash "$defs" json)))
       (let* ( ; Track if we're at the root of the JSON Schema document
              (document-root-p (not (boundp '*base-uri*)))
              ;; We're at the root of a new JSON Schema if we've just started
              ;; parsing or if we've run into a new $id
              (schema-root-p (or document-root-p
                                 id))
              ;; Parse $id early for the base URI's sake
              (parsed-id (when id
                           (parse-keyword-value "$id" id json)))
              ;; The base URI can only be the $id at the root of the JSON Schema
              ;; document.
              (*base-uri* (if document-root-p
                              (when parsed-id
                                (puri:parse-uri parsed-id))
                              *base-uri*))
              ;; Initialize the self registry whenever we reach a new JSON
              ;; Schema
              (*self-registry* (if schema-root-p
                                   (make-hash-table :test 'equal)
                                   *self-registry*))
              ;; Reset the tracked JSON Pointer whenever we reach a new JSON
              ;; Schema
              (*tracked-json-pointer* (if schema-root-p
                                          nil
                                          *tracked-json-pointer*)))
         ;; Check schema root-only values
         (unless schema-root-p
           (when schema
             (raise-invalid-schema "$schema is only allowed at the root level"))
           (when defs
             (raise-invalid-schema "$defs is only allowed at the root level")))
         ;; Parse the JSON Schema
         (let ((json-schema
                 (make-instance 'json-schema
                                :base-uri *base-uri*
                                :schema (when schema
                                          (parse-keyword-value "$schema" schema json))
                                :id parsed-id
                                :anchor (when anchor
                                          (parse-keyword-value "$anchor" anchor json))
                                :ref (when ref
                                       (parse-keyword-value "$ref" ref json))
                                :defs (when defs
                                        (parse-keyword-value "$defs" defs json))
                                :schema-spec (make-schema-spec json)
                                :self-registry *self-registry*)))
           ;; Check if $ref refers to another $ref
           (check-ref-to-ref json-schema)
           ;; Check if the specified $schema is not supported
           (when (and schema (not (equal schema *$schema*)))
             (raise-not-implemented "No support for $schema ~s" schema))
           ;; Only register the root JSON-SCHEMA with the root $id. If the
           ;; schema is not the one at the root of the JSON Schema document,
           ;; then $id has to resolve against the base URI.
           (when schema-root-p
             (if document-root-p
                 (when id
                   (register-schema id json-schema))
                 (register-schema-resource json-schema)))
           ;; Register in the self registry
           (register-self json-schema)
           ;; Return the new JSON-SCHEMA instance
           json-schema))))
    (t
     (raise-invalid-schema "JSON Schema must be a JSON boolean or object"))))


;;; Entrypoints

(defun parse (input &key allow-comments allow-trailing-comma)
  "Return an instance of 'JSON-SCHEMA.

INPUT can be a value previously parsed by JZON or a value that's parsable by
JZON. The keyargs ALLOW-COMMENTS and ALLOW-TRAILING-COMMA are forwarded to
'JZON:PARSE."
  (make-json-schema
   (typecase input
     (schema-like
      input)
     ((or string stream)
      (handler-case
          (jzon:parse input
                      :allow-comments allow-comments
                      :allow-trailing-comma allow-trailing-comma)
        (jzon:json-error (e)
          (raise-unparsable-json e))))
     (t
      (error "CL-JSCHEMA:PARSE accepts as input NIL, T, HASH-TABLE, STRING or ~
              STREAM")))))
