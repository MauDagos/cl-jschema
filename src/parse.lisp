(in-package :cl-jschema)


;;; Conditions

(define-condition invalid-schema (error) ())


(defun raise-invalid-schema (format-control &rest format-arguments)
  (error 'invalid-schema
         :format-control format-control
         :format-arguments format-arguments))


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
               (setf (gethash key schemas) (make-json-schema value)))
             json-object)
    (when (plusp (hash-table-count schemas))
      schemas)))


(defun parse-array-of-json-schemas (json-array)
  (let ((json-array-length (length json-array)))
    (when (plusp json-array-length)
      (loop
        for item across json-array
        collect (make-json-schema item)))))


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
                 (setf (gethash regex properties) (make-json-schema value))))
             json-object)
    (when (plusp (hash-table-count properties))
      properties)))


(defun parse-keyword-value (keyword value json-object)
  "Validate and parse KEYWORD's VALUE based on it's type.

If successful, also remove the KEYWORD from the JSON-OBJECT."
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
      (remhash keyword json-object))))


(defun make-const-schema (json-object)
  (alexandria:when-let ((const (gethash "const" json-object)))
    (make-instance 'const-schema
                   :const (parse-keyword-value "const" const json-object))))


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
                (raise-invalid-schema "Keywords ~a and ~a refer to different ~
                                       types"
                                      keyword other-keyword))))))))


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
     (make-instance 'json-schema-spec
                    :annotations (make-annotations json)
                    :condition-schemas (make-condition-schemas json)
                    :logical-schemas (make-logical-schemas json)
                    ;; Giving precedence to stricter types of schemas
                    :type-schema (or (make-const-schema json)
                                     (make-enum-schema json)
                                     (make-type-schema json))
                    :metadata (make-metadata json)))))


(defun register-schema-anchor (json-schema)
  "Register the $anchor path to JSON-SCHEMA if there's a base URI ($id).

The path is generated by adding $anchor as the fragment to the base URI. We
assume $anchor is a valid JSON Pointer."
  (let ((base-uri (base-uri json-schema))
        (anchor (anchor json-schema)))
    (when (and base-uri anchor)
      (let ((anchor-uri (puri:copy-uri base-uri)))
        (setf (puri:uri-fragment anchor-uri) anchor)
        (register-schema (puri:render-uri anchor-uri nil) json-schema)))))


(defun register-schema-defs (json-schema)
  "Register the $defs paths of JSON-SCHEMA if there's a base URI ($id).

The paths are generated by removing the URI path from the base URI and and
including in the base URI as fragments '/$defs/<name>', where <name> is a key of
the $defs JSON object. We assume this key is a valid JSON Pointer."
  (let ((base-uri (base-uri json-schema))
        (defs (defs json-schema)))
    (when (and base-uri defs)
      (maphash (lambda (def-name def-json-schema)
                 (let ((def-uri (puri:copy-uri base-uri)))
                   (setf (puri:uri-path def-uri) nil
                         (puri:uri-fragment def-uri) (format nil "/$defs/~a"
                                                             def-name))
                   (register-schema (puri:render-uri def-uri nil)
                                    def-json-schema)))
               defs))))


;;; For keeping track of the value of $id found on the root of the JSON Schema
;;; object.
(defvar *root-id*)


(defun make-json-schema (json)
  (typecase json
    ((or json-boolean hash-table)
     (let* ((rootp (not (boundp '*root-id*)))
            (*root-id* (if rootp nil *root-id*)))
       (multiple-value-bind (schema id anchor ref defs)
           (when (typep json 'hash-table)
             (values (gethash "$schema" json)
                     (gethash "$id" json)
                     (gethash "$anchor" json)
                     (gethash "$ref" json)
                     (gethash "$defs" json)))
         ;; Check root-only values
         (unless rootp
           (when schema
             (raise-invalid-schema "$schema is only allowed at the root level"))
           (when defs
             (raise-invalid-schema "$defs is only allowed at the root level")))
         ;; Parse the JSON Schema
         (when (and id rootp)
           (setq *root-id* id))
         (let ((json-schema
                 (make-instance 'json-schema
                                :schema (when schema
                                          (parse-keyword-value "$schema" schema json))
                                :id (when id
                                      (parse-keyword-value "$id" id json))
                                ;; Parse the root $id after the call to
                                ;; 'PARSE-KEYWORD-VALUE for $id, so we're sure that
                                ;; the root $id is valid
                                :base-uri (when *root-id*
                                            (puri:parse-uri *root-id*))
                                :anchor (when anchor
                                          (parse-keyword-value "$anchor" anchor json))
                                :ref (when ref
                                       (parse-keyword-value "$ref" ref json))
                                :defs (when defs
                                        (parse-keyword-value "$defs" defs json))
                                :schema-spec (make-schema-spec json))))
           ;; Warn if the specified $schema is not supported
           (when (and schema (not (equal schema *$schema*)))
             (warn "No defined support for schema ~s. The schema will be treated as ~
               of draft 2020-12."
                   schema))
           ;; Register schema IDs if there's a base URI ($id)
           (when *root-id*
             ;; Only register the root JSON-SCHEMA with the root id.
             (when rootp
               (register-schema *root-id* json-schema))
             (register-schema-anchor json-schema)
             (register-schema-defs json-schema))
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
   (etypecase input
     ((or json-boolean hash-table)
      input)
     ((or string stream)
      (handler-case
          (jzon:parse input
                      :allow-comments allow-comments
                      :allow-trailing-comma allow-trailing-comma)
        (jzon:json-error (e)
          (raise-unparsable-json e)))))))
