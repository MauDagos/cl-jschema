(in-package :cl-jschema)


(define-condition invalid-schema (error) ())


(defun check-type-value (value)
  (flet ((%check (val)
           (or (type-spec val)
               (error 'invalid-schema
                      :format-control "Type ~s is not allowed"
                      :format-arguments (list val)))))
    (typecase value
      (string (%check value))
      (array (maparray (lambda (item) (%check item))
                       value))
      (t (error 'invalid-schema
                :format-control "The value for \"type\" must be a string or an ~
                                 array")))))


(defun check-keyword-value-type (keyword value)
  "Check if VALUE fulfills KEYWORD's Lisp type. Return T if so."
  (alexandria:when-let ((type (keyword-type keyword)))
    (cond
      ((equal keyword "type")
       (check-type-value value))
      ((typep value type))
      (t
       (error 'invalid-schema
              :format-control (type-error-message type)
              :format-arguments (list keyword))))
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
      (error 'invalid-schema
             :format-control "This regex is not valid: ~a"
             :format-arguments (list string)))))


(defun parse-pattern-properties (json-object)
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
                (error 'invalid-schema
                       :format-control "Keyword ~a is for type ~a, not for ~
                                        type ~a"
                       :format-arguments (list keyword kw-type type))))))
        (dolist (keyword keywords)
          (alexandria:when-let ((kw-type (type-for-keyword keyword)))
            (dolist (other-keyword keywords)
              (unless (or (equal keyword other-keyword)
                          (equal kw-type (type-for-keyword other-keyword)))
                (error 'invalid-schema
                       :format-control "Keywords ~a and ~a refer to different ~
                                        types"
                       :format-arguments (list keyword other-keyword)))))))))


(defun make-type-schema (json-object)
  ;; TODO: can we make checking for enum/const schema nicer?
  (alexandria:when-let ((const-schema (make-const-schema json-object)))
    (return-from make-type-schema const-schema))
  (alexandria:when-let ((enum-schema (make-enum-schema json-object)))
    (return-from make-type-schema enum-schema))
  (let ((type-properties (make-hash-table :test 'equal)))
    (dolist (keyword *type-keywords*)
      (multiple-value-bind (value existsp)
          (gethash keyword json-object)
        (when existsp
          (add-to-type-properites type-properties keyword
                                  (parse-keyword-value keyword value json-object)))))
    (check-colliding-type-keywords type-properties)
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
                    :type-schema (make-type-schema json)
                    :metadata (make-metadata json)))))


;;; For keeping track of the value of $id found on the root of the JSON Schema
;;; object.
(defvar *root-id*)


(defun make-json-schema (json &key rootp)
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
        (error 'invalid-schema
               :format-control "$schema is only allowed at the root level"))
      (when defs
        (error 'invalid-schema
               :format-control "$defs is only allowed at the root level")))
    (when (and id rootp)
      (setq *root-id* id))
    ;; Parse the JSON Schema
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
      ;; Register the json schema in our registry if there's a base URI ($id)
      (when *root-id*
        (register-schema *root-id* json-schema)
        ;; Register the anchor as the base URI with the anchor as the fragment.
        ;; We assume the anchor is a valid JSON Pointer
        (when anchor
          (let ((anchor-uri (puri:copy-uri (base-uri json-schema))))
            (setf (puri:uri-fragment anchor-uri) anchor)
            (register-schema (puri:render-uri anchor-uri nil) json-schema)))
        ;; Register the $defs as base URI without a path and with the "$def
        ;; name" as the fragment. We assume the "$def name" is a valid JSON
        ;; Pointer
        ;;
        ;; NOTE: we're only registering the top-level names in $defs. That means
        ;; we won't support referencing with $refs internal subschemas that are
        ;; deeply nested.
        (when defs
          (maphash (lambda (def-name def-json-schema)
                     (let ((def-uri (puri:copy-uri (base-uri json-schema))))
                       (setf (puri:uri-path def-uri) nil
                             (puri:uri-fragment def-uri) (format nil "/$defs/~a"
                                                                 def-name))
                       (register-schema (puri:render-uri def-uri nil)
                                        def-json-schema)))
                   (defs json-schema))))
      json-schema)))


;;; Entrypoints

(defun parse (input &key allow-comments allow-trailing-comma)
  (let ((json (jzon:parse input
                          :allow-comments allow-comments
                          :allow-trailing-comma allow-trailing-comma)))
    (typecase json
      ((or json-boolean hash-table)
       (let ((*root-id* nil))
         (make-json-schema json :rootp t)))
      (t
       (error 'invalid-schema
              :format-control "The provided JSON schema is invalid")))))
