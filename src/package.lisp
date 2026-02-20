(defpackage #:cl-jschema
  (:use #:cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon)
                    (#:a #:alexandria))
  (:export
   ;; JSON Schema entrypoints
   #:parse
   #:validate
   #:clear-registry
   #:get-schema
   #:register-schema
   #:unregister-schema
   ;; JSON Schema object
   #:json-schema
   ;; Invalid JSON Schema conditions
   #:invalid-schema
   #:invalid-schema-error-message
   #:invalid-schema-base-uri
   #:invalid-schema-json-pointer
   #:unparsable-json
   #:unparsable-json-error
   #:not-implemented
   ;; Validation with JSON Schema conditions
   #:invalid-json
   #:invalid-json-errors
   #:invalid-json-value
   #:invalid-json-value-error-message
   #:invalid-json-value-json-pointer))
