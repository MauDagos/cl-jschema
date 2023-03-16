(defpackage #:cl-jschema
  (:use #:cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export
   ;; JSON Schema entrypoints
   #:parse
   #:validate
   #:clear-registry
   #:get-schema
   ;; JSON Schema object
   #:json-schema
   ;; Invalid JSON Schema conditions
   #:invalid-schema
   #:invalid-schema-error-message
   #:invalid-schema-json-pointer
   #:unparsable-json
   #:unparsable-json-error
   #:not-implemented
   ;; Validation with JSON Schema conditions
   #:invalid-json
   #:invalid-json-error-message
   #:invalid-json-json-pointer
   #:invalid-json-errors))
