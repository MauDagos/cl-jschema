(defpackage #:cl-jschema
  (:use #:cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  ;; JSON Schema entrypoints
  (:export #:parse
           #:validate
           #:clear-registry
           #:get-schema
           ;; JSON Schema object
           #:json-schema
           ;; Conditions
           #:invalid-schema
           #:not-implemented
           #:invalid-json
           #:invalid-json-error-message
           #:invalid-json-pointer
           #:invalid-json-errors))
