(asdf:defsystem #:cl-jschema
  :serial t
  :version "0.0.1"
  :description "Support for JSON Schema Draft 2020-12"
  :author "Mauro D'Agostino"
  :license ""                           ; TODO
  :depends-on (#:com.inuoe.jzon
               #:alexandria
               #:cl-ppcre
               #:puri)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "utils")
                             (:file "types")
                             (:file "keywords")
                             (:file "json-schema")
                             (:file "parse")
                             (:file "validate"))))
  :in-order-to ((test-op (test-op "cl-jschema/tests"))))


(asdf:defsystem #:cl-jschema/tests
  :serial t
  :depends-on (#:cl-jschema
               #:fiveam)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "testing-utils")
                             (:file "json-schema-tests"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :cl-jschema.tests)))
