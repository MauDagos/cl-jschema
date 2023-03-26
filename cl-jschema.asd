(asdf:defsystem #:cl-jschema
  :serial t
  :version "1.1.0"
  :description "Common Lisp implementation of JSON Schema"
  :author "Mauro D'Agostino"
  :license "MIT"
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
                             (:file "registry")
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
                             (:file "tests"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :cl-jschema.tests)))
