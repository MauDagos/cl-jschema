(in-package :cl-jschema)


(defvar *registry* (make-hash-table :test 'equal)
  "Map of URI-references to 'JSON-SCHEMA objects")


(defun register-schema (id json-schema)
  (setf (gethash id *registry*) json-schema))


;;; Entrypoints

(defun clear-registry ()
  (clrhash *registry*))


(defun get-schema (id)
  (gethash id *registry*))
