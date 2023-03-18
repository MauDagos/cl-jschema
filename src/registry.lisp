(in-package :cl-jschema)


(defvar *registry* (make-hash-table :test 'equal)
  "Map of URI-reference strings to 'JSON-SCHEMA objects")


(defun register-schema (id json-schema)
  "Store JSON-SCHEMA in the registry with id ID."
  (setf (gethash id *registry*) json-schema))


;;; Entrypoints

(defun clear-registry ()
  "Clear the registry of parsed JSON Schemas."
  (clrhash *registry*))


(defgeneric get-schema (uri)
  (:documentation "Return NIL or a 'JSON-SCHEMA object.

If URI contains a fragment, then it's either:
  1) A JSON Pointer to an inner schema.
  2) A string which should map to an $anchor.

If a fragment is given and we don't find an inner schema with the fragment, we
return NIL.")
  (:method ((uri string))
    (or (gethash uri *registry*)
        (get-schema (puri:parse-uri uri))))
  (:method ((uri puri:uri))
    (let ((subschema-id (puri:uri-fragment uri))
          (uri-copy (puri:copy-uri uri)))
      (setf (puri:uri-fragment uri-copy) nil)
      (let* ((id (puri:render-uri uri-copy nil))
             (json-schema (gethash id *registry*)))
        (when json-schema
          (if subschema-id
              (get-inner-schema json-schema subschema-id)
              json-schema))))))
