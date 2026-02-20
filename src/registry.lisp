(in-package :cl-jschema)


(defvar *registry* (make-hash-table :test 'equal)
  "Map of URI-reference strings to 'JSON-SCHEMA objects")

;;; Entrypoints

;; TODO make registration optional.
(defun register-schema (id json-schema)
  "Store JSON-SCHEMA in the registry with id ID."
  (setf (gethash id *registry*) json-schema))


(defun unregister-schema (id)
  "Remove the JSON-SCHEMA with the provided ID from the registry."
  (remhash id *registry*))


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
    (multiple-value-bind (value existsp) (gethash uri *registry*)
      (if existsp
          value
          (get-schema (puri:parse-uri uri)))))
  (:method ((uri puri:uri))
    (let ((subschema-id (puri:uri-fragment uri))
          (uri-copy (puri:copy-uri uri)))
      (setf (puri:uri-fragment uri-copy) nil)
      (let* ((id (puri:render-uri uri-copy nil)))
        (multiple-value-bind (json-schema json-schema-exists-p)
            (gethash id *registry*)
          (when json-schema-exists-p
            (if subschema-id
                (get-inner-schema json-schema subschema-id)
                json-schema)))))))
