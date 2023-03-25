(in-package :cl-jschema)


;;; JSON array utils

(defun maparray (function array &key (start 0))
  (loop
    for index from start below (length array)
    for item = (aref array index)
    do (funcall function item)))


;;; JSON symbols utils

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun json-null-p (value)
    "Is VALUE a JSON null value as parsed by JZON."
    (eq value 'null)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun json-true-p (value)
    "Is VALUE a JSON true value as parsed by JZON."
    (eq value t)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun json-false-p (value)
    "Is VALUE a JSON false value as parsed by JZON."
    (eq value nil)))


;;; JSON-EQUAL

(defmethod json-equal (json1 json2)
  (equal json1 json2))


(defmethod json-equal ((json1 number) (json2 number))
  ;; Compare numbers with '= so floats with digits of 0 are considered equal to
  ;; integers. E.g. (= 0 0.0)
  (= json1 json2))


(defmethod json-equal ((json1 hash-table) (json2 hash-table))
  (and (= (hash-table-count json1)
          (hash-table-count json2))
       (loop
         for key1 being the hash-key in json1
           using (hash-value value1)
         for value2 = (gethash key1 json2)
         always (json-equal value1 value2))))


(defmethod json-equal ((json1 array) (json2 array))
  (and (= (length json1)
          (length json2))
       (loop
         for item1 across json1
         for item2 across json2
         always (json-equal item1 item2))))


;;; Validating with a JSON Schema

(defun just-once (predicate &rest sequences)
  "Return T if PREDICATE is non-NIL only once in SEQUENCES."
  (let ((count 0))
    (apply 'mapc
           (lambda (&rest inputs)
             (when (apply predicate inputs)
               (incf count)
               (when (> count 1)
                 (return-from just-once nil))))
           sequences)
    (= count 1)))


(defun some-checking-all (predicate &rest sequences)
  "Return T if PREDICATE is non-NIL at least once in SEQUENCES, but without short
circuiting."
  (let (validp)
    (apply 'mapc
           (lambda (&rest inputs)
             (when (apply predicate inputs)
               (setq validp t)))
           sequences)
    validp))


;;; Keeping track of a JSON Pointer

(defvar *tracked-json-pointer* nil)


(defun call-with-tracked-json-pointer (key-or-index body-fn)
  (let ((*tracked-json-pointer* (cons key-or-index *tracked-json-pointer*)))
    (funcall body-fn)))


(defmacro with-tracked-json-pointer (key-or-index &body body)
  `(call-with-tracked-json-pointer ,key-or-index (lambda () ,@body)))


(defun tracked-json-pointer ()
  (format nil "~{/~a~^~}" (reverse *tracked-json-pointer*)))


;;; JSON Pointer escaping

(defparameter *json-pointer-escape-map*
  '(("~" . "~0")
    ("/" . "~1")
    ("%" . "%25"))
  "Map of characters to their respective escaped versions according to the JSON
Pointer RFC.")


(defun unescape-json-pointer (json-pointer)
  "Return the unescaped JSON Pointer of JSON-POINTER."
  (loop
    with unescaped-json-pointer = json-pointer
    for (unescaped . escaped) in *json-pointer-escape-map*
    do (setq unescaped-json-pointer
             (cl-ppcre:regex-replace-all escaped
                                         unescaped-json-pointer
                                         unescaped))
    finally (return unescaped-json-pointer)))
