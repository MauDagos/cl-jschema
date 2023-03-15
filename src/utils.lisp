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


;;; Misc

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
