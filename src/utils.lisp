(in-package :cl-jschema)


;;; Hash table utils

(defun hash-table-of-type-p (hash-table type)
  (loop
    for value being the hash-value in hash-table
    always (typep value type)))


;;; Array utils

(defun array-of-type-p (array type)
  (loop
    for item across array
    always (typep item type)))


(defun maparray (function array &key (start 0))
  (loop
    for index from start below (length array)
    for item = (aref array index)
    do (funcall function item)))


;;; JSON symbols utils

(defun json-null-p (value)
  (eq value 'null))


(defun json-true-p (value)
  (eq value t))


(defun json-false-p (value)
  (eq value nil))


;;; JSON-EQUALP

(defmethod json-equalp (json1 json2)
  (equal json1 json2))


(defmethod json-equalp ((json1 hash-table) (json2 hash-table))
  (and (= (hash-table-count json1)
          (hash-table-count json2))
       (loop
         for key1 being the hash-key in json1
           using (hash-value value1)
         for value2 = (gethash key1 json2)
         always (json-equalp value1 value2))))


(defmethod json-equalp ((json1 array) (json2 array))
  (and (= (length json1)
          (length json2))
       (loop
         for item1 across json1
         for item2 across json2
         always (json-equalp item1 item2))))


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
