(in-package :cl-jschema)


;;; JSON objects

(defun hash-table-of-schema-likes-p (hash-table)
  (hash-table-of-type-p hash-table 'schema-like))

(deftype hash-table-of-schema-likes ()
  `(and hash-table (satisfies hash-table-of-schema-likes-p)))


(defun hash-table-of-array-of-strings-p (hash-table)
  (hash-table-of-type-p hash-table 'array-of-strings))

(deftype hash-table-of-array-of-strings ()
  `(and hash-table (satisfies hash-table-of-array-of-strings-p)))


;;; JSON arrays

(defun non-empty-array-p (array)
  (plusp (length array)))

(deftype non-empty-array ()
  `(and simple-vector (satisfies non-empty-array-p)))


(defun array-of-strings-p (array)
  (array-of-type-p array 'string))

(deftype array-of-strings ()
  `(and simple-vector (satisfies array-of-strings-p)))


(defun array-of-hash-tables-p (array)
  (array-of-type-p array 'hash-table))

(deftype array-of-hash-tables ()
  `(and simple-vector (satisfies array-of-hash-tables-p)))


(defun array-of-schema-likes-p (array)
  (array-of-type-p array 'schema-like))

(deftype non-empty-array-of-schema-likes ()
  `(and non-empty-array (satisfies array-of-schema-likes-p)))


;;; JSON symbols

(deftype json-null ()
  `(satisfies json-null-p))

(deftype json-true ()
  `(satisfies json-true-p))

(deftype json-false ()
  `(satisfies json-false-p))

(deftype json-boolean ()
  `(or json-true json-false))


;;; JSON Schema

(deftype schema-like ()
  `(or json-boolean hash-table))


(defun json-true-schema-p (json-schema)
  (json-true-p (schema-spec json-schema)))

(deftype json-true-schema ()
  `(and json-schema (satisfies json-true-schema-p)))


(defun json-false-schema-p (json-schema)
  (json-false-p (schema-spec json-schema)))

(deftype json-false-schema ()
  `(and json-schema (satisfies json-false-schema-p)))


;;; Others / helpers

(deftype string-or-array-of-strings ()
  `(or string array-of-strings))


(defun integer-like-p (value)
  (or (integerp value)
      ;; Numbers with a zero fractional part are considered integers. E.g. 1.0
      (and (numberp value)
           (zerop (mod value 1)))))

(deftype integer-like ()
  `(satisfies integer-like-p))


(deftype regex ()
  `(and string))



(defun uri-reference-p (value)
  (and (stringp value)
       (plusp (length value))
       (not (null (ignore-errors (puri:parse-uri value))))))

(deftype uri-reference ()
  `(satisfies uri-reference-p))


(defun uri-reference-without-fragment-p (value)
  (null (puri:uri-fragment (puri:parse-uri value))))

(deftype uri-reference-without-fragment ()
  `(and uri-reference (satisfies uri-reference-without-fragment-p)))
