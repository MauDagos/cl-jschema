(in-package :cl-jschema)

;;; All functions specified in a SATISFIES clause conform to these guidelines:
;;; https://google.github.io/styleguide/lispguide.xml#SATISFIES

;;; Utils

(defun hash-table-of-type-p (hash-table type)
  "Return T if all of the values in HASH-TABLE are of type TYPE."
  (and (hash-table-p hash-table)
       (loop
         for value being the hash-value in hash-table
         always (typep value type))))


(defun array-of-type-p (array type)
  (and (arrayp array)
       (loop
         for item across array
         always (typep item type))))


;;; JSON objects

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun hash-table-of-schema-likes-p (hash-table)
    (hash-table-of-type-p hash-table 'schema-like)))

(deftype hash-table-of-schema-likes ()
  `(and hash-table (satisfies hash-table-of-schema-likes-p)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun hash-table-of-array-of-strings-p (hash-table)
    (hash-table-of-type-p hash-table 'array-of-strings)))

(deftype hash-table-of-array-of-strings ()
  `(and hash-table (satisfies hash-table-of-array-of-strings-p)))


;;; JSON arrays

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun non-empty-array-p (array)
    (and (arrayp array)
         (plusp (length array)))))

(deftype non-empty-array ()
  `(and simple-vector (satisfies non-empty-array-p)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun array-of-strings-p (array)
    (array-of-type-p array 'string)))

(deftype array-of-strings ()
  `(and simple-vector (satisfies array-of-strings-p)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun array-of-hash-tables-p (array)
    (array-of-type-p array 'hash-table)))

(deftype array-of-hash-tables ()
  `(and simple-vector (satisfies array-of-hash-tables-p)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun array-of-schema-likes-p (array)
    (array-of-type-p array 'schema-like)))

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun json-true-schema-p (json-schema)
    (and (typep json-schema 'json-schema)
         (null (ref json-schema))
         (json-true-p (schema-spec json-schema)))))

(deftype json-true-schema ()
  `(and json-schema (satisfies json-true-schema-p)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun json-false-schema-p (json-schema)
    (and (typep json-schema 'json-schema)
         (null (ref json-schema))
         (json-false-p (schema-spec json-schema)))))

(deftype json-false-schema ()
  `(and json-schema (satisfies json-false-schema-p)))


;;; Others / helpers

(deftype string-or-array-of-strings ()
  `(or string array-of-strings))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun integer-like-p (value)
    (or (integerp value)
        ;; Numbers with a zero fractional part are considered integers. E.g. 1.0
        (and (numberp value)
             (zerop (mod value 1))))))

(deftype integer-like ()
  `(satisfies integer-like-p))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun positive-number-p (value)
    (and (numberp value)
         (plusp value))))

(deftype positive-number ()
  `(and number (satisfies positive-number-p)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun non-negative-number-p (value)
    (and (numberp value)
         (>= value 0))))

(deftype non-negative-number ()
  `(and number (satisfies non-negative-number-p)))


(deftype regex ()
  `(and string))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun uri-reference-p (value)
    (and (stringp value)
         (plusp (length value))
         (not (null (ignore-errors (puri:parse-uri value)))))))

(deftype uri-reference ()
  `(and string (satisfies uri-reference-p)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun uri-reference-without-fragment-p (value)
    (and (typep value 'uri-reference)
         (null (puri:uri-fragment (puri:parse-uri value))))))

(deftype uri-reference-without-fragment ()
  `(and uri-reference (satisfies uri-reference-without-fragment-p)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun anchor-like-p (value)
    "Anchors must start with a letter followed by any number of letters, digits,
-, _, :, or ."
    (and (stringp value)
         (ppcre:scan (load-time-value
                      (ppcre:create-scanner "^[a-zA-Z][a-zA-Z0-9:_.\-]*$"))
                     value))))

(deftype anchor-like ()
  `(and string (satisfies anchor-like-p)))
