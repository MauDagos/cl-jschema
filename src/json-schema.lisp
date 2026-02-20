(in-package :cl-jschema)


(defparameter *$schema* "https://json-schema.org/draft/2020-12/schema"
  "The default value for $schema (and currently the only one supported).")


;;; Regex

(defclass regex-box ()
  ((regex-string :initarg :regex-string
                 :reader regex-string
                 :documentation "The original regex from the JSON Schema.")
   (regex :initarg :regex
          :reader regex
          :documentation "A CL-PPCRE scanner."))
  (:documentation "A class to encapsulate a regex with it's scanner."))


;;; Enum

(defclass enum-schema ()
  ((items :initarg :items
          :initform nil
          :reader items
          :documentation "The list of valid JSON values."))
  (:documentation "A class to represent an enum JSON Schema."))


;;; Const

(defclass const-schema ()
  ((const :initarg :const
          :initform nil
          :reader const
          :documentation "The only valid JSON value."))
  (:documentation "A class to represent a const JSON Schema."))


;;; Schema per "type" keyword

(defclass json-schema-type-property ()
  ((key :initarg :key
        :reader key
        :documentation "The original keyword from the JSON Schema.")
   (value :initarg :value
          :reader value
          :documentation "The parsed JSON value from the JSON Schema."))
  (:documentation "A class to represent a property of a JSON Schema."))


(defclass json-type-schema ()
  ((type-properties :initarg :type-properties
                    :initform nil
                    :reader type-properties
                    :documentation "A hash-table mapping JSON Schema keywords
                                    (converted to CL keywords) to instances of
                                    'JSON-SCHEMA-TYPE-PROPERTY."))
  (:documentation "A class to represent the core of a JSON Schema for validating
                   a value."))


(defun add-to-type-properites (keyword value type-properties)
  (setf (gethash (find-symbol keyword :keyword) type-properties)
        (make-instance 'json-schema-type-property
                       :key keyword
                       :value value)))


(defmethod get-type-property ((type-schema json-type-schema) keyword)
  (multiple-value-bind (type-prop existsp)
      (gethash (find-symbol keyword :keyword) (type-properties type-schema))
    (values (when type-prop (value type-prop))
            existsp)))


(defclass json-basic-type-schema (json-type-schema) ()
  (:documentation "A class to represent a JSON Schema for validating 'basic'
                   values (i.e. not JSON objects nor arrays)."))


(defmethod sanitize-schema-object ((type-schema json-type-schema) input-spec)
  "Remove keywords from the type-properties and store them in the object's
slots, based on the INPUT-SPEC."
  (loop
    with type-properties = (type-properties type-schema)
    for (keyword writer) in input-spec
    do (multiple-value-bind (value existsp)
           (get-type-property type-schema keyword)
         (when existsp
           (funcall writer value type-schema)
           (remhash (find-symbol keyword :keyword) type-properties)))))


(defclass json-object-schema (json-type-schema)
  ((additional-properties :initform nil
                          :accessor additional-properties
                          :documentation "NIL or an instance of a 'JSON-SCHEMA.
                                          Refers to the value of the JSON Schema
                                          keyword 'additionalProperties', if any.")
   (unevaluated-properties :initform nil
                           :accessor unevaluated-properties
                           :documentation "NIL or an instance of a 'JSON-SCHEMA.
                                           Refers to the value of the JSON Schema
                                           keyword 'unevaluatedProperties', if any."))
  (:documentation "A class to represent a JSON Schema for validating a JSON
                   object."))


(defmethod initialize-instance :after ((object-schema json-object-schema) &key)
  (sanitize-schema-object
   object-schema
   `(("additionalProperties"  ,#'(setf additional-properties))
     ("unevaluatedProperties" ,#'(setf unevaluated-properties)))))


(defclass json-array-schema (json-type-schema)
  ((items :initform (json-true-schema)
          :accessor items
          :documentation "An instance of a 'JSON-SCHEMA. Refers to the value of
                          the JSON Schema keyword 'items'.")
   (contains :initform nil
             :accessor contains
             :documentation "NIL or an instance of a 'JSON-SCHEMA. Refers to the
                             value of the JSON Schema keyword 'contains', if
                             specified.")
   (min-contains :initform nil
                 :accessor min-contains
                 :documentation "NIL or an integer. Refers to the value of the
                                 JSON Schema keyword 'minContains', if
                                 specified.")
   (max-contains :initform nil
                 :accessor max-contains
                 :documentation "NIL or an integer. Refers to the value of the
                                 JSON Schema keyword 'maxContains', if
                                 specified."))
  (:documentation "A class to represent a JSON Schema for validating a JSON
                   array."))


(defmethod initialize-instance :after ((array-schema json-array-schema) &key)
  (sanitize-schema-object
   array-schema
   `(("items"       ,#'(setf items))
     ("contains"    ,#'(setf contains))
     ("minContains" ,#'(setf min-contains))
     ("maxContains" ,#'(setf max-contains)))))


;;; Logical operations

(defclass json-logical-schema ()
  ((operator :initarg :operator
             :reader operator
             :documentation "The original keyword from the JSON Schema.")
   (schemas :initarg :schemas
            :reader schemas
            :documentation "A list of instances of 'JSON-SCHEMA."))
  (:documentation "A class to represent a combination of JSON Schemas with a
                   logical operator."))


(defmethod print-object ((schema json-logical-schema) stream)
  (print-unreadable-object (schema stream :type t :identity t)
    (format stream "~s"
            (operator schema))))


;;; JSON Schemas encapsulation

(defclass json-schema-spec ()
  ((annotations :initarg :annotations
                :initform nil
                :reader annotations
                :documentation "NIL or A hash-table of JSON Schema keywords
                                mapped to values as parsed by JZON. These
                                keywords are described as merely for annotation
                                in the JSON Schema documentation.")
   (condition-schemas :initarg :condition-schemas
                      :initform nil
                      :reader condition-schemas
                      :documentation "NIL or a hash-table of JSON Schema keywords
                                      mapped to instances of 'JSON-SCHEMA. For
                                      'If-Then-Else' validation.")
   (logical-schemas :initarg :logical-schemas
                    :initform nil
                    :reader logical-schemas
                    :documentation "A list of instances of 'JSON-LOGICAL-SCHEMA.
                                    For validating logical operators.")
   (type-schema :initarg :type-schema
                :initform nil
                :reader type-schema
                :documentation "NIL or an instance of type 'JSON-TYPE-SCHEMA,
                                'ENUM-SCHEMA or 'CONST-SCHEMA. For validating
                                incoming values.")
   (metadata :initarg :metadata
             :initform nil
             :reader metadata
             :documentation "NIL or a hash-table of keys to values as parsed by
                             JZON. Here goes anything that's not part of the JSON
                             Schema spec.")))


;;; Exported JSON Schema object

(defclass json-schema ()
  ((base-uri :initarg :base-uri
             :initform nil
             :reader base-uri
             :documentation "NIL or an instance of 'PURI:URI. Refers to the base
                             URI of the JSON Schema (see official JSON Schema
                             documentation).")
   (schema :initarg :schema
           :initform *$schema*
           :reader schema
           :documentation "The JSON Schema dialect (see official JSON Schema
                           documentation).")
   (id :initarg :id
       :initform nil
       :reader id
       :documentation "NIL or the value of '$id' for this JSON Schema")
   (anchor :initarg :anchor
           :initform nil
           :reader anchor
           :documentation "NIL or the value of '$anchor' for this JSON Schema")
   (ref :initarg :ref
        :initform nil
        :reader ref
        :documentation "NIL or the value of '$ref' for this JSON Schema")
   (defs :initarg :defs
         :initform nil
         :reader defs
         :documentation "NIL or a hash-table of keys to instances of 'JSON-SCHEMA.")
   (schema-spec :initarg :schema-spec
                :initform nil
                :reader schema-spec
                :documentation "The actual spec to validate values against. Can
                                be T, NIL or a 'JSON-SCHEMA-SPEC.")
   (self-registry :initarg :self-registry
                  :reader self-registry
                  :documentation "A hash-table of JSON Pointers to 'JSON-SCHEMA
                                  objects. This corresponds to a map of available
                                  JSON Schemas within the root JSON Schema.")))


(defun json-true-schema ()
  (make-instance 'json-schema :schema-spec t))


(defun json-false-schema ()
  (make-instance 'json-schema :schema-spec nil))


(defmethod get-inner-schema ((schema json-schema) json-pointer)
  (gethash json-pointer (self-registry schema)))


(defmethod get-schema-from-ref ((schema json-schema))
  "Return a 'JSON-SCHEMA by resolving $ref from JSON-SCHEMA. Can return NIL."
  (a:when-let ((ref (ref schema)))
    (let* ((fragmentp (eq #\# (char ref 0))) ; starts with '#'
           (maybe-json-pointer (unescape-json-pointer (if fragmentp
                                                          (subseq ref 1)
                                                          ref))))
      (or
       ;; $ref can be a JSON Pointer to an inner schema
       (get-inner-schema schema maybe-json-pointer)
       ;; $ref can be a non-relative URI
       (get-schema (puri:parse-uri ref))
       ;; $ref resolves against the base URI, if any. $ref can be either a path
       ;; or a fragment.
       (a:when-let ((base-uri (base-uri schema)))
         (let ((uri-copy (puri:copy-uri base-uri)))
           (if fragmentp
               (setf (puri:uri-fragment uri-copy) maybe-json-pointer)
               (setf (puri:uri-path uri-copy) maybe-json-pointer))
           (get-schema uri-copy)))))))


(defmethod print-object ((schema json-schema) stream)
  (print-unreadable-object (schema stream :type t :identity t)
    (format stream "~s"
            (id schema))))
