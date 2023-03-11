(in-package :cl-jschema)


(defparameter *$schema* "https://json-schema.org/draft/2020-12/schema"
  "The default value for $schema (and currently the only one supported).")


;;; Registry of JSON Schemas

(defvar *json-schema-registry* (make-hash-table :test 'equal)
  "Map of string identifiers to 'JSON-SCHEMA-SPEC objects")


;;; Regex

(defclass regex-box ()
  ((regex-string :initarg :regex-string
                 :reader regex-string)
   (regex :initarg :regex
          :reader regex)))


;;; Enum

(defclass enum-schema ()
  ((items :initarg :items
          :initform nil
          :reader items)))


;;; Const

(defclass const-schema ()
  ((const :initarg :const
          :initform nil
          :reader const)))


;;; Schema per "type" keyword

(defclass json-schema-type-property ()
  ((key :initarg :key
        :reader key)
   (value :initarg :value
          :reader value)))


(defclass json-type-schema ()
  ((type-properties :initarg :type-properties
                    :initform nil
                    :reader type-properties)))


(defun add-to-type-properites (type-properties keyword value)
  (setf (gethash (alexandria:make-keyword keyword) type-properties)
        (make-instance 'json-schema-type-property
                       :key keyword
                       :value value)))


(defmethod get-type-property ((type-schema json-type-schema) keyword)
  (multiple-value-bind (type-prop existsp)
      (gethash (alexandria:make-keyword keyword)
               (type-properties type-schema))
    (values (when type-prop (value type-prop))
            existsp)))


(defclass json-basic-type-schema (json-type-schema) ())


(defmethod sanitize-schema-object ((type-schema json-type-schema) input-spec)
  "Remove keywords from the type-properties and store them in the object's
slots, based on the INPUT-SPEC."
  (loop
    with type-properties = (type-properties type-schema)
    for (keyword slot-name) in input-spec
    do (multiple-value-bind (value existsp)
           (get-type-property type-schema keyword)
         (when existsp
           (setf (slot-value type-schema slot-name) value)
           (remhash (alexandria:make-keyword keyword) type-properties)))))


(defclass json-object-schema (json-type-schema)
  ((additional-properties :initform t   ; i.e. JSON true
                          :reader additional-properties)
   (unevaluated-properties :initform t  ; i.e. JSON true
                           :reader unevaluated-properties)))


(defmethod initialize-instance :after ((object-schema json-object-schema) &key)
  (sanitize-schema-object
   object-schema
   '(("additionalProperties"  additional-properties)
     ("unevaluatedProperties" unevaluated-properties))))


(defclass json-array-schema (json-type-schema)
  ((items :initform t                   ; i.e. JSON true
          :reader items)
   (contains :initform nil
             :reader contains)
   (min-contains :initform nil
                 :reader min-contains)
   (max-contains :initform nil
                 :reader max-contains)))


(defmethod initialize-instance :after ((array-schema json-array-schema) &key)
  (sanitize-schema-object
   array-schema
   '(("items"       items)
     ("contains"    contains)
     ("minContains" min-contains)
     ("maxContains" max-contains))))


;;; Logical operations

(defclass json-logical-schema ()
  ((operator :initarg :operator
             :reader operator)
   (schemas :initarg :schemas
            :reader schemas)))


(defmethod print-object ((schema json-logical-schema) stream)
  (print-unreadable-object (schema stream :type t :identity t)
    (format stream "~a"
            (operator schema))))


;;; JSON Schemas encapsulation

(defclass json-schema-spec ()
  ((id :initarg :id
       :initform nil
       :reader id)
   (annotations :initarg :annotations
                :initform nil
                :reader annotations)
   (condition-schemas :initarg :condition-schemas
                      :initform nil
                      :reader condition-schemas)
   (logical-schemas :initarg :logical-schemas
                    :initform nil
                    :reader logical-schemas)
   (type-schema :initarg :type-schema
                :initform nil
                :reader type-schema)
   (metadata :initarg :metadata
             :initform nil
             :reader metadata)))


;;; Exported JSON Schema objects

(defclass json-schema ()
  ((schema :initarg :schema
           :initform *$schema*
           :reader schema)
   (schema-spec :initarg :schema-spec
                :initform nil
                :reader schema-spec)))
