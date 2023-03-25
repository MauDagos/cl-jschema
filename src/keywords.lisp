(in-package :cl-jschema)


;;; All keywords

(defparameter *keyword-specs*
  '(;; schema dialect
    ("$schema" . string)
    ;; schema identification
    ("$id"     . uri-reference-without-fragment)
    ("$anchor" . anchor-like)
    ("$ref"    . uri-reference)
    ("$defs"   . hash-table-of-schema-likes)
    ;; annotations
    ("title"       . string)
    ("description" . string)
    ;; * default - if the JSON Schema is parsable, then it can be anything
    ("default"     . t)
    ("examples"    . simple-vector)
    ("readOnly"    . json-boolean)
    ("writeOnly"   . json-boolean)
    ("deprecated"  . json-boolean)
    ("$comment"    . string)
    ;; const schema - if the JSON Schema is parsable, then it can be anything
    ("const" . t)
    ;; enum schema
    ("enum" . non-empty-array)
    ;; type schema and per-type keywords
    ("type" . string-or-array-of-strings)
    ;; * string type
    ("minLength"        . non-negative-number)
    ("maxLength"        . non-negative-number)
    ("pattern"          . regex)
    ("format"           . string)
    ("contentEncoding"  . string)
    ("contentMediaType" . string)
    ;; * number / integer type
    ("multipleOf"       . positive-number)
    ("minimum"          . number)
    ("exclusiveMinimum" . number)
    ("maximum"          . number)
    ("exclusiveMaximum" . number)
    ;; * object type
    ("properties"            . hash-table-of-schema-likes)
    ("patternProperties"     . hash-table-of-schema-likes)
    ("additionalProperties"  . schema-like)
    ("unevaluatedProperties" . schema-like)
    ("required"              . array-of-strings)
    ("propertyNames"         . schema-like)
    ("minProperties"         . non-negative-number)
    ("maxProperties"         . non-negative-number)
    ("dependentRequired"     . hash-table-of-array-of-strings)
    ("dependentSchemas"      . hash-table-of-schema-likes)
    ;; * array type
    ("items"       . schema-like)
    ("prefixItems" . non-empty-array-of-schema-likes)
    ("contains"    . schema-like)
    ("minContains" . non-negative-number)
    ("maxContains" . non-negative-number)
    ("minItems"    . non-negative-number)
    ("maxItems"    . non-negative-number)
    ("uniqueItems" . json-boolean)
    ;; composition
    ("allOf" . non-empty-array-of-schema-likes)
    ("anyOf" . non-empty-array-of-schema-likes)
    ("oneOf" . non-empty-array-of-schema-likes)
    ("not"   . schema-like)
    ;; conditional
    ("if"   . schema-like)
    ("then" . schema-like)
    ("else" . schema-like))
  "Map of allowed keywords in the JSON Schema to their Lisp type.")


(defun keyword-type (keyword)
  "Return the expected Lisp type for JSON Schema KEYWORD."
  (cdr (assoc keyword *keyword-specs* :test 'equal)))


;;; Annotations

(defparameter *annotation-keywords*
  '("title"
    "description"
    "default"
    "examples"
    "readOnly"
    "writeOnly"
    "deprecated"
    "$comment")
  "List of keywords that are considered just annotations, according to the JSON
Schema documentation.")


;;; Logical operators

(defparameter *logical-keywords*
  '("allOf"
    "anyOf"
    "oneOf"
    "not"))


;;; Conditional operators

(defparameter *conditional-keywords*
  '("if"
    "then"
    "else"))


;;; Type

(defstruct (type-spec (:constructor make-type-spec (name lisp-type keywords)))
  name
  lisp-type
  keywords)


(defparameter *type-specs*
  (mapcar (lambda (spec) (apply 'make-type-spec spec))
          '(("string" string ("minLength"
                              "maxLength"
                              "pattern"
                              "format"
                              "contentEncoding"
                              "contentMediaType"))
            ("number" number ("multipleOf"
                              "minimum"
                              "exclusiveMinimum"
                              "maximum"
                              "exclusiveMaximum"))
            ("integer" integer-like ("multipleOf"
                                     "minimum"
                                     "exclusiveMinimum"
                                     "maximum"
                                     "exclusiveMaximum"))
            ("object" hash-table ("properties"
                                  "patternProperties"
                                  "additionalProperties"
                                  "unevaluatedProperties"
                                  "required"
                                  "propertyNames"
                                  "minProperties"
                                  "maxProperties"
                                  "dependentRequired"
                                  "dependentSchemas"))
            ("array" simple-vector ("items"
                                    "prefixItems"
                                    "contains"
                                    "minContains"
                                    "maxContains"
                                    "minItems"
                                    "maxItems"
                                    "uniqueItems"))
            ("boolean" json-boolean ())
            ("null" json-null ())))
  "Map of allowed \"type\" values to the Lisp type that the JSON being validated
must satisfy and JSON Schema keywords which map to the type.")


(defparameter *type-keywords*
  (append (list "type")
          (remove-duplicates (alexandria:flatten
                              (mapcar 'type-spec-keywords *type-specs*))
                             :test 'equal))
  "The list of allowed JSON Schema keywords for defining a valid value.")


(defun type-spec (type)
  "Return the TYPE-SPEC for JSON Schema keyword TYPE."
  (find type *type-specs* :key 'type-spec-name :test 'equal))


(defun type-for-keyword (keyword)
  "Return the Lisp type for JSON Schema KEYWORD."
  (loop
    for spec in *type-specs*
    when (member keyword (type-spec-keywords spec) :test 'equal)
      return (type-spec-name spec)))


;;; Pretty messages

(defparameter *type-error-format-strings*
  '((json-boolean
     . "Keyword ~a expects a boolean")
    (string
     . "Keyword ~a expects a string")
    (regex
     . "Keyword ~a expects a regular expression")
    (uri-reference
     . "Keyword ~a expects a URI")
    (uri-reference-without-fragment
     . "Keyword ~a expects a URI without a fragment")
    (anchor-like
     . "Keyword ~a expects a string starting with a letter followed by any ~
        number of letters, digits, -, _, : or .")
    (number
     . "Keyword ~a expects a number")
    (positive-number
     . "Keyword ~a expects a positive number")
    (non-negative-number
     . "Keyword ~a expects a non-negative number")
    (simple-vector
     . "Keyword ~a expects a JSON array")
    (non-empty-array
     . "Keyword ~a expects a non-empty JSON array")
    (array-of-strings
     . "Keyword ~a expects a JSON array of zero or more strings")
    (array-of-hash-tables
     . "Keyword ~a expects a JSON array of zero or more JSON objects")
    (non-empty-array-of-schema-likes
     . "Keyword ~a expects a non-empty JSON array of schemas")
    (schema-like
     . "Keyword ~a expects a schema")
    (hash-table-of-schema-likes
     . "Keyword ~a expects a JSON object with schema values")
    (hash-table-of-array-of-strings
     . "Keyword ~a expects a JSON object with JSON array of strings values"))
  "Map of Lisp types to format strings, which should expect the keyword as a
parameter. These are the validation error messages returned when validating a
JSON Schema.")


(defun type-error-format-string (type)
  "Return the format string for invalid values for the Lisp TYPE."
  (cdr (assoc type *type-error-format-strings* :test 'equal)))


(defparameter *validation-error-messages*
  '(("false-schema"                     ; Not an actual keyword in JSON Schema
     . "Value not allowed")
    ("const"
     . "Value not equal to const")
    ("enum"
     . "Value not part of enum")
    ("type"
     . "Not of type ~s")
    ("types"                            ; Not an actual keyword in JSON Schema
     . "Not of any of types: ~{~s~^, ~}")
    ("minLength"
     . "String is not at least ~d characters long")
    ("maxLength"
     . "String is longer than ~d characters")
    ("pattern"
     . "String does not match regex ~a")
    ("multipleOf"
     . "Number is not a multiple of ~d")
    ("minimum"
     . "Number is less than ~d")
    ("exclusiveMinimum"
     . "Number is less or equal to ~d")
    ("maximum"
     . "Number is greater than ~d")
    ("exclusiveMaximum"
     . "Number is greater or equal to ~d")
    ("required"
     . "Required property ~s not found")
    ("minProperties"
     . "JSON object does not have at least ~d properties")
    ("maxProperties"
     . "JSON object has more than ~d properties")
    ("minItems"
     . "JSON array does not have at least ~d items")
    ("maxItems"
     . "JSON array has more than ~d items")
    ("uniqueItems"
     . "JSON array contains duplicate values")
    ("additionalProperties"
     . "Property ~s is not allowed")
    ("items"
     . "JSON array contains extra items")
    ("minContains"
     . "Less than ~d items in the JSON array satisfy the contains schema")
    ("maxContains"
     . "More than ~d items in the JSON array satisfy the contains schema")
    ("allOf"
     . "Condition for \"allOf\" not met")
    ("anyOf"
     . "Condition for \"anyOf\" not met")
    ("oneOf"
     . "Condition for \"oneOf\" not met")
    ("not"
     . "Condition for \"not\" not met")
    ("$ref"
     . "Value does not satisfy the JSON Schema found by $ref ~s")
    ("unresolvable-ref"                 ; Not an actual keyword in JSON Schema
     . "Cannot validate value because no JSON Schema was found for $ref ~s"))
  "A map of JSON Schema keywords (well, not all are real keywords, check the
actual map) to format strings. These are the validation error messages returned
when validating a value with a JSON Schema.")


(defun keyword-validation-format-string (keyword)
  "Return the format string for invalid values for the JSON Schema KEYWORD."
  (or (cdr (assoc keyword *validation-error-messages* :test 'equal))
      (error "No validation message for keyword ~s" keyword)))
