(in-package :cl-jschema/tests)


(5am:in-suite :cl-jschema.tests)


(defun expected-message (expected-pointer expected-keyword &optional msg-arg)
  (format nil "~s : ~?"
          expected-pointer
          (cl-jschema::keyword-validation-format-string expected-keyword)
          (list msg-arg)))


(cl-jschema-test :empty-json-schemas-test
  (let ((empty-schema (cl-jschema:parse "{}"))
        (true-schema  (cl-jschema:parse "true"))
        (false-schema (cl-jschema:parse "false")))
    (dolist (json
             (list 42
                   "I'm a string"
                   (jzon:parse "{\"an\":[\"arbitrarily\",\"nested\"],\"data\":\"structure\"}")))
      (valid-value empty-schema json)
      (valid-value true-schema json)
      (invalid-value false-schema json (expected-message "" "false-schema")))
    (invalid-value false-schema "Resistance is futile...  This will always fail!!!"
                   (expected-message "" "false-schema"))))


(cl-jschema-test :valid-schema-dialect-test
  (5am:finishes
    (cl-jschema:parse "{
                         \"$schema\": \"https://json-schema.org/draft/2020-12/schema\"
                       }"))
  (signals cl-jschema:invalid-schema "$schema is only allowed at the root level"
    (cl-jschema:parse "{
                         \"additionalProperties\": {
                           \"$schema\": \"https://json-schema.org/draft/2020-12/schema\"
                         }
                       }")))


(cl-jschema-test :valid-id-test
  (dolist (id '( ; Full URI without fragment
                "http://yourdomain.com/schemas/myschema.json"
                ;; Relative URI
                "https://example.com/schemas/address"
                "/schemas/address"))
    (let (schema)
      (5am:finishes
        (setq schema (cl-jschema:parse (format nil "{\"$id\":~s}" id))))
      (5am:is (eq schema (cl-jschema:get-schema id)))))
  (dolist (schema '(
                    ;; Nested is also allowed
                    "{
                       \"additionalProperties\": {
                         \"$id\": \"http://yourdomain.com/schemas/myschema.json\"
                       }
                     }"))
    (5am:finishes
      (cl-jschema:parse schema))))


(cl-jschema-test :basic-number-type-test
  (let ((json-schema (cl-jschema:parse "{ \"type\": \"number\" }")))
    (valid-value json-schema 42 -1 5.0 2.99792458e8)
    (invalid-value json-schema "42" (expected-message "" "type" "number"))))


(cl-jschema-test :basic-integer-type-test
  (let ((json-schema (cl-jschema:parse "{ \"type\": \"integer\" }")))
    (valid-value json-schema 42 -1 1.0)
    (let ((expected-msg (expected-message "" "type" "integer")))
      (invalid-values json-schema `((3.1415926 ,expected-msg)
                                    ("42"      ,expected-msg))))))


(cl-jschema-test :basic-string-type-test
  (let ((json-schema (cl-jschema:parse "{ \"type\": \"string\" }")))
    (valid-value json-schema "I'm a string")
    (invalid-value json-schema 42 (expected-message "" "type" "string"))))


(cl-jschema-test :basic-multiple-types-test
  (let ((json-schema (cl-jschema:parse "{ \"type\": [\"number\", \"string\"] }")))
    (valid-value json-schema 42 "Life, the universe, and everything")
    (invalid-json json-schema "[\"Life\", \"the universe\", \"and everything\"]"
                  (expected-message "" "types" (list "number" "string")))))


(cl-jschema-test :string-max/min-length-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"type\":\"string\",
                                          \"minLength\":2,
                                          \"maxLength\":3
                                        }")))
    (valid-value json-schema "AB" "ABC")
    (invalid-values json-schema `(("A"    ,(expected-message "" "minLength" 2))
                                  ("ABCD" ,(expected-message "" "maxLength" 3))))))


(cl-jschema-test :string-pattern-test
  (let* ((regex "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$")
         (json-schema (cl-jschema:parse
                       (format nil "{
                                      \"type\": \"string\",
                                      \"pattern\": ~s
                                    }"
                               regex))))
    (valid-value json-schema "555-1212" "(888)555-1212")
    (invalid-values json-schema `(("(888)555-1212 ext. 532"
                                   ,(expected-message "" "pattern" regex))
                                  ("(800)FLOWERS"
                                   ,(expected-message "" "pattern" regex)))))
  (signals cl-jschema:invalid-schema "This regex is not valid: [0-a"
    (cl-jschema:parse "{\"pattern\": \"[0-a\"}")))


(cl-jschema-test :valid-string-format-test
  (5am:finishes
    (cl-jschema:parse "{
                         \"type\": \"string\",
                         \"format\": \"date-time\"
                       }")))


(cl-jschema-test :number-multiple-of-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"type\": \"number\",
                                          \"multipleOf\": 10
                                        }")))
    (valid-value json-schema 0 10 20)
    (invalid-value json-schema 23 (expected-message "" "multipleOf" 10))))


(cl-jschema-test :number-range-test
  ;; -1 <= x <= 100
  (let ((json-schema-closed (cl-jschema:parse "{
                                                 \"type\": \"number\",
                                                 \"minimum\": -1,
                                                 \"maximum\": 100
                                               }"))
        ;; -1 < x < 100
        (json-schema-opened (cl-jschema:parse "{
                                                 \"type\": \"number\",
                                                 \"exclusiveMinimum\": -1,
                                                 \"exclusiveMaximum\": 100
                                               }"))
        ;; 0 <= x < 100
        (json-schema-mixed  (cl-jschema:parse "{
                                                 \"type\": \"number\",
                                                 \"exclusiveMinimum\": -1,
                                                 \"minimum\": 0,
                                                 \"exclusiveMaximum\": 100,
                                                 \"maximum\": 101
                                               }")))
    ;; Valid cases in all schemas
    (dolist (num '(0 3.1415 42 99))
      (valid-value json-schema-closed num)
      (valid-value json-schema-opened num)
      (valid-value json-schema-mixed  num))
    ;; -1 is only allowed in the "closed" schema
    (valid-value json-schema-closed -1)
    (invalid-value json-schema-opened -1
                   (expected-message "" "exclusiveMinimum" -1))
    (invalid-value json-schema-mixed -1
                   "JSON Schema validation found these errors:

\"\" : Number is less than 0

\"\" : Number is less or equal to -1")
    ;; 100 is only allowed in the "closed" schema
    (valid-value json-schema-closed 100)
    (invalid-value json-schema-opened 100
                   (expected-message "" "exclusiveMaximum" 100))
    (invalid-value json-schema-mixed  100
                   (expected-message "" "exclusiveMaximum" 100))
    ;; -2 is not allowed in any
    (invalid-value json-schema-closed -2
                   (expected-message "" "minimum" -1))
    (invalid-value json-schema-opened -2
                   (expected-message "" "exclusiveMinimum" -1))
    (invalid-value json-schema-mixed -2
                   "JSON Schema validation found these errors:

\"\" : Number is less than 0

\"\" : Number is less or equal to -1")
    ;; 101 is not allowed in any
    (invalid-value json-schema-closed 101
                   (expected-message "" "maximum" 100))
    (invalid-value json-schema-opened 101
                   (expected-message "" "exclusiveMaximum" 100))
    (invalid-value json-schema-mixed  101
                   (expected-message "" "exclusiveMaximum" 100))))


(cl-jschema-test :basic-enum-test
  (let ((json-schema (cl-jschema:parse "{\"enum\": [\"red\", \"amber\", \"green\", null, 42]}")))
    (valid-value json-schema "red" (jzon:parse "null") 42)
    (let ((expected-msg (expected-message "" "enum")))
      (invalid-values json-schema `((0      ,expected-msg)
                                    ("blue" ,expected-msg))))))


(cl-jschema-test :basic-const-test
  (let ((json-schema
          (cl-jschema:parse "{
                               \"properties\": {
                                 \"country\": {
                                   \"const\": \"United States of America\"
                                 }
                               }
                             }")))
    (valid-json json-schema "{\"country\": \"United States of America\"}")
    (invalid-json json-schema "{\"country\": \"Canada\"}"
                  (expected-message "/country" "const"))))


(cl-jschema-test :basic-boolean-type-test
  (let ((json-schema (cl-jschema:parse "{\"type\": \"boolean\"}")))
    (valid-json json-schema "true" "false")
    (let ((expected-msg (expected-message "" "type" "boolean")))
      (invalid-values json-schema `(("true" ,expected-msg)
                                    (0      ,expected-msg))))))


(cl-jschema-test :basic-null-type-test
  (let ((json-schema (cl-jschema:parse "{\"type\": \"null\"}")))
    (valid-json json-schema "null")
    (let ((expected-msg (expected-message "" "type" "null")))
      (invalid-json json-schema "false" expected-msg)
      (invalid-values json-schema `((0   ,expected-msg)
                                    (""  ,expected-msg)
                                    (nil ,expected-msg))))))


(cl-jschema-test :basic-object-type-test
  (let ((json-schema (cl-jschema:parse "{\"type\":\"object\"}")))
    (valid-json json-schema "{
                               \"key\":\"value\",
                               \"anotherKey\":42
                             }")
    (let ((expected-msg (expected-message "" "type" "object")))
      (invalid-value json-schema "Not an object" expected-msg)
      (invalid-json json-schema "[\"An\",\"array\",\"not\",\"an\",\"object\"]"
                    expected-msg))))


(cl-jschema-test :object-properties-test
  (let ((json-schema
          (cl-jschema:parse "{
                               \"type\": \"object\",
                               \"properties\": {
                                 \"number\": {\"type\": \"number\"},
                                 \"street_name\": {\"type\": \"string\"},
                                 \"street_type\": {\"enum\": [\"Street\", \"Avenue\", \"Boulevard\"]}
                               }
                             }")))
    (valid-json json-schema
                "{\"number\":1600, \"street_name\":\"Pennsylvania\", \"street_type\":\"Avenue\"}"
                "{\"number\":1600, \"street_name\":\"Pennsylvania\"}"
                "{ }"
                "{\"number\":1600, \"street_name\":\"Pennsylvania\", \"street_type\":\"Avenue\", \"direction\":\"NW\"}")
    (invalid-jsons json-schema `(("{\"number\":\"1600\", \"street_name\":\"Pennsylvania\", \"street_type\":\"Avenue\"}"
                                  ,(expected-message "/number" "type" "number"))
                                 ("{\"number\":1600, \"street_name\":42, \"street_type\":\"Avenue\"}"
                                  ,(expected-message "/street_name" "type" "string"))
                                 ("{\"number\":1600, \"street_name\":\"Pennsylvania\", \"street_type\":\"Av\"}"
                                  ,(expected-message "/street_type" "enum"))))))


(cl-jschema-test :object-pattern-properties-test
  (let ((json-schema
          (cl-jschema:parse "{
                               \"type\": \"object\",
                               \"patternProperties\": {
                                 \"^S_\": {\"type\": \"string\"},
                                 \"^I_\": {\"type\": \"integer\"}
                               }
                             }")))
    (valid-json json-schema
                "{\"S_25\" : \"This is a string\"}"
                "{\"I_0\": 42}"
                "{\"keyword\": \"value\"}")
    (invalid-jsons json-schema
                   `(("{\"S_0\": 42}"
                      ,(expected-message "/S_0" "type" "string"))
                     ("{\"I_42\": \"This is a string\"}"
                      ,(expected-message "/I_42" "type" "integer"))))))


(cl-jschema-test :object-additional-properties-test
  (let* ((template "{
                      \"type\": \"object\",
                      \"properties\": {
                        \"number\": {\"type\": \"number\"},
                        \"street_name\": {\"type\": \"string\"},
                        \"street_type\": {\"enum\": [\"Street\", \"Avenue\", \"Boulevard\"]}
                      },
                      \"additionalProperties\": ~a
                    }")
         (additional-true  (cl-jschema:parse (format nil template "true")))
         (additional-false (cl-jschema:parse (format nil template "false")))
         (additional-type  (cl-jschema:parse (format nil template "{\"type\": \"string\"}"))))
    (dolist (object
             '("{\"number\":1600, \"street_name\":\"Pennsylvania\", \"street_type\":\"Avenue\"}"
               "{\"number\":1600, \"street_name\":\"Pennsylvania\"}"
               "{ }"))
      (valid-json additional-false object)
      (valid-json additional-type  object))
    (let ((extra-string-field
            (jzon:parse "{\"number\":1600, \"street_name\":\"Pennsylvania\", \"street_type\":\"Avenue\", \"direction\":\"NW\"}"))
          (extra-number-field
            (jzon:parse "{\"number\":1600, \"street_name\":\"Pennsylvania\", \"street_type\":\"Avenue\", \"office_number\":201}")))
      (valid-value additional-true extra-string-field extra-number-field)
      (invalid-values additional-false
                      `((,extra-string-field
                         ,(expected-message "" "additionalProperties" "direction"))
                        (,extra-number-field
                         ,(expected-message "" "additionalProperties" "office_number"))))
      (valid-value additional-type extra-string-field)
      (invalid-value additional-type extra-number-field
                     (expected-message "/office_number" "type" "string"))))
  (let ((json-schema
          (cl-jschema:parse "{
                               \"type\": \"object\",
                               \"properties\": {
                                 \"builtin\": {\"type\": \"number\"}
                               },
                               \"patternProperties\": {
                                 \"^S_\": {\"type\": \"string\"},
                                 \"^I_\": {\"type\": \"integer\"}
                               },
                               \"additionalProperties\": {\"type\": \"string\"}
                             }")))
    (valid-json json-schema "{\"builtin\": 42}" "{\"keyword\": \"value\"}")
    (invalid-json json-schema "{\"keyword\": 42}"
                  (expected-message "/keyword" "type" "string"))))


(cl-jschema-test :object-additional-properties-limited-to-same-subschema-test
  ;; It’s important to note that additionalProperties only recognizes properties
  ;; declared in the same subschema as itself. So, additionalProperties can
  ;; restrict you from "extending" a schema using Schema Composition keywords
  ;; such as allOf. In the following example, we can see how the
  ;; additionalProperties can cause attempts to extend the address schema
  ;; example to fail.
  (let ((json-schema
          (cl-jschema:parse "{
                               \"allOf\": [
                                 {
                                   \"type\": \"object\",
                                   \"properties\": {
                                     \"street_address\": { \"type\": \"string\" },
                                     \"city\": { \"type\": \"string\" },
                                     \"state\": { \"type\": \"string\" }
                                   },
                                   \"required\": [\"street_address\", \"city\", \"state\"],
                                   \"additionalProperties\": false
                                 }
                               ],
                               \"properties\": {
                                 \"type\": { \"enum\": [ \"residential\", \"business\" ] }
                               },
                               \"required\": [\"type\"]
                             }")))
    (invalid-jsons json-schema
                   `(("{
                         \"street_address\": \"1600 Pennsylvania Avenue NW\",
                         \"city\": \"Washington\",
                         \"state\": \"DC\",
                         \"type\": \"business\"
                      }"
                      ;; NOTE: here the validation fails because
                      ;; 'additionalProperties' fails, because "type" is
                      ;; considered additional. Perhaps the error message could
                      ;; be clearer.
                      ,(expected-message "" "allOf"))
                     ("{
                         \"street_address\": \"1600 Pennsylvania Avenue NW\",
                         \"city\": \"Washington\",
                         \"state\": \"DC\"
                      }"
                      ,(expected-message "" "required" "type")))))
  ;; Because additionalProperties only recognizes properties declared in the
  ;; same subschema, it considers anything other than "street_address", "city",
  ;; and "state" to be additional. Combining the schemas with allOf doesn’t
  ;; change that. A workaround you can use is to move additionalProperties to
  ;; the extending schema and redeclare the properties from the extended schema.
  (let ((json-schema
          (cl-jschema:parse "{
                               \"allOf\": [
                                 {
                                   \"type\": \"object\",
                                   \"properties\": {
                                     \"street_address\": { \"type\": \"string\" },
                                     \"city\": { \"type\": \"string\" },
                                     \"state\": { \"type\": \"string\" }
                                   },
                                   \"required\": [\"street_address\", \"city\", \"state\"]
                                 }
                               ],
                               \"properties\": {
                                 \"street_address\": true,
                                 \"city\": true,
                                 \"state\": true,
                                 \"type\": { \"enum\": [ \"residential\", \"business\" ] }
                               },
                               \"required\": [\"type\"],
                               \"additionalProperties\": false
                             }")))
    (valid-json json-schema "{
                               \"street_address\": \"1600 Pennsylvania Avenue NW\",
                               \"city\": \"Washington\",
                               \"state\": \"DC\",
                               \"type\": \"business\"
                            }")
    (invalid-json json-schema "{
                                 \"street_address\": \"1600 Pennsylvania Avenue NW\",
                                 \"city\": \"Washington\",
                                 \"state\": \"DC\",
                                 \"type\": \"business\",
                                 \"something that doesn't belong\": \"hi!\"
                              }"
                  (expected-message "" "additionalProperties" "something that doesn't belong"))))


(cl-jschema-test :object-unevaluated-properties-test
  (let ((json-schema
          (cl-jschema:parse "{
                               \"allOf\": [
                                 {
                                   \"type\": \"object\",
                                   \"properties\": {
                                     \"street_address\": { \"type\": \"string\" },
                                     \"city\": { \"type\": \"string\" },
                                     \"state\": { \"type\": \"string\" }
                                   },
                                   \"required\": [\"street_address\", \"city\", \"state\"]
                                 }
                               ],
                               \"properties\": {
                                 \"type\": { \"enum\": [\"residential\", \"business\"] }
                               },
                               \"required\": [\"type\"],
                               \"unevaluatedProperties\": false
                             }")))
    (valid-json json-schema "{
                               \"street_address\": \"1600 Pennsylvania Avenue NW\",
                               \"city\": \"Washington\",
                               \"state\": \"DC\",
                               \"type\": \"business\"
                            }")
    (invalid-json json-schema "{
                                 \"street_address\": \"1600 Pennsylvania Avenue NW\",
                                 \"city\": \"Washington\",
                                 \"state\": \"DC\",
                                 \"type\": \"business\",
                                 \"something that doesn't belong\": \"hi!\"
                              }"
                  (expected-message "" "additionalProperties" "something that doesn't belong")))
  ;; Test conditionally adding properties. The following example allows the
  ;; "department" property only if the "type" of address is "business".
  (let ((json-schema
          (cl-jschema:parse "{
                               \"type\": \"object\",
                               \"properties\": {
                                 \"street_address\": { \"type\": \"string\" },
                                 \"city\": { \"type\": \"string\" },
                                 \"state\": { \"type\": \"string\" },
                                 \"type\": { \"enum\": [\"residential\", \"business\"] }
                               },
                               \"required\": [\"street_address\", \"city\", \"state\", \"type\"],
                               \"if\": {
                                 \"type\": \"object\",
                                 \"properties\": {
                                   \"type\": { \"const\": \"business\" }
                                 },
                                 \"required\": [\"type\"]
                               },
                               \"then\": {
                                 \"properties\": {
                                   \"department\": { \"type\": \"string\" }
                                 }
                               },
                               \"unevaluatedProperties\": false
                             }")))
    (valid-json json-schema "{
                               \"street_address\": \"1600 Pennsylvania Avenue NW\",
                               \"city\": \"Washington\",
                               \"state\": \"DC\",
                               \"type\": \"business\",
                               \"department\": \"HR\"
                             }")
    (invalid-json json-schema "{
                                 \"street_address\": \"1600 Pennsylvania Avenue NW\",
                                 \"city\": \"Washington\",
                                 \"state\": \"DC\",
                                 \"type\": \"residential\",
                                 \"department\": \"HR\"
                               }"
                  "JSON Schema validation found these errors:

\"\" : Property \"type\" is not allowed

\"\" : Property \"department\" is not allowed")))


(cl-jschema-test :object-required-properties-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"type\": \"object\",
                                          \"properties\": {
                                            \"name\": {\"type\": \"string\"},
                                            \"email\": {\"type\": \"string\"},
                                            \"address\": {\"type\": \"string\"},
                                            \"telephone\": {\"type\": \"string\"}
                                          },
                                          \"required\": [\"name\", \"email\"]
                                        }")))
    (valid-json json-schema
                "{\"name\": \"William Shakespeare\", \"email\": \"bill@stratford-upon-avon.co.uk\"}"
                "{
                   \"name\": \"William Shakespeare\",
                   \"email\": \"bill@stratford-upon-avon.co.uk\",
                   \"address\": \"Henley Street, Stratford-upon-Avon, Warwickshire, England\",
                   \"authorship\": \"in question\"
                 }")
    (invalid-jsons json-schema `(("{\"name\": \"William Shakespeare\", \"address\": \"Henley Street, Stratford-upon-Avon, Warwickshire, England\"}"
                                  ,(expected-message "" "required" "email"))
                                 ("{
                                     \"name\": \"William Shakespeare\",
                                     \"address\": \"Henley Street, Stratford-upon-Avon, Warwickshire, England\",
                                     \"email\": null
                                   }"
                                  ,(expected-message "/email" "type" "string"))))))


(cl-jschema-test :object-property-names-test
  (let* ((pattern "^[A-Za-z_][A-Za-z0-9_]*$")
         (json-schema
           (cl-jschema:parse (format nil "{
                                            \"type\": \"object\",
                                            \"propertyNames\": {
                                               \"pattern\": ~s
                                            }
                                          }"
                                     pattern))))
    (valid-json json-schema "{\"_a_proper_token_001\": \"value\"}")
    (invalid-json json-schema "{\"001 invalid\": \"value\"}"
                  ;; NOTE: I understand this is a valid JSON Pointer...
                  (expected-message "/001 invalid" "pattern" pattern))))


(cl-jschema-test :object-max/min-properties-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"type\": \"object\",
                                          \"minProperties\": 2,
                                          \"maxProperties\": 3
                                        }")))
    (valid-json json-schema
                "{\"a\": 0, \"b\": 1}"
                "{\"a\": 0, \"b\": 1, \"c\": 2}")
    (invalid-jsons json-schema `(("{}"
                                  ,(expected-message "" "minProperties" 2))
                                 ("{\"a\": 0}"
                                  ,(expected-message "" "minProperties" 2))
                                 ("{\"a\": 0, \"b\": 1, \"c\": 2, \"d\": 3}"
                                  ,(expected-message "" "maxProperties" 3))))))


(cl-jschema-test :basic-array-type-test
  (let ((json-schema (cl-jschema:parse "{\"type\": \"array\"}")))
    (valid-json json-schema
                "[1, 2, 3, 4, 5]"
                "[3, \"different\", {\"types\": \"of values\"}]")
    (invalid-json json-schema "{\"Not\": \"an array\"}"
                  (expected-message "" "type" "array"))))


(cl-jschema-test :string-is-not-array-test
  (invalid-json (cl-jschema:parse "{
                                     \"type\": \"object\",
                                     \"properties\": {
                                       \"foo\": {
                                         \"type\": \"array\"
                                       }
                                     }
                                   }")
                "{\"foo\": \"Not an array\"}"
                (expected-message "/foo" "type" "array")))


(cl-jschema-test :array-items-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"type\": \"array\",
                                          \"items\": {
                                            \"type\": \"number\"
                                          }
                                        }")))
    (valid-json json-schema "[]" "[1, 2, 3, 4, 5]")
    (invalid-json json-schema "[1, 2, \"3\", 4, 5]"
                  (expected-message "/2" "type" "number"))))


(cl-jschema-test :array-prefix-items-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"type\": \"array\",
                                          \"prefixItems\": [
                                            {\"type\": \"number\"},
                                            {\"type\": \"string\"},
                                            {\"enum\": [\"Street\", \"Avenue\", \"Boulevard\"]},
                                            {\"enum\": [\"NW\", \"NE\", \"SW\", \"SE\"]}
                                          ]
                                        }")))
    (valid-json json-schema
                "[1600, \"Pennsylvania\", \"Avenue\", \"NW\"]"
                "[10, \"Downing\", \"Street\"]"
                "[1600, \"Pennsylvania\", \"Avenue\", \"NW\", \"Washington\"]"
                "[]")
    (invalid-jsons json-schema `(("[24, \"Sussex\", \"Drive\"]"
                                  ,(expected-message "/2" "enum"))
                                 ("[\"Palais de l'Élysée\"]"
                                  ,(expected-message "/0" "type" "number"))))))


(cl-jschema-test :array-additional-items-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"type\": \"array\",
                                          \"prefixItems\": [
                                            {\"type\": \"number\"},
                                            {\"type\": \"string\"},
                                            {\"enum\": [\"Street\", \"Avenue\", \"Boulevard\"]},
                                            {\"enum\": [\"NW\", \"NE\", \"SW\", \"SE\"]}
                                          ],
                                          \"items\": false
                                        }")))
    (valid-json json-schema
                "[1600, \"Pennsylvania\", \"Avenue\", \"NW\"]"
                "[10, \"Downing\", \"Street\"]"
                "[]")
    (invalid-json json-schema "[1600, \"Pennsylvania\", \"Avenue\", \"NW\", \"Washington\"]"
                  (expected-message "" "items")))
  (let ((json-schema (cl-jschema:parse "{
                                          \"type\": \"array\",
                                          \"prefixItems\": [
                                            {\"type\": \"number\"},
                                            {\"type\": \"string\"},
                                            {\"enum\": [\"Street\", \"Avenue\", \"Boulevard\"]},
                                            {\"enum\": [\"NW\", \"NE\", \"SW\", \"SE\"]}
                                          ],
                                          \"items\": {\"type\": \"string\"}
                                        }")))
    (valid-json json-schema "[1600, \"Pennsylvania\", \"Avenue\", \"NW\", \"Washington\"]")
    (invalid-json json-schema "[1600, \"Pennsylvania\", \"Avenue\", \"NW\", 20500]"
                  (expected-message "/4" "type" "string"))))


(cl-jschema-test :array-contains-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"type\": \"array\",
                                          \"contains\": {
                                            \"type\": \"number\"
                                          }
                                        }")))
    (valid-json json-schema
                "[\"life\", \"universe\", \"everything\", 42]"
                "[1, 2, 3, 4, 5]")
    (invalid-json json-schema "[\"life\", \"universe\", \"everything\", \"forty-two\"]"
                  (expected-message "" "minContains" 1))))


(cl-jschema-test :array-max/min-contains-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"type\": \"array\",
                                          \"contains\": {
                                            \"type\": \"number\"
                                          },
                                          \"minContains\": 2,
                                          \"maxContains\": 3
                                        }")))
    (valid-json json-schema
                "[\"apple\", \"orange\", 2, 4]"
                "[\"apple\", \"orange\", 2, 4, 8]")
    (invalid-jsons json-schema
                   `(("[\"apple\", \"orange\", 2]"
                      ,(expected-message "" "minContains" 2))
                     ("[\"apple\", \"orange\", 2, 4, 8, 16]"
                      ,(expected-message "" "maxContains" 3))))))


(cl-jschema-test :array-max/min-items-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"type\": \"array\",
                                          \"minItems\": 2,
                                          \"maxItems\": 3
                                        }")))
    (valid-json json-schema "[1, 2]" "[1, 2, 3]")
    (invalid-jsons json-schema `(("[]"
                                  ,(expected-message "" "minItems" 2))
                                 ("[1]"
                                  ,(expected-message "" "minItems" 2))
                                 ("[1, 2, 3, 4]"
                                  ,(expected-message "" "maxItems" 3))))))


(cl-jschema-test :array-unique-items-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"type\": \"array\",
                                          \"uniqueItems\": true
                                        }")))
    (valid-json json-schema "[1]" "[1, 2, 3, 4, 5]")
    (invalid-json json-schema "[1, 2, 3, 3, 5]"
                  (expected-message "" "uniqueItems"))))


(cl-jschema-test :valid-json-schema-annotations-test
  (5am:finishes
    (cl-jschema:parse "{
                         \"title\": \"Match anything\",
                         \"description\": \"This is a schema that matches anything\",
                         \"default\": \"Default value\",
                         \"examples\": [\"Anything\", 4035],
                         \"deprecated\": true,
                         \"readOnly\": true,
                         \"writeOnly\": false,
                         \"$comment\": \"Hello! Yes, this is dog.\"
                       }")))


(cl-jschema-test :valid-string-encodings-test
  (5am:finishes
    (cl-jschema:parse "{
                         \"type\": \"string\",
                         \"contentEncoding\": \"base64\",
                         \"contentMediaType\": \"image/png\"
                       }")))


(cl-jschema-test :all-of-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"allOf\": [
                                            {\"type\": \"string\"},
                                            {\"maxLength\": 5}
                                          ]
                                        }")))
    (valid-value json-schema "short")
    (invalid-value json-schema "too long" (expected-message "" "allOf"))))


(cl-jschema-test :any-of-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"anyOf\": [
                                            {\"type\": \"string\", \"maxLength\": 5},
                                            {\"type\": \"number\", \"minimum\": 0}
                                          ]
                                        }")))
    (valid-value json-schema "short" 12)
    (invalid-values json-schema `(("too long" ,(expected-message "" "anyOf"))
                                  (-5         ,(expected-message "" "anyOf"))))))


(cl-jschema-test :one-of-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"oneOf\": [
                                            {\"type\": \"number\", \"multipleOf\": 5},
                                            {\"type\": \"number\", \"multipleOf\": 3}
                                          ]
                                        }"))
        (factored-schema (cl-jschema:parse "{
                                               \"type\": \"number\",
                                               \"oneOf\": [
                                                 {\"multipleOf\": 5},
                                                 {\"multipleOf\": 3}
                                               ]
                                             }")))
    (dolist (schema (list json-schema factored-schema))
      (valid-value schema 10 9)
      (invalid-values schema `((2  ,(expected-message "" "oneOf"))
                               (15 ,(expected-message "" "oneOf")))))))


(cl-jschema-test :not-test
  (let ((json-schema (cl-jschema:parse "{\"not\": {\"type\": \"string\"}}")))
    (valid-value json-schema 42 (jzon:parse "{\"key\": \"value\"}"))
    (invalid-value json-schema "I am a string" (expected-message "" "not"))))


(cl-jschema-test :illogical-schema-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"allOf\": [
                                            {\"type\": \"string\"},
                                            {\"type\": \"number\"}
                                          ]
                                        }")))
    (invalid-values json-schema `(("No way" ,(expected-message "" "allOf"))
                                  (-1       ,(expected-message "" "allOf"))))))


(cl-jschema-test :multiple-logicals-test
  (let ((json-schema (cl-jschema:parse "{
                                          \"anyOf\": [
                                            {\"type\": \"string\"},
                                            {\"type\": \"number\"}
                                          ],
                                          \"not\": {
                                            \"type\": \"number\",
                                            \"multipleOf\": 2
                                          }
                                        }")))
    (valid-value json-schema "A string" "" 1 3 5)
    (invalid-json json-schema "[1]" (expected-message "" "anyOf"))
    (invalid-value json-schema 4 (expected-message "" "not"))))


(cl-jschema-test :dependent-required-test
  (let ((unidirectional-schema
          (cl-jschema:parse "{
                               \"type\": \"object\",
                               \"properties\": {
                                 \"name\": { \"type\": \"string\" },
                                 \"credit_card\": { \"type\": \"number\" },
                                 \"billing_address\": { \"type\": \"string\" }
                               },
                               \"required\": [\"name\"],
                               \"dependentRequired\": {
                                 \"credit_card\": [\"billing_address\"]
                               }
                             }"))
        (bidirectional-schema
          (cl-jschema:parse "{
                               \"type\": \"object\",
                               \"properties\": {
                                 \"name\": { \"type\": \"string\" },
                                 \"credit_card\": { \"type\": \"number\" },
                                 \"billing_address\": { \"type\": \"string\" }
                               },
                               \"required\": [\"name\"],
                               \"dependentRequired\": {
                                 \"credit_card\": [\"billing_address\"],
                                 \"billing_address\": [\"credit_card\"]
                               }
                             }")))
    (let ((just-name "{ \"name\": \"John Doe\" }")
          (just-billing "{
                           \"name\": \"John Doe\",
                           \"billing_address\": \"555 Debtor's Lane\"
                         }")
          (just-credit "{
                          \"name\": \"John Doe\",
                          \"credit_card\": 5555555555555555
                        }")
          (credit-and-billing "{
                                 \"name\": \"John Doe\",
                                 \"credit_card\": 5555555555555555,
                                 \"billing_address\": \"555 Debtor's Lane\"
                               }"))
      (valid-json unidirectional-schema just-name just-billing credit-and-billing)
      (valid-json bidirectional-schema  just-name credit-and-billing)
      (invalid-json unidirectional-schema just-credit
                    (expected-message "" "required" "billing_address"))
      (invalid-jsons bidirectional-schema
                     `((,just-billing ,(expected-message "" "required" "credit_card"))
                       (,just-credit  ,(expected-message "" "required" "billing_address")))))))


(cl-jschema-test :dependent-schemas-test
  (let ((json-schema
          (cl-jschema:parse "{
                               \"type\": \"object\",
                               \"properties\": {
                                 \"name\": { \"type\": \"string\" },
                                 \"credit_card\": { \"type\": \"number\" }
                               },
                               \"required\": [\"name\"],
                               \"dependentSchemas\": {
                                 \"credit_card\": {
                                   \"properties\": {
                                     \"billing_address\": { \"type\": \"string\" }
                                   },
                                   \"required\": [\"billing_address\"]
                                 }
                               }
                             }")))
    (valid-json json-schema
                "{
                   \"name\": \"John Doe\",
                   \"credit_card\": 5555555555555555,
                   \"billing_address\": \"555 Debtor's Lane\"
                }"
                "{
                   \"name\": \"John Doe\",
                   \"billing_address\": \"555 Debtor's Lane\"
                 }")
    (invalid-json json-schema "{
                                 \"name\": \"John Doe\",
                                 \"credit_card\": 5555555555555555
                               }"
                  (expected-message "" "required" "billing_address"))))


(cl-jschema-test :if-then-else-test
  (let ((json-schema
          (cl-jschema:parse
           "{
              \"type\": \"object\",
              \"properties\": {
                \"street_address\": {
                  \"type\": \"string\"
                },
                \"country\": {
                  \"default\": \"United States of America\",
                  \"enum\": [\"United States of America\", \"Canada\"]
                }
              },
              \"if\": {
                \"properties\": { \"country\": { \"const\": \"United States of America\" } }
              },
              \"then\": {
                \"properties\": { \"postal_code\": { \"pattern\": \"[0-9]{5}(-[0-9]{4})?\" } }
              },
              \"else\": {
                \"properties\": { \"postal_code\": { \"pattern\": \"[A-Z][0-9][A-Z] [0-9][A-Z][0-9]\" } }
              }
            }")))
    (valid-json json-schema
                "{
                   \"street_address\": \"1600 Pennsylvania Avenue NW\",
                   \"country\": \"United States of America\",
                   \"postal_code\": \"20500\"
                 }"
                "{
                   \"street_address\": \"1600 Pennsylvania Avenue NW\",
                   \"postal_code\": \"20500\"
                 }"
                "{
                   \"street_address\": \"24 Sussex Drive\",
                   \"country\": \"Canada\",
                   \"postal_code\": \"K1M 1M4\"
                 }")
    (invalid-jsons json-schema
                   `(("{
                         \"street_address\": \"24 Sussex Drive\",
                         \"country\": \"Canada\",
                         \"postal_code\": \"10000\"
                       }"
                      ,(expected-message "/postal_code" "pattern" "[A-Z][0-9][A-Z] [0-9][A-Z][0-9]"))
                     ("{
                         \"street_address\": \"1600 Pennsylvania Avenue NW\",
                         \"postal_code\": \"K1M 1M4\"
                       }"
                      ,(expected-message "/postal_code" "pattern" "[0-9]{5}(-[0-9]{4})?"))))))


(cl-jschema-test :if-then-else-test-2
  (let ((json-schema
          (cl-jschema:parse
           "{
              \"type\": \"object\",
              \"properties\": {
                \"street_address\": {
                  \"type\": \"string\"
                },
                \"country\": {
                  \"default\": \"United States of America\",
                  \"enum\": [\"United States of America\", \"Canada\", \"Netherlands\"]
                }
              },
              \"allOf\": [
                {
                  \"if\": {
                    \"properties\": { \"country\": { \"const\": \"United States of America\" } }
                  },
                  \"then\": {
                    \"properties\": { \"postal_code\": { \"pattern\": \"[0-9]{5}(-[0-9]{4})?\" } }
                  }
                },
                {
                  \"if\": {
                    \"properties\": { \"country\": { \"const\": \"Canada\" } },
                    \"required\": [\"country\"]
                  },
                  \"then\": {
                    \"properties\": { \"postal_code\": { \"pattern\": \"[A-Z][0-9][A-Z] [0-9][A-Z][0-9]\" } }
                  }
                },
                {
                  \"if\": {
                    \"properties\": { \"country\": { \"const\": \"Netherlands\" } },
                    \"required\": [\"country\"]
                  },
                  \"then\": {
                    \"properties\": { \"postal_code\": { \"pattern\": \"[0-9]{4} [A-Z]{2}\" } }
                  }
                }
              ]
            }")))
    (valid-json json-schema
                "{
                   \"street_address\": \"1600 Pennsylvania Avenue NW\",
                   \"country\": \"United States of America\",
                   \"postal_code\": \"20500\"
                 }"
                "{
                   \"street_address\": \"1600 Pennsylvania Avenue NW\",
                   \"postal_code\": \"20500\"
                 }"
                "{
                   \"street_address\": \"24 Sussex Drive\",
                   \"country\": \"Canada\",
                   \"postal_code\": \"K1M 1M4\"
                 }"
                "{
                   \"street_address\": \"Adriaan Goekooplaan\",
                   \"country\": \"Netherlands\",
                   \"postal_code\": \"2517 JX\"
                 }")
    (invalid-jsons json-schema
                   `(("{
                         \"street_address\": \"24 Sussex Drive\",
                         \"country\": \"Canada\",
                         \"postal_code\": \"10000\"
                       }"
                      ,(expected-message "" "allOf"))
                     ("{
                         \"street_address\": \"1600 Pennsylvania Avenue NW\",
                         \"postal_code\": \"K1M 1M4\"
                       }"
                      ,(expected-message "" "allOf"))))))


(cl-jschema-test :algebra-implication-test
  ;;  `A -> B` (pronounced "A implies B") means that if A is true, then B must
  ;;  also be true. It can be expressed as `!A || B` which can be expressed as a
  ;;  JSON Schema.
  (let ((json-schema
          (cl-jschema:parse
           "{
              \"type\": \"object\",
              \"properties\": {
                \"restaurantType\": { \"enum\": [\"fast-food\", \"sit-down\"] },
                \"total\": { \"type\": \"number\" },
                \"tip\": { \"type\": \"number\" }
              },
              \"anyOf\": [
                {
                  \"not\": {
                    \"properties\": { \"restaurantType\": { \"const\": \"sit-down\" } },
                    \"required\": [\"restaurantType\"]
                  }
                },
                { \"required\": [\"tip\"] }
              ]
            }")))
    (valid-json json-schema
                "{
                   \"restaurantType\": \"sit-down\",
                   \"total\": 16.99,
                   \"tip\": 3.4
                 }"
                "{
                   \"restaurantType\": \"fast-food\",
                   \"total\": 6.99
                 }"
                "{ \"total\": 5.25 }")
    (invalid-json json-schema "{
                                 \"restaurantType\": \"sit-down\",
                                 \"total\": 16.99
                               }"
                  (expected-message "" "anyOf"))))


(cl-jschema-test :valid-metadata-keywords-test
  (dolist (json '("{
                     \"type\": \"number\",
                     \"units\": \"kg\"
                   }"
                  "{
                     \"type\": \"integer\",
                     \"isEven\": true
                   }"
                  "{
                     \"type\": \"object\",
                     \"requiredProperties\": {
                       \"foo\": { \"type\": \"string\" }
                     }
                   }"))
    (5am:finishes (cl-jschema:parse json))))


(cl-jschema-test :valid-anchor-test
  (let (schema)
    (5am:finishes
      (setq schema
            (cl-jschema:parse "{
                                 \"$id\": \"https://example.com/schemas/address\",
                                 \"type\": \"object\",
                                 \"properties\": {
                                   \"street_address\":
                                     {
                                       \"$anchor\": \"street_address\",
                                       \"type\": \"string\"
                                     },
                                   \"city\": { \"type\": \"string\" },
                                   \"state\": { \"type\": \"string\" }
                                 },
                                 \"required\": [\"street_address\", \"city\", \"state\"]
                               }")))
    (5am:is (eq (cl-jschema:get-schema "https://example.com/schemas/address#street_address")
                ;; The path to the inner schema object. I hope this doesn't have
                ;; to be updated often...
                (gethash "street_address"
                         (cl-jschema::get-type-property
                          (cl-jschema::type-schema
                           (cl-jschema::schema-spec schema))
                          "properties"))))))


(cl-jschema-test :ref-test
  (let ((schema-address
          (cl-jschema:parse "{
                               \"$id\": \"https://example.com/schemas/address\",
                               \"type\": \"object\",
                               \"properties\": {
                                 \"street_address\": { \"type\": \"string\" },
                                 \"city\": { \"type\": \"string\" },
                                 \"state\": { \"type\": \"string\" }
                               },
                               \"required\": [\"street_address\", \"city\", \"state\"]
                             }"))
        schema-customer)
    (5am:finishes
      (setq schema-customer
            (cl-jschema:parse "{
                                 \"$id\": \"https://example.com/schemas/customer\",
                                 \"type\": \"object\",
                                 \"properties\": {
                                   \"first_name\": { \"type\": \"string\" },
                                   \"last_name\": { \"type\": \"string\" },
                                   \"shipping_address\": { \"$ref\": \"/schemas/address\" },
                                   \"billing_address\": { \"$ref\": \"/schemas/address\" }
                                 },
                                 \"required\": [\"first_name\", \"last_name\", \"shipping_address\", \"billing_address\"]
                               }")))
    (5am:is (eq (cl-jschema:get-schema "https://example.com/schemas/address")
                schema-address))
    (5am:is (eq (cl-jschema:get-schema "https://example.com/schemas/customer")
                schema-customer))
    (valid-json schema-customer "{
                                   \"first_name\": \"Willy\",
                                   \"last_name\": \"Wonka\",
                                   \"shipping_address\": {\"street_address\": \"St. Willy\", \"city\": \"Wonka City\", \"state\": \"Wonk\"},
                                   \"billing_address\": {\"street_address\": \"St. Willy\", \"city\": \"Wonka City\", \"state\": \"Wonk\"}
                                 }")
    (invalid-json schema-customer "{
                                     \"first_name\": \"Willy\",
                                     \"last_name\": \"Wonka\",
                                     \"shipping_address\": {\"street_address\": 42, \"city\": \"Wonka City\", \"state\": \"Wonk\"},
                                     \"billing_address\": {\"street_address\": \"St. Willy\", \"city\": 42, \"state\": \"Wonk\"}
                                   }"
                  "JSON Schema validation found these errors:

\"/shipping_address/street_address\" : Not of type \"string\"

\"/billing_address/city\" : Not of type \"string\"")))


(cl-jschema-test :ref-anonymous-schema-test
  (let ((schema-address
          (cl-jschema:parse "{
                               \"$id\": \"https://example.com/schemas/address\",
                               \"type\": \"object\",
                               \"properties\": {
                                 \"street_address\": { \"type\": \"string\" },
                                 \"city\": { \"type\": \"string\" },
                                 \"state\": { \"type\": \"string\" }
                               },
                               \"required\": [\"street_address\", \"city\", \"state\"]
                             }"))
        schema-customer)
    (5am:finishes
      (setq schema-customer
            (cl-jschema:parse "{
                                 \"type\": \"object\",
                                 \"properties\": {
                                   \"first_name\": { \"type\": \"string\" },
                                   \"last_name\": { \"type\": \"string\" },
                                   \"shipping_address\": { \"$ref\": \"https://example.com/schemas/address\" },
                                   \"billing_address\": { \"$ref\": \"/schemas/address\" }
                                 },
                                 \"required\": [\"first_name\", \"last_name\", \"shipping_address\", \"billing_address\"]
                               }")))
    (5am:is (eq (cl-jschema:get-schema "https://example.com/schemas/address")
                schema-address))
    ;; The schema has no root $id, so it's not registered
    (5am:is (null (cl-jschema:get-schema "https://example.com/schemas/customer")))
    ;; Since the billing_address $ref can't be resolved, then billing_address
    ;; can't be validated.
    (valid-json schema-customer
                "{
                   \"first_name\": \"Willy\",
                   \"last_name\": \"Wonka\",
                   \"shipping_address\": {\"street_address\": \"St. Willy\", \"city\": \"Wonka City\", \"state\": \"Wonk\"},
                   \"billing_address\": {\"street_address\": \"St. Willy\", \"city\": \"Wonka City\", \"state\": \"Wonk\"}
                 }"
                "{
                   \"first_name\": \"Willy\",
                   \"last_name\": \"Wonka\",
                   \"shipping_address\": {\"street_address\": \"St. Willy\", \"city\": \"Wonka City\", \"state\": \"Wonk\"},
                   \"billing_address\": 42
                 }")
    (invalid-json schema-customer "{
                                     \"first_name\": \"Willy\",
                                     \"last_name\": \"Wonka\",
                                     \"shipping_address\": {\"street_address\": 42, \"city\": \"Wonka City\", \"state\": \"Wonk\"},
                                     \"billing_address\": {\"street_address\": \"St. Willy\", \"city\": 42, \"state\": \"Wonk\"}
                                   }"
                  (expected-message "/shipping_address/street_address" "type" "string"))))


(cl-jschema-test :defs-test
  (let (schema-customer)
    (5am:finishes
      (setq schema-customer
            (cl-jschema:parse "{
                                 \"$id\": \"https://example.com/schemas/customer\",
                                 \"type\": \"object\",
                                 \"properties\": {
                                   \"first_name\": { \"$ref\": \"#/$defs/name\" },
                                   \"last_name\": { \"$ref\": \"#/$defs/unknown\" }
                                 },
                                 \"$defs\": {
                                   \"name\": { \"type\": \"string\" }
                                 }
                               }")))
    (5am:is (eq (cl-jschema:get-schema "https://example.com#/$defs/name")
                ;; The path to the inner schema object
                (gethash "name" (cl-jschema::defs schema-customer))))
    ;; The $ref for last_name can't be resolved, so last_name is not validated
    (valid-json schema-customer
                "{\"first_name\": \"Willy\", \"last_name\": \"Wonka\"}"
                "{\"first_name\": \"Willy\", \"last_name\": 42}")
    ;;
    (invalid-json schema-customer "{\"first_name\": 42, \"last_name\": 42}"
                  (expected-message "/first_name" "type" "string"))))


(cl-jschema-test :defs-anonymous-schema-test
  (let (schema-customer)
    (5am:finishes
      (setq schema-customer
            (cl-jschema:parse "{
                                 \"type\": \"object\",
                                 \"properties\": {
                                   \"first_name\": { \"$ref\": \"#/$defs/name\" },
                                   \"last_name\": { \"$ref\": \"#/$defs/unknown\" }
                                 },
                                 \"$defs\": {
                                   \"name\": { \"type\": \"string\" }
                                 }
                               }")))
    ;; The schema had no root $id, so nothing was registered
    (5am:is (null (cl-jschema:get-schema "https://example.com#/$defs/name")))
    ;; Because nothing was registered, the properties aren't validated
    (valid-json schema-customer
                "{\"first_name\": \"Willy\", \"last_name\": \"Wonka\"}"
                "{\"first_name\": 42, \"last_name\": 42}")))


(cl-jschema-test :invalid-schemas-test
  (dolist (data '(("[]"
                   "JSON Schema must be a JSON boolean or object")
                  ("{\"type\":\"jorl\"}"
                   "Type \"jorl\" is not allowed")
                  ("{\"type\":[\"string\",\"jorl\"]}"
                   "Type \"jorl\" is not allowed")
                  ("{\"$schema\":42}"
                   "Keyword $schema expects a string")
                  ("{\"$id\":42}"
                   "Keyword $id expects a URI without a fragment")
                  ("{\"$id\":\"\"}"
                   "Keyword $id expects a URI without a fragment")
                  ("{\"$id\":\"https://example.com/schemas/address#foo\"}"
                   "Keyword $id expects a URI without a fragment")
                  ("{\"$anchor\":42}"
                   "Keyword $anchor expects a string")
                  ("{\"$ref\":42}"
                   "Keyword $ref expects a URI")
                  ("{\"$ref\":\"\"}"
                   "Keyword $ref expects a URI")
                  ("{\"$defs\":42}"
                   "Keyword $defs expects a JSON object with schema values")
                  ("{\"type\":42}"
                   "The value for \"type\" must be a string or an array")
                  ("{\"minLength\":\"2\"}"
                   "Keyword minLength expects a non-negative integer")
                  ("{\"minLength\":-1}"
                   "Keyword minLength expects a non-negative integer")
                  ("{\"maxLength\":\"3\"}"
                   "Keyword maxLength expects a non-negative integer")
                  ("{\"maxLength\":-1}"
                   "Keyword maxLength expects a non-negative integer")
                  ("{\"pattern\":42}"
                   "Keyword pattern expects a regular expression")
                  ("{\"format\":42}"
                   "Keyword format expects a string")
                  ("{\"multipleOf\":0}"
                   "Keyword multipleOf expects a positive integer")
                  ("{\"minimum\":\"42\"}"
                   "Keyword minimum expects a number")
                  ("{\"exclusiveMinimum\":\"42\"}"
                   "Keyword exclusiveMinimum expects a number")
                  ("{\"maximum\":\"42\"}"
                   "Keyword maximum expects a number")
                  ("{\"exclusiveMaximum\":\"42\"}"
                   "Keyword exclusiveMaximum expects a number")
                  ("{\"enum\":42}"
                   "Keyword enum expects a non-empty JSON array")
                  ("{\"enum\":[]}"
                   "Keyword enum expects a non-empty JSON array")
                  ("{\"properties\":42}"
                   "Keyword properties expects a JSON object with schema values")
                  ("{\"patternProperties\":42}"
                   "Keyword patternProperties expects a JSON object with schema values")
                  ("{\"additionalProperties\":42}"
                   "Keyword additionalProperties expects a schema")
                  ("{\"unevaluatedProperties\":42}"
                   "Keyword unevaluatedProperties expects a schema")
                  ("{\"required\":42}"
                   "Keyword required expects a JSON array of zero or more strings")
                  ("{\"required\":[42]}"
                   "Keyword required expects a JSON array of zero or more strings")
                  ("{\"propertyNames\":42}"
                   "Keyword propertyNames expects a schema")
                  ("{\"minProperties\":\"2\"}"
                   "Keyword minProperties expects a non-negative integer")
                  ("{\"minProperties\":-1}"
                   "Keyword minProperties expects a non-negative integer")
                  ("{\"maxProperties\":\"3\"}"
                   "Keyword maxProperties expects a non-negative integer")
                  ("{\"maxProperties\":-1}"
                   "Keyword maxProperties expects a non-negative integer")
                  ("{\"items\": 42}"
                   "Keyword items expects a schema")
                  ("{\"prefixItems\":42}"
                   "Keyword prefixItems expects a non-empty JSON array of schemas")
                  ("{\"prefixItems\":[42]}"
                   "Keyword prefixItems expects a non-empty JSON array of schemas")
                  ("{\"minContains\":\"2\"}"
                   "Keyword minContains expects a non-negative integer")
                  ("{\"minContains\":-1}"
                   "Keyword minContains expects a non-negative integer")
                  ("{\"maxContains\":\"3\"}"
                   "Keyword maxContains expects a non-negative integer")
                  ("{\"maxContains\":-1}"
                   "Keyword maxContains expects a non-negative integer")
                  ("{\"minItems\":\"2\"}"
                   "Keyword minItems expects a non-negative integer")
                  ("{\"minItems\":-1}"
                   "Keyword minItems expects a non-negative integer")
                  ("{\"maxItems\":\"3\"}"
                   "Keyword maxItems expects a non-negative integer")
                  ("{\"maxItems\":-1}"
                   "Keyword maxItems expects a non-negative integer")
                  ("{\"uniqueItems\":42}"
                   "Keyword uniqueItems expects a boolean")
                  ("{\"title\":42}"
                   "Keyword title expects a string")
                  ("{\"description\":42}"
                   "Keyword description expects a string")
                  ("{\"examples\":42}"
                   "Keyword examples expects a JSON array")
                  ;; A string should not be considered a JSON array
                  ("{\"examples\":\"hello\"}"
                   "Keyword examples expects a JSON array")
                  ("{\"readOnly\":42}"
                   "Keyword readOnly expects a boolean")
                  ("{\"writeOnly\":42}"
                   "Keyword writeOnly expects a boolean")
                  ("{\"deprecated\":42}"
                   "Keyword deprecated expects a boolean")
                  ("{\"$comment\":42}"
                   "Keyword $comment expects a string")
                  ("{\"contentEncoding\":42}"
                   "Keyword contentEncoding expects a string")
                  ("{\"contentMediaType\":42}"
                   "Keyword contentMediaType expects a string")
                  ("{\"minLength\":42, \"minimum\":42}"
                   "Keywords minLength and minimum refer to different types")
                  ("{\"type\":\"string\", \"minItems\":42}"
                   "Keyword minItems is for type array, not for type string")
                  ("{\"allOf\":42}"
                   "Keyword allOf expects a non-empty JSON array of schemas")
                  ("{\"allOf\":[]}"
                   "Keyword allOf expects a non-empty JSON array of schemas")
                  ("{\"anyOf\":42}"
                   "Keyword anyOf expects a non-empty JSON array of schemas")
                  ("{\"anyOf\":[]}"
                   "Keyword anyOf expects a non-empty JSON array of schemas")
                  ("{\"oneOf\":42}"
                   "Keyword oneOf expects a non-empty JSON array of schemas")
                  ("{\"oneOf\":[]}"
                   "Keyword oneOf expects a non-empty JSON array of schemas")
                  ("{\"not\":42}"
                   "Keyword not expects a schema")
                  ("{\"dependentRequired\":42}"
                   "Keyword dependentRequired expects a JSON object with JSON array of strings values")
                  ("{\"dependentRequired\":[]}"
                   "Keyword dependentRequired expects a JSON object with JSON array of strings values")
                  ("{\"dependentRequired\":{\"key\":42}}"
                   "Keyword dependentRequired expects a JSON object with JSON array of strings values")
                  ("{\"dependentRequired\":{\"key\":[{}]}}"
                   "Keyword dependentRequired expects a JSON object with JSON array of strings values")
                  ("{\"dependentSchemas\":42}"
                   "Keyword dependentSchemas expects a JSON object with schema values")
                  ("{\"dependentSchemas\":{\"key\":[]}}"
                   "Keyword dependentSchemas expects a JSON object with schema values")
                  ("{\"if\":42}"
                   "Keyword if expects a schema")
                  ("{\"then\":42}"
                   "Keyword then expects a schema")
                  ("{\"else\":42}"
                   "Keyword else expects a schema")))
    (destructuring-bind (json-schema error-message) data
      (signals cl-jschema:invalid-schema error-message
        (cl-jschema:parse json-schema)))))


(cl-jschema-test :unparsable-json-test
  (dolist (bad-json '(""
                      "["
                      "{"))
    (5am:signals cl-jschema:unparsable-json
        (cl-jschema:parse bad-json))))
