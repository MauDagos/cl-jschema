# CL-JSCHEMA

Common Lisp implementation of [JSON Schema](https://json-schema.org/).

### Index

* [Setup](#setup)
* [Limitations](#limitations)
* [User guide](#user-guide)
  * [Entrypoints](#entrypoints)
  * [Classes](#classes)
  * [Conditions](#conditions)
* [Example](#example)
* [Roadmap](#setup)
* [Dependencies](#dependencies)

## Setup

`CL-JSCHEMA` is not yet in Quicklisp or Ultralisp (but it's in the
[roadmap](#roadmap)!).

In the meantime you can clone this repository and load the system `:cl-jschema`
with ASDF.

## Limitations

* Currently only supports JSON Schema draft 2020-12.
* Currently only supports validating values which look like they were parsed by
  `COM.INUOE.JZON:PARSE`.
* Does not support fetching JSON Schemas from the web.
* The base URI is only considered with the `$id` at the root of the JSON Schema
  document. If you fetch a JSON Schema from the web and it does not include an
  `$id` at the root, then the JSON Schema will not be registered in the
  `CL-JSCHEMA` **registry**.

## User guide

### Entrypoints

#### `CL-JSCHEMA:PARSE`

Parse a JSON Schema. Returns an instance of `CL-JSCHEMA:JSON-SCHEMA` or throws a
condition of type `CL-JSCHEMA:INVALID-SCHEMA`.

Allows parsing a string or a `CL:STREAM`. You may also supply it a previously
parsed JSON that was parsed with `COM.INUOE.JZON:PARSE`. In any other case, a
condition of type `CL:ERROR` is thrown.

Keyword arguments:

* `ALLOW-COMMENTS`: defaults to `NIL`. Can be set to non-`NIL` to allow comments
  (`//`).
* `ALLOW-TRAILING-COMMA` defaults to `NIL`. Can be set to non-`NIL` to allow
  trailing commas.

Upon successfully parsing and if the JSON Schema contained a base URI (an `$id`
at the root of the JSON Schema document), then the root JSON Schema and any
[bundled JSON Schema
Resources](https://json-schema.org/understanding-json-schema/structuring.html#bundling)
are registered in the `CL-JSCHEMA` **registry**.

#### `CL-JSCHEMA:VALIDATE`

Validate a value with an instance of `CL-JSCHEMA:JSON-SCHEMA`. Returns `T` or
throws a condition of type `CL-JSCHEMA:INVALID-JSON`.

Currently only supports validating values which look like they have been
previously parsed by `COM.INUOE.JZON:PARSE`.

Keyword arguments:

* `IGNORE-UNRESOLVABLE-REFS`: defaults to `NIL`, meaning a value meant to be
  validated by a `$ref` will be considered invalid if no JSON Schema is found
  when resolving `$ref`. Can be set to non-`NIL` to consider values as valid
  when `$ref` is not resolvable.

If a condition of type `CL-JSCHEMA:INVALID-JSON` is thrown, then it's possible
to access all of the validation errors found throughout the validation by using
`CL-JSCHEMA:INVALID-JSON-ERRORS`.

#### `CL-JSCHEMA:CLEAR-REGISTRY`

Clear the `CL-JSCHEMA` **registry** of parsed JSON Schemas.

#### `CL-JSCHEMA:GET-SCHEMA`

Find an instance of `CL-JSCHEMA:JSON-SCHEMA` given a URI as a string or as an
instance of `PURI:URI`. Can return `NIL`.

If the URI contains a fragment, then it will be used to find a JSON Schema
inside the JSON Schema found by the URI without the fragment. It's assumed that
the fragment is a [JSON Pointer](https://www.rfc-editor.org/rfc/rfc6901) or the
name of an `$anchor`.

### Classes

#### `CL-JSCHEMA:JSON-SCHEMA`

A class representing a parsed JSON Schema. Use `CL-JSCHEMA:PARSE` to create a
new one or `CL-JSCHEMA:GET-SCHEMA` to find a previously parsed one.

### Conditions

#### `CL-JSCHEMA:INVALID-SCHEMA`

Conditions of this type are thrown when using `CL-JSCHEMA:PARSE`. This condition
indicates that the JSON Schema being parsed is invalid.

Information about this condition can be accessed with:

* `CL-JSCHEMA:INVALID-SCHEMA-ERROR-MESSAGE`: the reason why the condition was
  thrown. Returns a string.
* `CL-JSCHEMA:INVALID-SCHEMA-BASE-URI`: the base URI, if any, of the JSON Schema
  being validated when the condition was thrown. Returns `NIL` or an instance of
  `PURI:URI`.
* `CL-JSCHEMA:INVALID-SCHEMA-JSON-POINTER`: the [JSON
  Pointer](https://www.rfc-editor.org/rfc/rfc6901) to the value being validated
  in the JSON Schema when the condition was thrown. The JSON Pointer is relative
  to the base URI. Returns a string.

#### `CL-JSCHEMA:UNPARSABLE-JSON`

Conditions of this type are thrown when using `CL-JSCHEMA:PARSE`. This condition
indicates that an error was encountered when trying to parse the JSON Schema
JSON.

This condition is a subtype of `CL-JSCHEMA:INVALID-SCHEMA`, meaning the same
information can be accessed with the same accessors. Additionally:

*  `CL-JSCHEMA:UNPARSABLE-JSON-ERROR`: the condition caught when trying to parse
   the JSON Schema JSON. Returns an instance of a condition of type
   `COM.INUOE.JZON:JSON-ERROR`.

#### `CL-JSCHEMA:NOT-IMPLEMENTED`

Conditions of this type are thrown when using `CL-JSCHEMA:PARSE`. This condition
indicates that the JSON Schema is trying to use some property which is not
implemented by `CL-JSCHEMA`.

This condition is a subtype of `CL-JSCHEMA:INVALID-SCHEMA`, meaning the same
information can be accessed with the same accessors.

#### `CL-JSCHEMA:INVALID-JSON`

Conditions of this type are thrown when using `CL-JSCHEMA:VALIDATE`. This
condition indicates that the value supplied for validation is invalid.

Information about this condition can be accessed with:

* `CL-JSCHEMA:INVALID-JSON-ERRORS`: a list of all conditions of type
  `CL-JSCHEMA:INVALID-JSON-VALUE` caught while validating the value supplied for
  validation.

#### `CL-JSCHEMA:INVALID-JSON-VALUE`

Conditions of this type are thrown yet handled internally when using
`CL-JSCHEMA:VALIDATE`. This condition indicates at a particular value inside the
supplied value for validation is invalid.

* `CL-JSCHEMA:INVALID-JSON-VALUE-ERROR-MESSAGE`: the reason why the condition
  was thrown. Returns a string.
* `CL-JSCHEMA:INVALID-JSON-VALUE-JSON-POINTER`: the [JSON
  Pointer](https://www.rfc-editor.org/rfc/rfc6901) to the value being validated
  when the condition was thrown.

## Example

``` common-lisp
CL-USER> (let ((json-schema
                 (cl-jschema:parse "{
                                      \"properties\": {
                                        \"number\": {
                                          \"type\": \"number\",
                                          \"exclusiveMinimum\": -1,
                                          \"minimum\": 0
                                        }
                                      }
                                    }"))
               (value (com.inuoe.jzon:parse "{\"number\": -1}")))
           (handler-case
               (cl-jschema:validate json-schema value)
             (cl-jschema:invalid-json (e)
               (dolist (value-error (cl-jschema:invalid-json-errors e))
                 (format t "JSON Pointer: ~s~%~
                            Error: ~a~2%"
                         (cl-jschema:invalid-json-value-json-pointer value-error)
                         (cl-jschema:invalid-json-value-error-message value-error))))))
JSON Pointer: "/number"
Error: Number is less than 0

JSON Pointer: "/number"
Error: Number is less or equal to -1

NIL
```

**Pro tip**: check out
[CL-JSON-POINTER](https://github.com/y2q-actionman/cl-json-pointer) for using
the JSON Pointers created by `CL-JSCHEMA`.

## Roadmap

* Publish in Quicklisp.
* Publish in Ultralisp.
* Add GitHub workflow for running `FIVEAM` test suite.
* Include [JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite).
* Support for validating values parsed by [other Common Lisp JSON
  libraries](https://sabracrolleton.github.io/json-review.html#libraries).
* Support for previous [JSON Schema
  drafts](https://json-schema.org/specification-links.html#published-drafts).

## Dependencies

* [com.inuoe.jzon](https://github.com/Zulu-Inuoe/jzon)
* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* [cl-ppcre](https://github.com/edicl/cl-ppcre/)
* [puri](https://gitlab.common-lisp.net/clpm/puri)
