# CL-JSCHEMA

Common Lisp implementation of [JSON Schema](https://json-schema.org/).

Currently only supporting draft 2020-12.

### Index

* [Setup](#setup)
* [User guide](#user-guide)
* [Roadmap](#setup)
* [Dependencies](#dependencies)

## Setup

`cl-jschema` is not yet in Quicklisp or Ultralisp (but it's in the roadmap!).

In the meantime you can clone this repository and load the system `:cl-jschema`
with ASDF.

## User guide

### Entrypoints

#### `cl-jschema:parse`

Parse a JSON Schema. Allows parsing a string or a `CL:STREAM`. You may also
supply it a previously parsed JSON that was parsed with `COM.INUOE.JZON:PARSE`.

Keyargs `ALLOW-COMMENTS` and `ALLOW-TRAILING-COMMA` default to `NIL` but can be
set to non-`NIL` to allow comments (`//`) or trailing commas, respectively.

#### `cl-jschema:validate`
#### `cl-jschema:clear-registry`
#### `cl-jschema:get-schema`

### Classes

#### `cl-jschema:json-schema`

### Conditions

#### `cl-jschema:invalid-schema`
#### `cl-jschema:invalid-schema-error-message`
#### `cl-jschema:invalid-schema-json-pointer`
#### `cl-jschema:unparsable-json`
#### `cl-jschema:unparsable-json-error`
#### `cl-jschema:not-implemented`
#### `cl-jschema:invalid-json`
#### `cl-jschema:invalid-json-error-message`
#### `cl-jschema:invalid-json-json-pointer`
#### `cl-jschema:invalid-json-errors`

## Roadmap

* Publish in Quicklisp.
* Publish in Ultralisp.
* Add GitHub workflow for running test suite.
* Include [JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite).

## Dependencies

* [com.inuoe.jzon](https://github.com/Zulu-Inuoe/jzon)
* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* [cl-ppcre](https://github.com/edicl/cl-ppcre/)
* [puri](https://gitlab.common-lisp.net/clpm/puri)
