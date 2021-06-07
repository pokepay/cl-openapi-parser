# cl-openapi-parser
OpenAPI 3.0.1 and 3.1.0 parser/validator

## Usage

```common-lisp
(ql:quickload :openapi-parser)

(openapi-parser:parse-file "examples/api-with-examples.yaml")
; => #<OPENAPI-PARSER/SCHEMA/3.0.1:<OPEN-API> {10036FFBC3}>

(openapi-parser:parse-file "examples/error.yaml")

; Path: paths./.get.responses.200
; Line: 11
; Expected type: #<SCHEMA-METACLASS OPENAPI-PARSER/SCHEMA/3.0.1:<RESPONSE>>
; Missing field key 'description'
```

## License
MIT
