(defsystem "openapi-parser"
  :license "MIT"
  :author "pokepay developer team"
  :description "OpenAPI 3.0.1 and 3.1.0 parser/validator"
  :depends-on ("cl-yaml"
               "alexandria"
               "cl-change-case"
               "trivial-types"
               "closer-mop"
               "esrap"
               "trivia"
               "str"
               "cl-package-locks")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "path")
               (:file "error")
               (:file "schema-interface")
               (:file "schema-base")
               (:file "schema-generator")
               (:file "schema-3-0-1")
               (:file "schema-3-1-0")
               (:file "yaml")
               (:file "parser"))
  :in-order-to ((test-op (test-op "openapi-parser-tests"))))

(defsystem "openapi-parser-tests"
  :depends-on ("openapi-parser"
               "rove")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "parser"))
  :perform (test-op (o c) (symbol-call :rove :run c)))
