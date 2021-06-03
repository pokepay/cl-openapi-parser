(defpackage :openapi-parser
  (:use :cl :alexandria)
  (:export :parse-file))

(defpackage :openapi-parser/schema
  (:use :cl :alexandria))

(defpackage :openapi-parser/schema/3.0.1
  (:use :cl :alexandria))

(defpackage :openapi-parser/schema/3.1.0
  (:use :cl :alexandria))

(cl-package-locks:lock-package :openapi-parser)
(cl-package-locks:lock-package :openapi-parser/schema)
(cl-package-locks:lock-package :openapi-parser/schema/3.0.1)
(cl-package-locks:lock-package :openapi-parser/schema/3.1.0)
