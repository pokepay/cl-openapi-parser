(defpackage :openapi-parser
  (:use :cl :alexandria)
  (:export :parse-file))

(defpackage :openapi-parser/schema
  (:use :cl :alexandria))

(defpackage :openapi-parser/schema/3.0.1
  (:use :cl :alexandria)
  (:export :<open-api>
           :<info>
           :<contact>
           :<license>
           :<server>
           :<server-variable>
           :<components>
           :<paths>
           :<path-item>
           :<operation>
           :<external-documentation>
           :<parameter>
           :<request-body>
           :<media-type>
           :<encoding>
           :<responses>
           :<response>
           :<callback>
           :<example>
           :<link>
           :<header>
           :<tag>
           :<reference>
           :<schema>
           :<discriminator>
           :<xml>
           :<security-scheme>
           :<oauth-flow>
           :<o-auth-flow>
           :<security-requirement>))

(defpackage :openapi-parser/schema/3.1.0
  (:use :cl :alexandria)
  (:export :<open-api>
           :<info>
           :<contact>
           :<license>
           :<server>
           :<server-variable>
           :<components>
           :<paths>
           :<path-item>
           :<operation>
           :<external-documentation>
           :<parameter>
           :<request-body>
           :<media-type>
           :<encoding>
           :<responses>
           :<response>
           :<callback>
           :<example>
           :<link>
           :<header>
           :<tag>
           :<reference>
           :<schema>
           :<discriminator>
           :<xml>
           :<security-scheme>
           :<oauth-flow>
           :<o-auth-flow>
           :<security-requirement>))

(cl-package-locks:lock-package :openapi-parser)
(cl-package-locks:lock-package :openapi-parser/schema)
(cl-package-locks:lock-package :openapi-parser/schema/3.0.1)
(cl-package-locks:lock-package :openapi-parser/schema/3.1.0)
