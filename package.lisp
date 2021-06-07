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

(defpackage :openapi-parser/schema/3/interface
  (:use :cl)
  (:export :<json-schema>
           :<open-api>
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
           :<security-requirement>
           :->type
           :->enum
           :->const
           :->multiple-of
           :->maximum
           :->exclusive-maximum
           :->minimum
           :->exclusive-minimum
           :->max-length
           :->min-length
           :->pattern
           :->max-items
           :->min-items
           :->unique-items
           :->max-contents
           :->min-contents
           :->max-properties
           :->min-properties
           :->required
           :->dependent-required
           :->default
           :->properties
           :->all-of
           :->any-of
           :->one-of
           :->not
           :->items
           :->title
           :->additional-properties
           :->description
           :->format
           :->$ref
           :->nullable
           :->read-only
           :->write-only
           :->openapi
           :->info
           :->json-schema-dialect
           :->paths
           :->webhooks
           :->components
           :->title
           :->terms-of-service
           :->contact
           :->license
           :->version
           :->email
           :->identifier
           :->variables
           :->enum
           :->schemas
           :->request-bodies
           :->security-schemes
           :->path-items
           :->get
           :->put
           :->post
           :->delete
           :->options
           :->head
           :->patch
           :->trace
           :->tags
           :->responses
           :->callbacks
           :->security
           :->servers
           :->url
           :->deprecated
           :->allow-empty-value
           :->required
           :->schema
           :->examples
           :->encoding
           :->content-type
           :->style
           :->explode
           :->allow-reserved
           :->default
           :->headers
           :->content
           :->links
           :->value
           :->external-value
           :->operation-ref
           :->operation-id
           :->parameters
           :->request-body
           :->server
           :->$ref
           :->summary
           :->discriminator
           :->xml
           :->external-docs
           :->example
           :->property-name
           :->mapping
           :->namespace
           :->prefix
           :->attribute
           :->wrapped
           :->type
           :->description
           :->name
           :->in
           :->scheme
           :->bearer-format
           :->flows
           :->open-id-connect-url
           :->implicit
           :->password
           :->client-credentials
           :->authorization-code
           :->authorization-url
           :->token-url
           :->refresh-url
           :->scopes
           :->field*))

(defpackage :openapi-parser/schema
  (:use :cl :alexandria)
  (:export :get-x-property))

(defpackage :openapi-parser
  (:use :cl :alexandria)
  ;; (:local-nicknames
  ;;  (:3.0.1
  ;;   :openapi-parser/schema/3.0.1)
  ;;  (:3.1.0
  ;;   :openapi-parser/schema/3.1.0))
  (:export :parse-file))

(cl-package-locks:lock-package :openapi-parser)
(cl-package-locks:lock-package :openapi-parser/schema)
(cl-package-locks:lock-package :openapi-parser/schema/3.0.1)
(cl-package-locks:lock-package :openapi-parser/schema/3.1.0)
