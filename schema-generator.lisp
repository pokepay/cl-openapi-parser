(defpackage :openapi-parser/schema-generator
  (:use :cl
        :alexandria))
(in-package :openapi-parser/schema-generator)

(defvar *version*)
(defvar *generated-package*)
(defvar *export-names*)
(defvar *readers*)

(defun change-to-chain-case (string)
  (cond ((string= string "OAuth Flows")
         "oauth-flow")
        (t
         (cl-change-case:param-case string))))

(defun blank-string-p (string)
  (string= "" (string-trim '(#\space #\tab) string)))

(defun string-trim-whitespaces (string)
  (string-trim '(#\space #\tab) string))

(defun split-vbars (string)
  (let ((strings '())
        (acc-chars '())
        (pos 0))
    (flet ((add-to-strings ()
             (push (string-trim-whitespaces (coerce (nreverse acc-chars) 'string))
                   strings)
             (setf acc-chars '()))
           (add-to-chars ()
             (push (char string pos) acc-chars)))
      (loop :while (< pos (length string))
            :do (case (char string pos)
                  (#\\
                   (incf pos)
                   (add-to-chars))
                  (#\|
                   (add-to-strings))
                  (otherwise
                   (add-to-chars)))
                (incf pos)
            :finally (add-to-strings)))
    (nreverse strings)))

#|
#+#.(swank::with-symbol :run :rove)
(rove:deftest split-vbars
  (rove:ok (equal '("foo")
                  (split-vbars "foo")))
  (rove:ok (equal '("foo" "bar")
                  (split-vbars "foo|bar")))
  (rove:ok (equal '("foo" "bar" "baz")
                  (split-vbars "foo|bar|baz")))
  (rove:ok (equal '("foo" "bar" "b|az")
                  (split-vbars "foo|bar|b\\|az"))))
|#

(defparameter *schema-base-symbols*
  '(:define-schema
    :$ref))

(defun schema-base-symbol-p (symbol-name)
  (member symbol-name
          *schema-base-symbols*
          :test #'string=))

(defun string-to-symbol (string)
  (let ((symbol-name (string-upcase string)))
    (if (schema-base-symbol-p symbol-name)
        (multiple-value-bind (symbol foundp)
            (find-symbol symbol-name :openapi-parser/schema)
          (assert foundp)
          symbol)
        (intern symbol-name
                *generated-package*))))

(defun pprint* (object stream)
  (let ((*package* *generated-package*))
    (pprint object stream)))

(defun trim-a-tag (string &optional (prefix ""))
  (ppcre:register-groups-bind (part) ((format nil "^~A<a name=\"\\w+\"></a>\\s*(.*)" prefix) string)
    part))

(defstruct schema-table
  (ordered-names '())
  (map (make-hash-table :test 'equal)))

(defclass schema ()
  ((name :initarg :name
         :reader schema-name)
   (fields :initarg :fields
           :initform nil
           :accessor schema-fields)
   (base-schema :initarg :base-schema
                :initform nil
                :accessor schema-base-schema)))

(defclass fixed-fields-schema (schema)
  ()
  (:default-initargs :base-schema 'openapi-parser/schema::fixed-fields-schema))

(defclass patterned-fields-schema (schema)
  ()
  (:default-initargs :base-schema 'openapi-parser/schema::patterned-fields-schema))

(defun change-to-schema-class-name (name)
  (string-to-symbol (format nil "<~A>" (change-to-chain-case name))))

(defun construct-schema-fields-with-fixed-fields (header rows)
  (let ((fields
          (loop :for row :in rows
                :collect (loop :for col :in row
                               :for name :in header
                               :append (switch (name :test #'string=)
                                         ("Field Name"
                                          (let ((name (or (trim-a-tag col) col)))
                                            (list :field-name name)))
                                         ("Type"
                                          (list :type col))
                                         ("Description"
                                          (list* :description col
                                                 (when (starts-with-subseq "**REQUIRED**." col)
                                                   (list :required t)))))))))
    fields))

(defun construct-schema-with-fixed-fields (object-name header rows)
  (let ((fields (construct-schema-fields-with-fixed-fields header rows)))
    (make-instance 'fixed-fields-schema
                   :name object-name
                   :fields fields)))

(defun construct-schema-fields-with-patterned-fields (header rows)
  (let ((fields (loop :for row :in rows
                      :collect (loop :for col :in row
                                     :for name :in header
                                     :append (switch (name :test #'string=)
                                               ("Field Pattern"
                                                (list :field-name (trim-a-tag col)))
                                               ("Type"
                                                (list :type col))
                                               ("Description"
                                                (list :description col)))))))
    (assert (length= fields 1))
    fields))

(defun construct-schema-with-patterned-fields (object-name header rows)
  (let ((fields (construct-schema-fields-with-patterned-fields header rows)))
    (make-instance 'patterned-fields-schema
                   :name object-name
                   :fields fields)))

(defun sanitize-field-name (field-name)
  (cond ((string= field-name "$ref")
         "$ref")
        ((ppcre:register-groups-bind (x) ("^/?{(\\w+)}$" field-name) x))
        (t
         (handler-case (change-to-chain-case (second (esrap:parse 'link field-name)))
           (esrap:esrap-parse-error ()
             (change-to-chain-case field-name))))))

(defmethod schema-field-to-slot-form ((schema patterned-fields-schema) field)
  (destructuring-bind (&key field-name type description) field
    (declare (ignorable description))
    `(,openapi-parser/schema::+patterned-field-slot-name+
      :field-name ,field-name
      :type ,(parse-field-type type)
      #|:documentation ,description|#)))

(defmethod schema-field-to-slot-form ((schema fixed-fields-schema) field)
  (destructuring-bind (&key field-name type description required) field
    (declare (ignorable description))
    (let ((slot-name (string-to-symbol (sanitize-field-name field-name))))
      `(,slot-name
        :field-name ,field-name
        :type ,(parse-field-type type)
        ,@(when required `(:required t))
        #|:documentation ,description|#))))

(defun field-to-slot-form (schema field)
  (schema-field-to-slot-form schema field))

(defun schema-to-base-class-name (schema)
  (if (symbolp (schema-base-schema schema))
      (schema-base-schema schema)
      (schema-name-to-class-name (schema-base-schema schema))))

(defun schema-name-to-class-name (schema-name)
  (change-to-schema-class-name
   (ppcre:regex-replace " Object$" schema-name "")))

(defun schema-to-defclass-form (schemas)
  (let ((schema (first schemas)))
    (let ((class-name (schema-name-to-class-name (schema-name schema))))
      (push class-name *export-names*)
      `(openapi-parser/schema::define-schema ,class-name
           ,(mapcar #'schema-to-base-class-name schemas)
         ,@(mapcan (lambda (schema)
                     (mapcar (lambda (field)
                               (let ((slot-form (field-to-slot-form schema field)))
                                 (push (list class-name (first slot-form)) *readers*)
                                 slot-form))
                             (schema-fields schema)))
                   schemas)))))

(defstruct reader
  stream
  pool-lines
  last-table-header
  last-table-alignment)

(defun next-line (reader)
  (if (null (reader-pool-lines reader))
      (read-line (reader-stream reader))
      (pop (reader-pool-lines reader))))

(defun pushback-line (reader line)
  (push line (reader-pool-lines reader)))

(defun read-table-body (reader)
  (loop :for line := (next-line reader)
        :until (string= line "")
        :collect (split-vbars line)))

(defun read-table (reader)
  (let ((header
          (loop :for line := (next-line reader)
                :do (unless (blank-string-p line)
                      (return (split-vbars line))))))
    (setf (reader-last-table-header reader) header)
    (let ((table-alignment (next-line reader)))
      (assert (member table-alignment
                      '("---|:---:|---"
                        "---|:---|---"
                        "---|:---:|---|---")
                      :test #'string=))
      (setf (reader-last-table-alignment reader) table-alignment))
    (let ((rows (read-table-body reader)))
      (values header
              rows))))

(defun table-continue-p (line reader)
  (when (equal (split-vbars line)
               (reader-last-table-header reader))
    (let ((line (next-line reader)))
      (cond ((equal line (reader-last-table-alignment reader))
             t)
            (t
             (pushback-line reader line)
             nil)))))

(defun append-schema-fields (schema header rows)
  (etypecase schema
    (fixed-fields-schema
     (appendf (schema-fields schema)
              (construct-schema-fields-with-fixed-fields header rows)))
    (patterned-fields-schema
     (appendf (schema-fields schema)
              (construct-schema-fields-with-patterned-fields header rows)))))

(defun read-openapi (openapi-file)
  (let ((schema-table (make-schema-table))
        (last-schema nil))
    (flet ((add-schema (schema)
             (when (equal "Schema Object" (schema-name schema))
               (setf (schema-base-schema schema) openapi-parser/schema::+json-schema-class-name+))
             (setf last-schema schema)
             (pushnew (schema-name schema)
                      (schema-table-ordered-names schema-table)
                      :test #'equal)
             (push schema
                   (gethash (schema-name schema)
                            (schema-table-map schema-table)))))
      (with-open-file (in openapi-file)
        (let ((reader (make-reader :stream in)))
          (loop :for line := (next-line reader)
                :do (when (ppcre:scan "^### Schema$" line)
                      (return)))
          (loop :with last-object-name
                :for line := (next-line reader)
                :until (ppcre:scan "^###\\s" line)
                :do (cond
                      ((when-let ((object-name (trim-a-tag line "#### ")))
                         (setf last-object-name object-name)
                         t))
                      ((table-continue-p line reader)
                       ;; Parameter Objectのテーブルが分割している形に対応
                       (let ((rows (read-table-body reader)))
                         (append-schema-fields last-schema
                                               (reader-last-table-header reader) 
                                               rows)))
                      ((ppcre:scan "^##### .*Fixed Fields\\s*$" line)
                       ;; 次の二つのケースがある
                       ;; - ##### Fixed Fields
                       ;; - ##### <a name="baseVocabulary"></a>Fixed Fields
                       (multiple-value-bind (headers rows) (read-table reader)
                         (add-schema (construct-schema-with-fixed-fields last-object-name
                                                                         headers
                                                                         rows))))
                      ((ppcre:scan "^##### Patterned Fields" line)
                       (multiple-value-bind (headers rows) (read-table reader)
                         (add-schema (construct-schema-with-patterned-fields last-object-name
                                                                             headers
                                                                             rows))))
                      ((and (equal last-object-name "Header Object")
                            (equal line "The Header Object follows the structure of the [Parameter Object](#parameterObject) with the following changes:"))
                       (add-schema (make-instance 'schema
                                                  :name last-object-name
                                                  :base-schema "Parameter Object")))))))
      (nreversef (schema-table-ordered-names schema-table))
      schema-table)))

(defun version (pathname)
  ;; #P"/versions/3.1.0.md"というような値が引数であることを想定している
  (pathname-name pathname))

(defun generated-package (version)
  (openapi-parser/schema::version-package version))

(defun generate-defpackage-form (package-name export-names)
  (terpri)
  (pprint `(defpackage ,(make-keyword package-name)
             (:use :cl :alexandria)
             (:export ,@(mapcar #'make-keyword export-names)))))

(defun generate-schema.lisp (openapi-file output-stream)
  (let* ((*version* (version openapi-file))
         (*generated-package* (generated-package *version*))
         (*export-names* '())
         (schema-table (read-openapi openapi-file)))
    (write-line ";;; Code generated by schema-generator.lisp: DO NOT EDIT." output-stream)
    (let ((*print-case* :downcase)
          (*print-right-margin* 100))
      (pprint* `(in-package ,(make-keyword (package-name *generated-package*))) output-stream)
      (dolist (schema-name (schema-table-ordered-names schema-table))
        (let ((schemas (gethash schema-name (schema-table-map schema-table))))
          (terpri output-stream)
          (pprint* (schema-to-defclass-form schemas) output-stream)))
      (generate-defpackage-form (package-name *generated-package*)
                                 (nreverse *export-names*)))))

(defun generate-lisp (openapi-file schema-definition-file)
  (with-open-file (out schema-definition-file
                       :direction :output
                       :if-exists :supersede)
    (generate-schema.lisp openapi-file out)))

(defun json-schema-reader-names ()
  (loop :for slot-name :in
           (mapcar #'c2mop:slot-definition-name
                   (openapi-parser/schema::schema-spec-slots
                    (find-class openapi-parser/schema::+json-schema-class-name+)))
        :collect (make-keyword
                  (openapi-parser/schema::make-reader-name
                   openapi-parser/schema::+json-schema-class-name+
                   slot-name))))

(defun generate-all-readers (readers reader-definition-file)
  (with-open-file (out reader-definition-file
                       :direction :output
                       :if-exists :supersede)
    (let ((*print-case* :downcase)
          (*print-right-margin* 100))
      (write-line ";;; Code generated by schema-generator.lisp: DO NOT EDIT." out)
      (pprint `(in-package ,(make-keyword :openapi-parser/schema)) out)
      (generate-defpackage-form
       :openapi-parser/schema
       (remove-duplicates
        (append
         (json-schema-reader-names)
         (loop :for (class-name slot-name) :in readers
               :for reader-defmethod-form := (openapi-parser/schema::generate-schema-slot-reader
                                              class-name
                                              slot-name
                                              (find-package :openapi-parser/schema))
               :do (terpri out)
                   (let ((*package* (find-package :openapi-parser/schema)))
                     (pprint reader-defmethod-form out))
               :collect (second reader-defmethod-form))))))))

(defun generate ()
  (let ((*readers* '()))
    (generate-lisp (asdf:system-relative-pathname :openapi-parser "versions/3.0.1.md")
                   (asdf:system-relative-pathname :openapi-parser "schema-3-0-1.lisp"))
    (generate-lisp (asdf:system-relative-pathname :openapi-parser "versions/3.1.0.md")
                   (asdf:system-relative-pathname :openapi-parser "schema-3-1-0.lisp"))
    (generate-all-readers (nreverse *readers*)
                          (asdf:system-relative-pathname :openapi-parser "schema-readers.lisp"))))

;;;
(esrap:defrule whitespace (* #\space)
  (:constant nil))

(esrap:defrule primitive-type (and #\` (+ (alphanumericp character)) #\`)
  (:lambda (result)
    (destructuring-bind (start characters end) result
      (declare (ignore start end))
      (eswitch ((with-output-to-string (out)
                  (dolist (char characters)
                    (write-char char out)))
                :test #'string=)
        ("string" 'string)
        ("boolean" 'boolean)))))

(defun space-or-alphanumeric-p (character)
  (or (alphanumericp character)
      (char= character #\space)))

(esrap:defrule link (and "["
                           (+ (space-or-alphanumeric-p character))
                           "](#"
                           (+ (alpha-char-p character))
                           ")")
  (:lambda (result)
    (list (coerce (second result) 'string)
          (coerce (fourth result) 'string))))

(esrap:defrule object link
  (:lambda (result)
    (let ((camel-string (second result)))
      (change-to-schema-class-name
       (ppcre:regex-replace "Object$" camel-string "")))))

(esrap:defrule map (and "Map["
                        field-type
                        ", "
                        field-type
                        whitespace
                        "]")
  (:lambda (result)
    `(openapi-parser/schema::<map> ,(second result) ,(fourth result))))

(esrap:defrule array (and "[" field-type "]")
  (:lambda (result)
    `(trivial-types:proper-list ,(second result))))

(esrap:defrule single-type (or map object array primitive-type)
  (:identity t))

(esrap:defrule complex-type (and single-type whitespace "|" whitespace single-type)
  (:lambda (result)
    `(or ,(first result)
         ,(fifth result))))

(esrap:defrule field-type (or complex-type single-type)
  (:identity t))

(defun parse-field-type (string)
  (cond ((string= string "Any")
         t)
        (t
         (esrap:parse 'field-type string :junk-allowed t))))
