(in-package :openapi-parser)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defvar *reading-toplevel-yaml-object*)
(defvar *reading-yaml-filename*)

(defun openapi-parse-error (datum &rest arguments)
  (let* ((path (get-path))
         (line-number (compute-line-number-from-path *reading-yaml-filename* path)))
    (apply #'error datum
           :path (get-path)
           :line-number line-number
           arguments)))

(defun validate-type (value type)
  (unless (typep value type)
    (openapi-parse-error 'invalid-value
                         :value value
                         :expected-type type)))

(defun finalize-class (class)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class)))

(defun subclass-of (subclass superclass-name)
  (finalize-class subclass)
  (not (null (member superclass-name
                     (c2mop:class-precedence-list subclass)
                     :key #'class-name))))

(defun fixed-fields-schema-p (class)
  (subclass-of class 'openapi-parser/schema::fixed-fields-schema))

(defun patterned-fields-schema-p (class)
  (subclass-of class 'openapi-parser/schema::patterned-fields-schema))

(defun specification-extension-key-p (key)
  (starts-with-subseq "x-" key))

(defun slot-name-to-key-name (slot-name)
  (if (eq 'openapi-parser/schema::$ref slot-name)
      "$ref"
      (change-case:camel-case (string slot-name))))

(defun parse-fixed-fields-schema (class yaml)
  (validate-type yaml 'hash-table)
  (let ((initargs '())
        (seen-keys '()))
    (loop :for slot :in (openapi-parser/schema::schema-spec-slots class)
          :for slot-name := (c2mop:slot-definition-name slot)
          :for key := (slot-name-to-key-name slot-name)
          :do (if (not (hash-exists-p key yaml))
                  (when (openapi-parser/schema::field-slot-required slot)
                    (openapi-parse-error 'missing-field
                                         :name key
                                         :expected-type class))
                  (let ((value (gethash key yaml)))
                    (push key seen-keys)
                    (push (make-keyword (string slot-name)) initargs)
                    (push (parse (c2mop:slot-definition-type slot)
                                 value
                                 :key key)
                          initargs))))
    (let* ((unknown-keys
             (set-difference (hash-table-keys yaml) seen-keys :test #'equal))
           (rest-keys
             (remove-if #'specification-extension-key-p
                        unknown-keys)))
      (let ((ext (make-hash-table :test 'equal)))
        (dolist (x-key (remove-if-not #'specification-extension-key-p unknown-keys))
          (setf (gethash x-key ext) (gethash x-key yaml)))
        (push :x-properties initargs)
        (push ext initargs))
      (values (apply #'make-instance
                     class
                     (nreverse initargs))
              rest-keys))))

(defun parse-patterned-fields-schema (schema yaml)
  (validate-type yaml 'hash-table)
  (let ((patterned-field-slot (openapi-parser/schema::patterned-schema-field-slot schema))
        (field-values '())
        (matched-keys '()))
    (maphash (lambda (key value)
               (ppcre:register-groups-bind (matched)
                   ((openapi-parser/schema::field-slot-pattern patterned-field-slot)
                    key)
                 (when matched
                   (push key matched-keys)
                   (push (cons key
                               (parse (c2mop:slot-definition-type patterned-field-slot)
                                      value
                                      :key key))
                         field-values))))
             yaml)
    (setf (openapi-parser/schema::patterned-fields-schema-field schema)
          (nreverse field-values))
    (values schema
            matched-keys)))

(defun merge-schema (dst-schema src-schema class)
  (assert (eq (type-of dst-schema) (type-of src-schema)))
  (assert (eq (class-of dst-schema) class))
  (loop :for slot :in (c2mop:class-slots class)
        :for slot-name := (c2mop:slot-definition-name slot)
        :do (when (slot-boundp src-schema slot-name)
              (setf (slot-value dst-schema slot-name)
                    (slot-value src-schema slot-name))))
  (values))

(defun parse-json-schema (schema-class value result)
  (when (hash-exists-p "$ref" value)
    (let ((ref-schema
            (let* ((path (parse-ref-path (gethash "$ref" value)))
                   (ref-value (or (reference-path *reading-toplevel-yaml-object* path)
                                  (openapi-parse-error 'no-such-field-error :ref (gethash "$ref" value)))))
              (with-path (:replace path)
                (parse schema-class ref-value)))))
      (merge-schema result ref-schema schema-class)))
  result)

(defun parse-schema (schema-class value)
  ;; fixed/patterned-field-schema-pの両方がtになるケースを考慮
  (multiple-value-bind (schema rest-keys)
      (when (fixed-fields-schema-p schema-class)
        (parse-fixed-fields-schema schema-class value))
    (when (patterned-fields-schema-p schema-class)
      (let (matched-keys)
        (setf (values schema matched-keys)
              (parse-patterned-fields-schema (or schema (make-instance schema-class))
                                             value))
        (setf rest-keys (set-difference rest-keys matched-keys :test #'string=))))
    (dolist (key rest-keys)
      (with-path (:append key)
        (warn 'out-of-spec-key
              :key key
              :expected-type schema-class)))
    (assert (not (null schema)))

    (when (eq (class-name schema-class) (openapi-parser/schema::get-schema-class))
      (parse-json-schema schema-class value schema))

    schema))

(defun reference-path (yaml path)
  (dolist (key path)
    (unless (hash-exists-p key yaml)
      (return-from reference-path nil))
    (setf yaml (gethash key yaml)))
  yaml)

(defun parse-ref-path (ref)
  (assert (ppcre:scan "^#/" ref)) ;TODO
  (rest (uiop:split-string ref :separator "/")))

(defun parse-reference-or (type-spec value)
  (handler-case (parse type-spec value)
    (openapi-parser-error (c)
      (let* ((reference (handler-case
                            (parse-schema (find-class (openapi-parser/schema::get-reference-class))
                                          value)
                          (openapi-parser-error ()
                            (error c))))
             (path (parse-ref-path (openapi-parser/schema::$ref reference)))
             (ref-value (or (reference-path *reading-toplevel-yaml-object*
                                            path)
                            (openapi-parse-error 'no-such-field-error
                                                 :ref (openapi-parser/schema::$ref reference)))))
        (with-path (:replace path)
          (parse type-spec ref-value))))
    (:no-error (result)
      result)))

(defun parse-map (k-type v-type yaml)
  (validate-type yaml 'hash-table)
  (let ((new-map (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (validate-type k k-type)
               (setf (gethash k new-map)
                     (parse v-type v :key k)))
             yaml)
    new-map))

(defun parse-schema-aux (type-spec value)
  (trivia:ematch type-spec
    (t
     value)
    ((list 'openapi-parser/schema::<map> k v)
     (parse-map k v value))
    ('openapi-parser/schema::<forward-referenced-schema>
     (parse (openapi-parser/schema::get-schema-class)
            value))
    ((list 'or
           (trivia:guard class-name
                         (eq class-name (openapi-parser/schema::get-reference-class)))
           x)
     (parse-reference-or x value))
    ((list 'or
           x
           (trivia:guard class-name
                         (eq class-name (openapi-parser/schema::get-reference-class))))
     (parse-reference-or x value))
    ((list* 'or types)
     (loop :with parser-errors := '()
           :for type :in types
           :do (handler-case (parse type value)
                 (openapi-parser-error (c)
                   (push c parser-errors))
                 (:no-error (result)
                   (return result)))
           :finally (openapi-parse-error 'invalid-all-values
                           :key (lastcar (get-path))
                           :value value
                           :expected-type type-spec
                           :errors parser-errors)))
    ((list 'trivial-types:proper-list array-type)
     (validate-type value 'list)
     (loop :for item :in value
           :for i :from 0
           :collect (with-path (:append i) ; 配列の添字をkeyにしている
                      (parse array-type item))))
    ((type symbol)
     (if-let (class (find-class type-spec nil))
       (parse class value)
       (progn
         (validate-type value type-spec)
         value)))
    ((type built-in-class)
     (validate-type value type-spec)
     value)
    ((type standard-class)
     (parse-schema type-spec value))))

(defun parse (type-spec value &key key)
  (with-path (:append key)
    (parse-schema-aux type-spec value)))

(defun print-file-location (stream &optional (path (get-path)))
  (let ((line-number (compute-line-number-from-path *reading-yaml-filename* path)))
    (format stream "File: ~A~%" *reading-yaml-filename*)
    (format stream "Line: ~D~%" line-number)))

(defun handle-openapi-parser-error (c)
  (print-file-location *error-output* (openapi-parser-condition-context-path c))
  (format *error-output* "~A~%" c))

(defun version (yaml)
  (gethash "openapi" yaml))

(defun parse-file (pathname)
  (check-type pathname pathname)
  (let* ((yaml (cl-yaml:parse pathname))
         (*reading-toplevel-yaml-object* yaml)
         (*reading-yaml-filename* pathname)
         (openapi-parser/schema::*openapi-version-package*
           (openapi-parser/schema::version-package
            (version yaml))))
    (parse (find-class (openapi-parser/schema::get-openapi-class)) yaml)))
