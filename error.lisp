(in-package :openapi-parser)

(defun path-to-string (path)
  (format nil "~{~A~^.~}" path))

(defun print-error-header (condition stream)
  (when-let (file (openapi-parser-condition-context-file condition))
    (format stream "File: ~A~%" file))
  (format stream "Path: ~A~%" (path-to-string (openapi-parser-condition-context-path condition)))
  (when-let (line (openapi-parser-condition-context-line-number condition))
    (format stream "Line: ~A~%" line))
  (when (slot-boundp condition 'expected-type)
    (format stream "Expected type: ~A~%" (openapi-parser-condition-context-expected-type condition))))

(define-condition openapi-parser-condition-context ()
  ((file
    :initarg :file
    :initform nil
    :reader openapi-parser-condition-context-file)
   (path
    :initarg :path
    :initform (get-path)
    :reader openapi-parser-condition-context-path)
   (line-number
    :initarg :line-number
    :initform nil
    :reader openapi-parser-condition-context-line-number)
   (expected-type
    :initarg :expected-type
    :reader openapi-parser-condition-context-expected-type)))

(define-condition openapi-parser-error (simple-error openapi-parser-condition-context)
  ())

(define-condition openapi-parser-warning (simple-warning openapi-parser-condition-context)
  ())

(define-condition out-of-spec-key (openapi-parser-warning)
  ((key :initarg :key))
  (:report (lambda (c s)
             (with-slots (key) c
               (print-error-header c s)
               (format s "Out of specification key '~A'" key)))))

(define-condition missing-field (openapi-parser-error)
  ((name :initarg :name
         :reader missing-field-name))
  (:report (lambda (c s)
             (with-slots (path name) c
               (print-error-header c s)
               (format s "Missing field key '~A'" name)))))

(define-condition invalid-value (openapi-parser-error)
  ((key :initarg :key
        :reader invalid-value-key)
   (value :initarg :value
          :reader invalid-value-value))
  (:report (lambda (c s)
             (with-slots (path key value expected-type) c
               (print-error-header c s)
               ;; (format s "The value ~S for key ~S is invalid" value key)
               (format s "The actual type ~S is not of expected type ~S"
                       (type-of value)
                       expected-type)))))

(define-condition invalid-all-values (invalid-value)
  ((errors :initarg :errors
           :reader invalid-all-values-errors))
  (:report (lambda (c s)
             (with-slots (errors) c
               (print-error-header c s)
               (format s "The following errors~%")
               (format s "~%~{~A~^~2%~}" errors)))))

(define-condition no-such-field-error (openapi-parser-error)
  ((ref :initarg :ref
         :reader no-such-field-error-ref))
  (:report (lambda (c s)
             (with-slots (ref) c
               (print-error-header c s)
               (format s "No such field ~S~%" ref)))))
