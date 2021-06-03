(in-package :openapi-parser)

(defvar *current-path* '())

(defun append-path (*current-path* key)
  (cons key *current-path*))

(defun call-with-path (operator value function)
  (let ((*current-path*
          (ecase operator
            (:append (if value
                         (append-path *current-path* value)
                         *current-path*))
            (:replace (reverse value)))))
    (funcall function)))

(defmacro with-path ((operator value) &body body)
  `(call-with-path ,operator ,value (lambda () ,@body)))

(defun get-path ()
  (reverse *current-path*))
