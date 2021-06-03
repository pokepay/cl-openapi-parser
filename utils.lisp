(in-package :openapi-parser)

(defun hash-exists-p (key hash-table)
  (nth-value 1 (gethash key hash-table)))
