(in-package :openapi-parser)

(defun compute-line-number-from-path (yaml-file path)
  (flet ((match-key-p (indent line key)
           (when-let (pos (position #\space line :test-not #'char=))
             (when (eql pos indent)
               (cond
                 ((ppcre:scan "^\\d+$" key)
                  (starts-with-subseq (format nil "'~A':" key)
                                      (subseq line pos)))
                 ((string= key "-")
                  (starts-with-subseq "- "
                                      (subseq line pos)))
                 (t
                  (starts-with-subseq (format nil "~A:" key)
                                      (subseq line pos))))))))
    (with-open-file (in yaml-file)
      (let ((linum 0)
            (line-queue))
        (flet ((next-line ()
                 (cond ((null line-queue)
                        (incf linum)
                        (read-line in))
                       (t
                        (pop line-queue))))
               (pushback (line)
                 (push line line-queue)))
          (loop :for key :in path
                :for indent :from 0 :by 2
                :do (etypecase key
                      (integer
                       (loop :with count := 0
                             :for line := (next-line)
                             :do (when (match-key-p indent line "-")
                                   (when (= count key)
                                     (pushback (str:concat (str:pad (+ 2 indent) "")
                                                           (str:trim-left
                                                            (string-left-trim
                                                             "-"
                                                             (str:trim-left line)))))
                                     (return))
                                   (incf count))))
                      (string
                       (loop :for line := (next-line)
                             :do (when (match-key-p indent line key)
                                   (return)))))
                :finally (return linum)))))))
