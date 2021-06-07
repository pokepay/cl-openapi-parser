(in-package :openapi-parser)

(defstruct scalar
  value
  line)

(defstruct object
  plist)

(defun get-object-value (object key)
  (loop :for (k v) :on (object-plist object) :by #'cddr
        :do (when (and (scalar-p k)
                       (equal key (scalar-value k)))
              (return v))))

(defun parser-line-number (parser)
  (1+ (libyaml.parser:error-line parser)))

;;; copied from
;;; - yaml.parser::parse-yaml
;;; - yaml.parser::parse-tokens
(defun parse-yaml (yaml-string)
  (let ((contexts (list nil)))
    (libyaml.macros:with-parser (parser yaml-string)
      (libyaml.macros:with-event (event)
        (loop
          (let ((parsing-result (libyaml.parser:parse parser event)))
            (assert parsing-result)
            (case (libyaml.event:event-type event)
              ;; Stream events
              (:stream-start-event
               )
              (:stream-end-event
               (return))
              ;; Document events, push them to the output list
              (:document-start-event
               (push (list) contexts))
              (:document-end-event
               (let ((con (pop contexts)))
                 (setf (first contexts)
                       (append (first contexts)
                               con))))
              (:scalar-event
               (destructuring-bind (&key value tag style &allow-other-keys)
                   (libyaml.event:event-scalar-data event)
                 (let ((elt (yaml.parser::convert-scalar value tag style)))
                   (setf (first contexts)
                         (append (first contexts)
                                 (list (make-scalar :value elt
                                                    :line (parser-line-number parser))))))))
              ;; Sequence start and end events
              (:sequence-start-event
               (push (list (getf (libyaml.event:event-sequence-start-data event) :tag))
                     contexts))
              (:sequence-end-event
               (destructuring-bind (tag &rest seq) (pop contexts)
                 (setf (first contexts)
                       (append (first contexts)
                               (list (yaml.parser::convert-sequence seq tag))))))
              ;; Mapping start and end events
              (:mapping-start-event
               (push (list (getf (libyaml.event:event-mapping-start-data event) :tag)
                           :line (parser-line-number parser))
                     contexts))
              (:mapping-end-event
               (destructuring-bind (tag &rest plist) (pop contexts)
                 (declare (ignore tag))
                 (setf (first contexts)
                       (append (first contexts)
                               (list (make-object :plist plist)))))))))))
    (first (first contexts))))

(defun follow-line-number (yaml path)
  (dolist (key path)
    (etypecase key
      (integer
       (setf yaml (elt yaml key)))
      (string
       (setf yaml (get-object-value yaml key)))))
  (assert yaml)
  (etypecase yaml
    (scalar
     (scalar-line yaml))
    (object
     (getf (object-plist yaml) :line))))

(defun compute-line-number-from-path (yaml-file path)
  (follow-line-number (parse-yaml (read-file-into-string yaml-file))
                      path))
