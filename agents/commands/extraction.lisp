(in-package #:org.shirakumo.maiden.agents.commands)

(defvar *extractors* ())

(defun command-extractor (name)
  (cdr (assoc name *extractors*)))

(defun (setf command-extractor) (function name)
  (update-list (cons name function) *extractors* :key #'car))

(defun remove-command-extractor (name)
  (setf *extractors* (remove name *extractors* :key #'car)))

(defmacro define-command-extractor (name (event) &body body)
  `(progn (setf (command-extractor ',name)
                (lambda (,event)
                  ,@body))
          ',name))

(defmethod extract-command ((event message-event))
  (loop for extractor in *extractors*
        thereis (funcall (cdr extractor) event)))

(defmethod command-p ((event message-event))
  (not (null (extract-command event))))

(define-command-extractor prefix (event)
  (when (starts-with "::" (message event))
    (subseq (message event) 2)))

(define-command-extractor username (event)
  (let* ((msg (string-left-trim "@" (message event)))
         (username (username (client event)))
         (length (length username)))
    (when (and (< (1+ length) (length msg))
               (string-equal msg username :end1 length)
               (find (aref msg length) ":,：、"))
      (string-left-trim " 　" (subseq msg (1+ length))))))
