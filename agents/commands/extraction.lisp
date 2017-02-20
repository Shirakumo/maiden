#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.commands)

(defvar *extractors* ())

(defun command-extractor (name)
  (cdr (assoc name *extractors*)))

(defun (setf command-extractor) (function name)
  (update-list (cons name function) *extractors* :key #'car))

(defun remove-command-extractor (name)
  (setf *extractors* (remove name *extractors* :key #'car)))

(defmacro define-command-extractor (name (event) &body body)
  `(setf (command-extractor ',name)
         (lambda (,event)
           ,@body)))

(defmethod extract-command ((event message-event))
  (loop for extractor in *extractors*
        thereis (funcall (cdr extractor) event)))

(defmethod command-p ((event message-event))
  (not (null (extract-command event))))

(define-command-extractor prefix (event)
  (when (starts-with "::" (message event))
    (subseq (message event) 2)))

(define-command-extractor username (event)
  (when (starts-with (username (client event))
                     (message event))
    (subseq (message event) (length (username (client event))))))
