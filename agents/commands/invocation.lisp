#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.commands)

(defvar *invokers* ())

(defvar *framework-client* (make-instance 'client :name "FRAMEWORK"))
(defvar *framework-sender* (make-instance 'user :name "FRAMEWORK" :client *framework-client*))
(defvar *dispatch-event* NIL)
(defvar *alternative-distance-threshold* 10)

(define-event framework-message (message-event)
  ()
  (:default-initargs
   :client *framework-client*
   :sender *framework-sender*
   :message ""))

(defmethod reply ((event framework-message) format-string &rest format-args)
  (apply #'v:info :commands format-string format-args))

(defun issue-message (core command-string)
  (do-issue core framework-message :message command-string))

(define-event command-event (instruction-event)
  ((dispatch-event :initarg :dispatch-event :accessor dispatch-event))
  (:default-initargs
   :dispatch-event (or *dispatch-event* (make-instance 'framework-message))))

(defun command-invoker (name)
  (cdr (assoc name *invokers* :test #'string-equal)))

(defun (setf command-invoker) (function name)
  (update-list (cons (string-downcase name) function) *invokers* :key #'car :test #'string-equal))

(defun remove-command-invoker (name)
  (setf *invokers* (remove name *invokers* :key #'car :test #'string-equal)))

(defmacro define-command-invoker (command (event &rest args) &body body)
  `(setf (command-invoker ',command)
         (lambda (,event message)
           (with-command-destructuring-bind ,args message
             ,@body))))

(defmacro define-command ((consumer name) (instance event &rest args) &body body)
  (form-fiddle:with-body-options (body options command command-event-variable) body
    (let ((fun-kargs (loop for arg in (lambda-fiddle:extract-lambda-vars args)
                           collect (kw arg) collect arg))
          (command-event-variable (or command-event-variable (gensym "COMMAND-EVENT"))))
      `(progn (define-instruction (,consumer ,name) (,instance ,command-event-variable ,@args)
                :superclasses (command-event)
                ,@options
                (let* ((,event (dispatch-event ,command-event-variable))
                       (*dispatch-event* ,event))
                  ,@body))
              (define-command-invoker ,(or command (string name)) (,event ,@args)
                (issue (make-instance ',name :dispatch-event ,event ,@fun-kargs)
                       (event-loop ,event)))))))

(defun find-matching-command (message)
  (let ((match NIL)
        (alternatives ()))
    (loop for command in *invokers*
          for prefix = (car command)
          for cut = (subseq message 0 (length prefix))
          for distance = (levenshtein-distance prefix cut)
          do (when (and (= 0 distance)
                        (or (null match) (< (length (car match))
                                            (length prefix))))
               (setf match command))
             (when (< distance *alternative-distance-threshold*)
               (push (cons distance command) alternatives)))
    (values match alternatives)))
