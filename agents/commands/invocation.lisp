#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.commands)

(defvar *invokers* ())

(defvar *framework-client* (make-instance 'client :name "FRAMEWORK"))
(defvar *framework-user* (make-instance 'user :name "FRAMEWORK" :client *framework-client* :authenticated T))
(defvar *dispatch-event* NIL)
(defvar *alternative-distance-threshold* 10)

(define-event framework-message (message-event passive-event)
  ()
  (:default-initargs
   :client *framework-client*
   :user *framework-user*
   :message ""))

(defmethod send-to ((user (eql *framework-user*)) format-string &rest format-args)
  (v:info :commands "=> ~?" (list* format-string format-args)))

(defmethod reply ((event framework-message) format-string &rest format-args)
  (apply #'v:info :commands format-string format-args))

(defun issue-message (core command-string)
  (do-issue core framework-message :message command-string))

(define-event command-event (instruction-event active-event)
  ((dispatch-event :initarg :dispatch-event :reader dispatch-event))
  (:default-initargs
   :dispatch-event (or *dispatch-event* (make-instance 'framework-message)))
  (:advice public))

(defun command-invoker (name)
  (cdr (assoc name *invokers* :test #'string-equal)))

(defun (setf command-invoker) (function name)
  (update-list (cons (string-downcase name) function) *invokers* :key #'car :test #'string-equal)
  function)

(defun remove-command-invoker (name)
  (setf *invokers* (remove name *invokers* :key #'car :test #'string-equal)))

(defmacro define-command-invoker (command (event &rest args) &body body)
  `(setf (command-invoker ',command)
         (lambda (,event message)
           (with-command-destructuring-bind ,args message
             ,@body))))

(defmacro define-simple-command-invoker (command args event-type &key message-event-initarg)
  (let ((event (gensym "EVENT"))
        (fun-kargs (loop for arg in (lambda-fiddle:extract-lambda-vars args)
                         collect (kw arg) collect arg)))
    `(define-command-invoker ,command (,event ,@args)
       (issue (make-instance ',event-type
                             ,@(when message-event-initarg `(,message-event-initarg ,event))
                             ,@fun-kargs)
              (event-loop ,event)))))

(defmacro define-command ((consumer name &optional (event-type name)) (instance event &rest args) &body body)
  (form-fiddle:with-body-options (body options superclasses command command-event-variable) body
    (let ((command-event-variable (or command-event-variable (gensym "COMMAND-EVENT")))
          (error (gensym "ERROR")))
      `(progn (define-function-handler (,consumer ,name ,event-type) (,instance ,command-event-variable ,@args)
                :superclasses (,@superclasses command-event)
                ,@options
                (let* ((,event (dispatch-event ,command-event-variable))
                       (*dispatch-event* ,event))
                  (handler-case
                      (handler-bind ((error (lambda (err)
                                              (with-simple-restart (continue "Don't handle the error.")
                                                (invoke-debugger err)))))
                        ,@body)
                    (error (,error)
                      (v:warn :maiden.agents.commands ,error)
                      (reply ,event "~a" ,error)))))
              (define-simple-command-invoker ,(or command (string name)) ,args ,event-type
                :message-event-initarg :dispatch-event)
              (list ',consumer ',name)))))

(defun find-matching-command (message)
  (let ((match NIL)
        (alternatives ()))
    (loop for command in *invokers*
          for prefix = (car command)
          do (when (<= (length prefix) (length message))
               (let* ((cut (subseq message 0 (length prefix)))
                      (distance (levenshtein-distance prefix cut)))
                 (when (and (= 0 distance)
                            (or (null match) (< (length (car match))
                                                (length prefix))))
                   (setf match command))
                 (when (< distance *alternative-distance-threshold*)
                   (push (cons distance command) alternatives)))))
    (values match alternatives)))
