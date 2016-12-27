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

(defmethod reply ((event framework-message) format-string &rest format-args)
  (apply #'v:info :commands format-string format-args))

(defun issue-message (core command-string)
  (do-issue core framework-message :message command-string))

(define-event command-event (instruction-event active-event)
  ((dispatch-event :initarg :dispatch-event :reader dispatch-event))
  (:default-initargs
   :dispatch-event (or *dispatch-event* (make-instance 'framework-message)))
  (:advice public))

(defun relay (ev new-command &rest args)
  (issue (apply #'make-instance new-command :dispatch-event ev args)
         (core ev)))

(defclass command-invoker ()
  ((name :initarg :name :accessor name)
   (prefix :initarg :prefix :accessor prefix)
   (lambda-list :initarg :lambda-list :accessor lambda-list)
   (invoker :initarg :invoker :accessor invoker)
   (docstring :initarg :docstring :accessor docstring)))

(defmethod print-object ((command-invoker command-invoker) stream)
  (print-unreadable-object (command-invoker stream :type T)
    (format stream "~a" (name command-invoker))))

(defmethod documentation ((command-invoker command-invoker) type)
  (docstring command-invoker))

(defmethod (setf documentation) (value (command-invoker command-invoker) type)
  (setf (docstring command-invoker) value))

(defun command-invoker (name)
  (find name *invokers* :key #'name))

(defun (setf command-invoker) (invoker name)
  (remove-command-invoker name)  
  (setf *invokers* (sort (cons invoker *invokers*) #'string< :key #'prefix))
  invoker)

(defun remove-command-invoker (name)
  (setf *invokers* (remove name *invokers* :key #'name)))

(defun list-command-invokers ()
  (copy-list *invokers*))

(defun find-command-invoker (name)
  (loop for invoker in *invokers*
        do (when (or (eql name (name invoker))
                     (string-equal name (prefix invoker)))
             (return invoker))))

(defmacro define-command-invoker (name (event &rest lambda-list) &body body)
  (let ((message (gensym "MESSAGE")))
    (destructuring-bind (name prefix) (enlist name name)
      `(setf (command-invoker ',name)
             (make-instance 'command-invoker
                            :name ',name
                            :prefix ,(string-downcase prefix)
                            :lambda-list ',lambda-list
                            :invoker (lambda (,event ,message)
                                       (with-command-destructuring-bind ,lambda-list ,message
                                         ,@body))
                            :docstring ,(form-fiddle:lambda-docstring `(lambda () ,@body)))))))

(defmethod documentation (slot (type (eql 'command)))
  (documentation (command-invoker slot) T))

(defmethod (setf documentation) (docstring slot (type (eql 'command)))
  (setf (documentation (command-invoker slot) T) docstring))

(defmacro define-simple-command-invoker (name args event-type &key message-event-initarg documentation)
  (let ((event (gensym "EVENT"))
        (fun-kargs (loop for arg in (lambda-fiddle:extract-lambda-vars args)
                         collect (kw arg) collect arg)))
    `(define-command-invoker ,name (,event ,@args)
       ,documentation
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
                      (handler-bind ((error (lambda (e)
                                              (maybe-invoke-debugger e 'abort-handling))))
                        ,@body)
                    (error (,error)
                      (v:warn :maiden.agents.commands ,error)
                      (reply ,event "~a" ,error)))))
              (define-simple-command-invoker (,name ,(or command (string name))) ,args ,event-type
                :message-event-initarg :dispatch-event
                :documentation ,(form-fiddle:lambda-docstring `(lambda () ,@body)))
              (list ',consumer ',name)))))

(defun remove-command (consumer name &optional (event-type name) (command (string name)))
  (remove-function-handler consumer name event-type)
  (remove-command-invoker command))

(defun find-matching-command (message)
  (let ((match NIL)
        (alternatives ()))
    (loop for command in *invokers*
          for prefix = (prefix command)
          do (let* ((cut (subseq message 0 (min (length message) (length prefix))))
                    (distance (levenshtein-distance prefix cut)))
               (when (and (= 0 distance)
                          (or (= (length message)
                                 (length prefix))
                              (char= #\  (aref message (length prefix))))
                          (or (null match) (< (length (prefix match))
                                              (length prefix))))
                 (setf match command))
               (when (< distance *alternative-distance-threshold*)
                 (push (cons distance command) alternatives))))
    (values match alternatives)))
