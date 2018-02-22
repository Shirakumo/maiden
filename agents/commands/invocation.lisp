#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.commands)

(defvar *invokers* ())

(defvar *framework-client* (make-instance 'user-client :name "FRAMEWORK"))
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
   (invoker :initarg :invoker :accessor invoker)))

(defmethod print-object ((command-invoker command-invoker) stream)
  (print-unreadable-object (command-invoker stream :type T)
    (format stream "~a" (name command-invoker))))

(defmethod documentation ((command-invoker command-invoker) type)
  (documentation (invoker command-invoker) T))

(defmethod (setf documentation) (value (command-invoker command-invoker) type)
  (setf (documentation (invoker command-invoker) T) value))

(defmethod documentation (slot (type (eql 'command)))
  (documentation (command-invoker slot) T))

(defmethod (setf documentation) (docstring slot (type (eql 'command)))
  (setf (documentation (command-invoker slot) T) docstring))

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
                                       ,(form-fiddle:lambda-docstring `(lambda () ,@body))
                                       (with-command-destructuring-bind ,lambda-list ,message
                                         ,@body)))))))

(defmacro define-simple-command-invoker (name args event-type &key message-event-initarg documentation)
  (let ((event (gensym "EVENT"))
        (fun-kargs (loop for arg in (let ((lambda-fiddle:*lambda-keywords* (cons '&string lambda-fiddle:*lambda-keywords*)))
                                      (lambda-fiddle:extract-lambda-vars args))
                         collect (kw arg) collect arg)))
    `(define-command-invoker ,name (,event ,@args)
       ,documentation
       (issue (make-instance ',event-type
                             ,@(when message-event-initarg `(,message-event-initarg ,event))
                             ,@fun-kargs)
              (core ,event)))))

(defmacro define-command ((consumer name &optional (event-type name)) (instance event &rest args) &body body)
  (form-fiddle:with-body-options (body options superclasses command command-event-variable) body
    (let ((command-event-variable (or command-event-variable (gensym "COMMAND-EVENT")))
          (error (gensym "ERROR")))
      `(progn (define-function-handler (,consumer ,name ,event-type) (,instance ,command-event-variable ,@(remove '&string args))
                :superclasses (,@superclasses command-event)
                ,@options
                (let* ((,event (dispatch-event ,command-event-variable))
                       (*dispatch-event* ,event))
                  (handler-case
                      (handler-bind ((error #'maybe-invoke-debugger))
                        ,@body)
                    (error (,error)
                      (v:debug :maiden.agents.commands ,error)
                      (reply ,event "~a" ,error)))))
              (define-simple-command-invoker (,name ,(or command (string name))) ,args ,event-type
                :message-event-initarg :dispatch-event
                :documentation ,(form-fiddle:lambda-docstring `(lambda () ,@body)))
              (list ',consumer ',name)))))

(defun remove-command (consumer name &optional (event-type name) (command (string name)))
  (remove-function-handler consumer name event-type)
  (remove-command-invoker command))

(defun flatten-typespec (type)
  (if (listp type)
      (loop for item in (rest type)
            nconc (flatten-typespec item))
      (list type)))

(defun consumer-commands (consumer)
  (loop for handler in (effective-handlers (class-of consumer))
        when (loop for type in (flatten-typespec (getf (options handler) :event-type))
                   thereis (c2mop:subclassp (find-class type)
                                            (find-class 'command-event)))
        collect (command-invoker (name handler))))

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

(defun levenshtein-distance (a b)
  (cond ((= 0 (length a)) (length b))
        ((= 0 (length b)) (length a))
        (T
         (let ((v0 (make-array (1+ (length b))))
               (v1 (make-array (1+ (length b)))))
           (dotimes (i (length v0)) (setf (aref v0 i) i))
           (dotimes (i (length a) (aref v1 (length b)))
             (incf (aref v1 0))
             (dotimes (j (length b))
               (let ((cost (if (char= (char a i) (char b j)) 0 1)))
                 (setf (aref v1 (1+ j)) (min (1+ (aref v1 j))
                                             (1+ (aref v0 (1+ j)))
                                             (+ cost (aref v0 j))))))
             (dotimes (j (length v0))
               (setf (aref v0 j) (aref v1 j))))))))
