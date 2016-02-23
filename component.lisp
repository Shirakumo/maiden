#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defclass component-class (standard-class)
  ((handlers :initform () :accessor handlers)))

(defmethod c2mop:validate-superclass ((class component-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass component-class))
  T)

(defmethod c2mop:validate-superclass ((class component-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class component-class) (superclass component-class))
  T)

(defclass component ()
  ((handlers :initform () :accessor handlers))
  (:metaclass component-class))

(defmethod initialize-instance :after ((component component) &key)
  (dolist (handler (handlers (class-of component)))
    (push (instantiate-handler handler component) (handlers component))))

(defmethod add-consumer :after ((component component) (core core))
  (dolist (handler (handlers component))
    (register-handler handler core)))

(defmethod remove-consumer :after ((component component) (core core))
  (dolist (handler (handlers component))
    (deregister-handler handler core)))

(defclass abstract-handler ()
  ((target-class :initarg :target-class :accessor target-class)
   (options :initarg :options :accessor options))
  (:default-initargs
   :target-class 'queued-handler
   :options ()))

(defmethod initialize-instance :after ((handler abstract-handler) &rest args &key &allow-other-keys)
  (let ((class (getf args :target-class)))
    (when class (setf (target-class handler) class))
    (setf (options handler) (deeds::removef args :target-class))))

;; What to do if we need specialising on the particular component?
(defmethod instantiate-handler ((handler abstract-handler) (component component))
  (apply #'make-instance
         (target-class handler)
         :delivery-function (lambda (event)
                              (funcall (getf (options handler) :delivery-function) component event))
         (deeds::removef (options handler) :delivery-function)))

(defun make-abstract-handler (target-class &rest options)
  (apply #'make-instance 'abstract-handler :target-class target-class :options options))

(defmacro define-handler ((component name event-type) args &body body)
  (destructuring-bind (compvar event &rest args) args
    (multiple-value-bind (options body) (deeds::parse-into-kargs-and-body body)
      (let ((class (or (getf options :class) 'queued-handler))
            (options (deeds::removef options :class)))
        `(pushnew (make-abstract-handler
                   ',class
                   :name ',name
                   :event-type ',event-type
                   :delivery-function (lambda (,compvar ,event)
                                        (with-origin (',name)
                                          (with-fuzzy-slot-bindings ,args (,event ,event-type)
                                            ,@body)))
                   ,@options)
                  (handlers (find-class ',component) :test #'name))))))

(defmacro define-component (name direct-superclasses direct-slots &rest options)
  (unless (loop for super in direct-superclasses
                never (c2mop:subclassp (find-class super) (find-class 'component)))
    (push 'component direct-superclasses))
  (unless (find :metaclass options :key #'first)
    (push `(:metaclass component-class) options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))
