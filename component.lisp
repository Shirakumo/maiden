#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defclass component-class (standard-class)
  ((handlers :initform () :accessor handlers)
   (instances :initform () :accessor instances)))

(defmethod c2mop:validate-superclass ((class component-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass component-class))
  T)

(defmethod c2mop:validate-superclass ((class component-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class component-class) (superclass component-class))
  T)

(defmethod (setf handlers) :after (handlers (class component-class))
  (setf (instances class)
        (loop for pointer in (instances class)
              for component = (trivial-garbage:weak-pointer-value pointer)
              when component
              collect (prog1 pointer 
                        (reinitialize-handlers component (handlers class))))))

(defclass component (named-entity)
  ((handlers :initform () :accessor handlers)
   (cores :initform () :accessor cores))
  (:metaclass component-class))

(defmethod initialize-instance :after ((component component) &key)
  (push (trivial-garbage:make-weak-pointer component) (instances (class-of component)))
  (dolist (handler (handlers (class-of component)))
    (push (instantiate-handler handler component) (handlers component))))

(defmethod reinitialize-handlers ((component component) handlers)
  (v:info :colleen.core.component "~a updating handlers." component)
  (let ((cores (cores component)))
    ;; Deregister
    (remove-consumer component cores)
    ;; Rebuild
    (stop component)
    (setf (handlers component) ())
    (dolist (handler handlers)
      (push (instantiate-handler handler component) (handlers component)))
    (start component)
    ;; Reregister
    (add-consumer component cores)))

(defmethod add-consumer :after ((component component) (core core))
  (register-handler (handlers component) core)
  (push core (cores component)))

(defmethod remove-consumer ((component component) (everywhere (eql T)))
  (dolist (core (cores component))
    (remove-consumer component core)))

(defmethod remove-consumer :after ((component component) (core core))
  (deregister-handler (handlers component) core)
  (setf (cores component) (remove core (cores component))))

(defmethod start ((component component))
  (start (handlers component))
  component)

(defmethod stop ((component component))
  (stop (handlers component))
  component)

(defclass abstract-handler ()
  ((target-class :initarg :target-class :accessor target-class)
   (options :initarg :options :accessor options)
   (name :initarg :name :accessor name))
  (:default-initargs
   :target-class 'queued-handler
   :options ()))

(defmethod initialize-instance :after ((handler abstract-handler) &rest args &key &allow-other-keys)
  (let ((class (getf args :target-class)))
    (when class (setf (target-class handler) class))
    (setf (options handler) (deeds::removef args :target-class :options))))

(defmethod instantiate-handler ((handler abstract-handler) (component component))
  (let* ((options (options handler))
         (filter (getf options :filter))
         (delivery (getf options :delivery-function))
         (match (getf options :match-component)))
    ;; Extend filter to match component.
    (when match
      (if (eql match T)
          (setf filter `(and (eq ,component component) ,filter))
          (setf filter `(and (eq ,component ,match) ,filter))))
    (apply #'make-instance
           (target-class handler)
           :delivery-function (lambda (event) (funcall delivery component event))
           :filter filter
           (deeds::removef options :delivery-function :filter :match-component))))

(defmacro define-handler ((component name event-type) args &body body)
  (destructuring-bind (compvar event &rest args) args
    (multiple-value-bind (options body) (deeds::parse-into-kargs-and-body body)
      (let ((class (or (getf options :class) 'queued-handler))
            (options (deeds::removef options :class)))
        `(progn
           (update-list (make-instance
                         'abstract-handler
                         :target-class ',class
                         :name ',name
                         :event-type ',event-type
                         :delivery-function (lambda (,compvar ,event)
                                              (declare (ignorable ,compvar ,event))
                                              (with-origin (',name)
                                                (with-fuzzy-slot-bindings ,args (,event ,event-type)
                                                  ,@body)))
                         ,@options)
                        (handlers (find-class ',component))
                        :key #'name)
           (list ',component ',name))))))

(defmacro define-component (name direct-superclasses direct-slots &rest options)
  (when (loop for super in direct-superclasses
              never (c2mop:subclassp (find-class super) (find-class 'component)))
    (push 'component direct-superclasses))
  (unless (find :metaclass options :key #'first)
    (push `(:metaclass component-class) options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))
