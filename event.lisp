#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden)

(defclass event-class (deeds:event-class)
  ((effective-advice :initform () :accessor advice)
   (direct-advice :initarg :advice :accessor direct-advice))
  (:default-initargs
   :direct-advice ()))

(defmethod c2mop:finalize-inheritance :after ((class event-class))
  (let ((advice ()))
    ;; Calculate inherited advice
    (dolist (super (c2mop:class-direct-superclasses class))
      (when (typep super 'event-class)
        (unless (c2mop:class-finalized-p super)
          (c2mop:finalize-inheritance super))
        (setf advice (union advice (advice super)))))
    ;; Remove blocked advice
    (when (slot-boundp class 'direct-advice)
      (dolist (direct-advice (direct-advice class))
        (cond ((and (listp direct-advice) (eql (first direct-advice) 'not))
               (setf advice (remove (second direct-advice) advice)))
              ((listp direct-advice)
               (setf advice (union advice direct-advice :test #'equalp)))
              (T
               (pushnew direct-advice advice :test #'equalp)))))
    ;; Set effective advice.
    (setf (advice class) advice)))

(defclass event (deeds:event)
  ()
  (:metaclass event-class))

(defmethod advice ((event event))
  (advice (class-of event)))

(defmacro define-event (name direct-superclasses direct-slots &rest options)
  (when (loop for super in direct-superclasses
              never (c2mop:subclassp (find-class super) (find-class 'event)))
    (push 'event direct-superclasses))
  (pushnew `(:metaclass event-class) options
           :test #'(lambda (a b) (eql (car a) (car b))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))
