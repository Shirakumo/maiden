#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defclass event-loop (deeds:compiled-event-loop)
  ())

(defclass block-loop (deeds:event-loop)
  ())

(defmethod deeds:handle :before ((event deeds:event) (event-loop event-loop))
  (v:trace :colleen.event "Handling event ~a" event))

(defvar *event-loop* (deeds:start (make-instance 'event-loop)))
(defvar *block-loop* (deeds:start (make-instance 'block-loop)))

(defmacro do-issue (event &rest args)
  `(deeds:do-issue ',event ,@args :loop *event-loop*))

(defmacro define-event (name direct-superclasses direct-slots &rest options)
  `(deeds:define-event ,name ,direct-superclasses
     ,direct-slots
     ,@options))

(defmacro define-handler ((name ev) args &body body)
  `(deeds:define-handler (,name ,ev) ,args
     :loop *event-loop*
     ,@body))

(define-handler (block-bridge event) (ev)
  :class 'locally-blocking-handler
  (issue ev *block-loop*))

(defclass block-handler (deeds:one-time-handler)
  ((condition :initform (bt:make-condition-variable) :accessor condition)
   (event :initform NIL :accessor event)))

(defun make-block-handler (event-type filter)
  (let ((handler (make-instance
                  'block-handler
                  :event-type event-type
                  :filter filter
                  :delivery-function (lambda (ev)
                                       (setf (event handler) ev)
                                       (unwind-protect
                                            (bt:condition-notify (condition handler))
                                         (deeds:deregister-handler handler *block-loop*))))))
    (register-handler handler *block-loop*)
    handler))

(defmacro with-response (issue response (&key filter timeout) &body body)
  (let ((handler (gensym "HANDLER")))
    (destructuring-bind (issue-event &rest issue-args) (ensure-list issue)
      (destructuring-bind (response-event &optional (event 'ev) &rest response-args) (ensure-list response)
        `(let ((,handler (make-block-handler ,response-event ,filter)))
           (do-issue ,issue-event ,@issue-args)
           (bt:with-lock-held ((deeds:handler-lock ,handler))
             (case (bt:condition-wait (condition ,handler) (deeds:handler-lock ,handler) :timeout ,timeout)
               ((NIL)
                (deeds:deregister-handler ,handler *block-loop*)
                (stop ,handler))
               (T
                (let ((,event (event ,handler)))
                  (deeds:with-fuzzy-slot-bindings ,response-args (,event ,response-event)
                    ,@body))))))))))


