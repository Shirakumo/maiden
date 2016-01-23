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
  `(deeds:do-issue ,event ,@args :loop *event-loop*))

(defmacro define-event (name direct-superclasses direct-slots &rest options)
  `(deeds:define-event ,name ,direct-superclasses
     ,direct-slots
     ,@options))

(defmacro define-handler ((name ev) args &body body)
  `(deeds:define-handler (,name ,ev) ,args
     :loop *event-loop*
     ,@body))

(define-handler (block-bridge deeds:event) (ev)
  :class 'deeds:locally-blocking-handler
  (deeds:issue ev *block-loop*))

(defclass block-handler (deeds:one-time-handler)
  ((cvar :initform (bt:make-condition-variable) :accessor cvar)
   (lock :initform (bt:make-lock) :accessor lock)
   (event :initform NIL :accessor event)))

(defun make-block-handler (event-type filter)
  (let ((handler NIL))
    (flet ((handler (ev)
             (setf (event handler) ev)
             ;; Quickly access lock to make sure the issuer has
             ;; entered the condition-wait.
             (bt:with-lock-held ((lock handler)))
             (unwind-protect
                  (bt:condition-notify (cvar handler))
               (deeds:deregister-handler handler *block-loop*))))
      (setf handler (make-instance
                     'block-handler
                     :event-type event-type
                     :filter filter
                     :delivery-function #'handler))
      (deeds:register-handler handler *block-loop*)
      handler)))

(defmacro with-response (issue response (&key filter timeout) &body body)
  (let ((handler (gensym "HANDLER")))
    (destructuring-bind (issue-event &rest issue-args) (ensure-list issue)
      (destructuring-bind (response-event &optional (event 'ev) &rest response-args) (ensure-list response)
        `(let ((,handler (make-block-handler ',response-event ',filter)))
           (unwind-protect
                (bt:with-lock-held ((lock ,handler))
                  (do-issue ,issue-event ,@issue-args)
                  (when (bt:condition-wait (cvar ,handler) (lock ,handler) :timeout ,timeout)
                    (let ((,event (event ,handler)))
                      (declare (ignorable ,event))
                      (deeds:with-fuzzy-slot-bindings ,response-args (,event ,response-event)
                        ,@body))))
             (deeds:deregister-handler ,handler *block-loop*)
             (deeds:stop ,handler)))))))


