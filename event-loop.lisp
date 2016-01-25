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

(defmacro with-handler (event-type args &body options-and-body)
  `(deeds:with-handler ,event-type ,args
     :loop *event-loop*
     ,@options-and-body))

(defmacro define-handler ((name ev) args &body options-and-body)
  `(deeds:define-handler (,name ,ev) ,args
     :loop *event-loop*
     ,@options-and-body))

(defmacro with-response (issue response (&rest kargs) &body body)
  `(deeds:with-response (,@(ensure-list issue) :loop *event-loop*) ,response (,@kargs :loop *block-loop*)
     ,@body))

(define-handler (block-bridge deeds:event) (ev)
  :class 'deeds:locally-blocking-handler
  (deeds:issue ev *block-loop*))
