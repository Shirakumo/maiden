#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.client-entities)

(define-event user-event (client-event)
  ((user :initarg :user :initform (error "USER required.") :reader user)))

(define-event user-removed (user-event)
  ())

(define-event user-added (user-event)
  ())

(define-event user-name-changed (user-event)
  ((old-name :initarg :old-name :initform (error "OLD-NAME required.") :reader old-name)))

(define-event message-event (user-event)
  ((message :initarg :message :initform (error "MESSAGE required.") :accessor message :mutable T)))

(defgeneric reply (event format-string &rest format-args))

(define-event channel-event (client-event)
  ((channel :initarg :channel :initform (error "CHANNEL required.") :reader channel)))

(define-event user-entered (user-event channel-event)
  ())

(define-event user-left (user-event channel-event)
  ())

(defmethod reply ((event channel-event) fmst &rest args)
  (apply #'reply (channel event) fmst args))
