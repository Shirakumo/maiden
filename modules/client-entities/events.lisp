#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.client-entities)

(define-event user-event (client-event)
  ((user :initarg :user :reader user))
  (:default-initargs
   :user (error "USER required.")))

(define-event user-removed (user-event)
  ())

(define-event user-added (user-event)
  ())

(define-event user-name-changed (user-event)
  ((old-name :initarg :old-name :reader old-name))
  (:default-initargs
   :old-name (error "OLD-NAME required.")))

(define-event message-event (user-event)
  ((message :initarg :message :reader message :mutable T))
  (:default-initargs
   :message (error "MESSAGE required.")))

(defgeneric reply (event format-string &rest format-args))

(define-event channel-event (client-event)
  ((channel :initarg :channel :reader channel))
  (:default-initargs
   :channel (error "CHANNEL required.")))

(define-event channel-topic-changed (channel-event)
  ((old-topic :initarg :old-topic :reader old-topic))
  (:default-initargs
   :topic (error "TOPIC required.")))

(define-event user-entered (user-event channel-event)
  ())

(define-event user-left (user-event channel-event)
  ())

(defmethod reply ((event channel-event) fmst &rest args)
  (apply #'reply (channel event) fmst args))
