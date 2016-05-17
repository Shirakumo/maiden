#|
 This file is a part of Maiden
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden)

(defgeneric users (client))
(defgeneric user (name client))
(defgeneric authenticate (sender client))
(defgeneric channels (client))
(defgeneric channel (name client))

(define-consumer client () ())

(define-consumer user-client (client)
  ())

(defmethod user (name (client user-client))
  NIL)

(defmethod users ((client user-client))
  ())

(defmethod ensure-user ((name string) (client user-client))
  (or (user name client)
      (make-instance 'user :name name :client client)))

(defmethod authenticate (user (client user-client))
  NIL)

(define-consumer channel-client (client)
  ())

(defmethod channels ((client channel-client))
  ())

(defmethod channel (name (client channel-client))
  NIL)

(defmethod ensure-channel ((name string) (client channel-client))
  (or (channel name client)
      (make-instance 'channel :name name :client client)))
