#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden)

(defgeneric users (client))
(defgeneric find-user (name client))
(defgeneric authenticate (user client))
(defgeneric channels (client))
(defgeneric find-channel (name client))

(define-consumer client () ())

(define-consumer user-client (client)
  ())

(defmethod find-user (name (client user-client))
  NIL)

(defmethod users ((client user-client))
  ())

(defmethod ensure-user ((name string) (client user-client))
  (or (find-user name client)
      (make-instance 'user :name name :client client)))

(defmethod authenticate (user (client user-client))
  (authenticate (ensure-user user client) client))

(defmethod authenticate ((user user) (client user-client))
  NIL)

(define-consumer channel-client (client)
  ())

(defmethod find-channel (name (client channel-client))
  NIL)

(defmethod channels ((client channel-client))
  ())

(defmethod ensure-channel ((name string) (client channel-client))
  (or (find-channel name client)
      (make-instance 'channel :name name :client client)))
