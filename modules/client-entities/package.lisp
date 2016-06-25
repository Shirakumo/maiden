#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-client-entities
  (:nicknames #:org.shirakumo.maiden.modules.client-entities)
  (:use #:cl #:maiden)
  ;; clients.lisp
  (:export
   #:user-client
   #:find-user
   #:authenticate
   #:channel-client
   #:find-channel)
  ;; entities.lisp
  (:export
   #:client-entity
   #:send-to
   #:client
   #:user
   #:authenticated
   #:ensure-user
   #:authenticated-p
   #:channels
   #:channel
   #:topic
   #:ensure-channel
   #:users)
  ;; events.lisp
  (:export
   #:user-event
   #:user
   #:user-removed
   #:user-added
   #:user-name-changed
   #:message-event
   #:message
   #:reply
   #:channel-event
   #:channel
   #:channel-topic-changed
   #:old-topic
   #:user-entered
   #:user-left))
