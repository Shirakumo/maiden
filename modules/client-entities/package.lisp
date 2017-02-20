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
   #:username
   #:find-user
   #:authenticate
   #:channel-client
   #:find-channel
   #:user-container
   #:user-map
   #:remove-channel
   #:channel-container
   #:channel-map
   #:remove-user
   #:simple-user-channel-client
   #:simple-user
   #:simple-channel)
  ;; entities.lisp
  (:export
   #:client-entity
   #:client
   #:user
   #:username
   #:authenticated
   #:ensure-user
   #:authenticated-p
   #:channels
   #:channel
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
   #:old-topic
   #:user-entered
   #:user-left))

(use-package '#:maiden-client-entities '#:maiden-user)
