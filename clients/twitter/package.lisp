#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-twitter
  (:nicknames #:org.shirakumo.maiden.clients.twitter)
  (:use #:cl #:maiden #:maiden-client-entities)
  (:shadow #:user #:channel)
  (:export
   #:twitter-client
   #:api-key
   #:api-secret
   #:access-token
   #:access-secret
   #:login
   #:send-event
   #:send
   #:user
   #:channel
   #:update-channel
   #:update-status
   #:reply-to
   #:status
   #:object
   #:direct-message
   #:object))
