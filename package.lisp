#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:colleen
  (:nicknames #:org.shirakumo.colleen)
  (:use #:cl)
  ;; client.lisp
  (:export
   #:client
   #:name
   #:client
   #:add-client
   #:remove-client
   #:list-clients
   #:user-client
   #:authenticate
   #:remote-client
   #:client-connected-p
   #:close-connection
   #:server-client
   #:host
   #:port
   #:encoding
   #:initiate-connection)
  ;; conditions.lisp
  (:export
   #:colleen-condition
   #:client-condition
   #:client
   #:client-error
   #:client-warning
   #:client-already-exists-error
   #:existing-client
   #:client-connection-failed-error
   #:client-still-connected-error
   #:client-timeout-error
   #:timeout
   #:message-condition
   #:message
   #:message-parse-error
   #:unknown-message-event-warning)
  ;; event-loop.lisp
  (:export
   #:event-loop
   #:block-loop
   #:*event-loop*
   #:*block-loop*
   #:do-issue
   #:with-event
   #:define-event
   #:define-handler
   #:with-response)
  ;; event.lisp
  (:export
   #:client-event
   #:client
   #:sender-event
   #:sender
   #:message-event
   #:message
   #:reply)
  ;; toolkit.lisp
  (:export
   #:kw
   #:ensure-list
   #:unlist
   #:with-default-encoding))
