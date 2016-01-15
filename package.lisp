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
   #:*event-loop*
   #:define-handler
   #:define-event)
  ;; event.lisp
  (:export
   #:client-event
   #:client)
  ;; toolkit.lisp
  (:export
   #:kw
   #:ensure-list
   #:unlist
   #:with-default-encoding))
