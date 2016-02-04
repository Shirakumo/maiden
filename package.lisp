#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(modularize:define-module #:colleen
  (:nicknames #:org.shirakumo.colleen)
  (:use #:cl #:modularize)
  ;; re-export from modularize
  (:export
   #:virtual-module
   #:virtual-module-name
   #:define-module
   #:define-module-extension
   #:delete-module
   #:module
   #:module-p
   #:module-storage
   #:module-storage-remove
   #:module-identifier
   #:module-name
   #:current-module)
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

(defpackage #:colleen-user
  (:nicknames #:org.shirakumo.colleen.user)
  (:use #:cl #:colleen))
