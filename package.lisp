#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(modularize:define-module #:colleen
  (:nicknames #:org.shirakumo.colleen)
  (:use #:cl #:modularize #:deeds)
  (:shadow #:define-handler)
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
  ;; component.lisp
  (:export
   #:component-class
   #:handlers
   #:component
   #:cores
   #:start
   #:stop
   #:define-handler
   #:define-component)
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
  ;; core.lisp
  (:export
   #:matches
   #:entity
   #:named-entity
   #:name
   #:id
   #:core
   #:event-loop
   #:block-loop
   #:consumers
   #:consumer
   #:add-consumer
   #:remove-consumer
   #:handler
   #:register-handler
   #:deregister-handler
   #:core-event-loop
   #:core-block-loop)
  ;; event.lisp
  (:export
   #:respond
   #:client-event
   #:client
   #:sender-event
   #:sender
   #:message-event
   #:message)
  ;; toolkit.lisp
  (:export
   #:kw
   #:ensure-list
   #:unlist
   #:with-default-encoding))

(defpackage #:colleen-user
  (:nicknames #:org.shirakumo.colleen.user)
  (:use #:cl #:colleen))
