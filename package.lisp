#|
 This file is a part of Maiden
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(modularize:define-module #:maiden
  (:nicknames #:org.shirakumo.maiden)
  (:use #:cl #:modularize #:deeds)
  (:shadow #:define-handler #:define-command #:with-response #:with-awaiting)
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
  ;; re-export from deeds
  (:export
   #:event
   #:define-event
   #:do-issue
   #:issue
   #:broadcast)
  (:shadow
   #:message-event
   #:info-event
   #:warning-event
   #:error-event)
  ;; agent.lisp
  (:export
   #:agent)
  ;; client.lisp
  (:export
   #:authenticate
   #:client-connected-p
   #:close-connection
   #:initiate-connection
   #:handle-connection
   #:handle-connection-error
   #:handle-connection-idle
   #:process
   #:send
   #:receive
   #:accept
   #:make-tcp-server-client
   #:client
   #:user-client
   #:remote-client
   #:ip-client
   #:host
   #:port
   #:socket-client
   #:socket
   #:read-thread
   #:reconnecting-client
   #:failures
   #:max-failures
   #:backoff
   #:interval
   #:timeout-client
   #:timeout
   #:last-received-time
   #:text-client
   #:encoding
   #:buffer
   #:tcp-client
   #:element-type
   #:idle-interval
   #:text-tcp-client
   #:tcp-server
   #:clients
   #:tcp-server-client
   #:server)
  ;; conditions.lisp
  (:export
   #:maiden-condition
   #:core-condition
   #:core
   #:consumer-name-duplicated-warning
   #:existing-consumer
   #:new-consumer
   #:agent-condition
   #:agent-already-exists-error
   #:existing-agent
   #:client-condition
   #:client
   #:client-connection-failed-error
   #:client-still-connected-error
   #:client-connection-closed-uncleanly-warning
   #:closing-error
   #:client-timeout-error
   #:timeout
   #:message-condition
   #:message
   #:message-parse-error
   #:unknown-message-warning)
  ;; consumer.lisp
  (:export
   #:consumer-class
   #:handlers
   #:consumer
   #:cores
   #:lock
   #:start
   #:stop
   #:define-handler
   #:define-instruction
   #:define-query
   #:define-consumer)
  ;; core.lisp
  (:export
   #:matches
   #:entity
   #:named-entity
   #:name
   #:id
   #:find-entity
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
   #:core-block-loop
   #:with-awaiting
   #:with-response)
  ;; event.lisp
  (:export
   #:instruction-event
   #:respond
   #:query-event
   #:response-event
   #:client-event
   #:client
   #:connection-event
   #:connection-initiated
   #:connection-closed
   #:sender-event
   #:sender
   #:message-event
   #:message
   #:reply
   #:core-event
   #:consumer-added
   #:consumer-removed
   #:instruction-event
   #:execute-instruction
   #:core-instruction-event
   #:add-consumer
   #:consumer-type
   #:name
   #:initargs
   #:remove-consumer
   #:name
   #:stop)
  ;; toolkit.lisp
  (:export
   #:kw
   #:ensure-list
   #:unlist
   #:with-default-encoding
   #:with-retry-restart))

(defpackage #:maiden-user
  (:nicknames #:org.shirakumo.maiden.user)
  (:use #:cl #:maiden))
