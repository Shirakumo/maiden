#|
 This file is a part of Maiden
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(defpackage #:maiden
  (:nicknames #:org.shirakumo.maiden)
  (:use #:cl #:deeds)
  (:shadow #:define-handler #:define-command #:with-response #:with-awaiting)
  ;; re-export from deeds
  (:export
   #:running
   #:start
   #:stop
   #:event
   #:define-event
   #:issue)
  (:shadow
   #:do-issue
   #:broadcast
   #:message-event
   #:info-event
   #:warning-event
   #:error-event)
  ;; agent.lisp
  (:export
   #:agent)
  ;; client.lisp
  (:export
   #:users
   #:user
   #:authenticate
   #:channels
   #:channel
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
   #:channel-client
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
   #:server
   #:socket)
  ;; conditions.lisp
  (:export
   #:maiden-condition
   #:core-condition
   #:core
   #:consumer-name-duplicated-warning
   #:existing-consumer
   #:new-consumer
   #:agent-condition
   #:agent
   #:agent-already-exists-error
   #:existing-agent
   #:client-condition
   #:client
   #:client-connection-failed-error
   #:client-still-connected-error
   #:client-reconnection-exceeded-error
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
   #:direct-handlers
   #:handlers
   #:instances
   #:consumer
   #:handlers
   #:cores
   #:lock
   #:abstract-handler
   #:target-class
   #:options
   #:name
   #:instantiate-handler
   #:define-handler
   #:define-instruction
   #:define-query
   #:define-consumer)
  ;; core.lisp
  (:export
   #:consumer
   #:add-consumer
   #:remove-consumer
   #:core
   #:event-loop
   #:block-loop
   #:consumers
   #:with-awaiting
   #:with-response)
  ;; entity.lisp
  (:export
   #:matches
   #:entity
   #:id
   #:named-entity
   #:name
   #:client-entity
   #:server
   #:user
   #:ensure-user
   #:channel
   #:ensure-channel
   #:users)
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
   #:executed-instruction
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
   #:xor
   #:xnor
   #:kw
   #:enlist
   #:unlist
   #:starts-with
   #:with-default-encoding
   #:update-list
   #:with-retry-restart
   #:do-issue
   #:broadcast))

(defpackage #:maiden-user
  (:nicknames #:org.shirakumo.maiden.user)
  (:use #:cl #:maiden))
