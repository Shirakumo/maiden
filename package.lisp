#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
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
   #:client
   #:user-client
   #:channel-client)
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
   #:client)
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
   #:with-response
   #:make-simple-core)
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
   #:authenticated-p
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
   #:*root*
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
