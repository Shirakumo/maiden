#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(defpackage #:maiden
  (:nicknames #:org.shirakumo.maiden)
  (:use #:cl #:deeds)
  (:shadow
   #:event-class
   #:event
   #:define-event
   #:define-handler
   #:define-command
   #:with-response
   #:with-awaiting
   #:do-issue
   #:broadcast
   #:message-event
   #:info-event
   #:warning-event
   #:error-event)
  ;; re-export from deeds
  (:export
   #:running
   #:start
   #:stop
   #:issue
   #:cancel)
  ;; agent.lisp
  (:export
   #:agent)
  ;; client.lisp
  (:export
   #:client)
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
   #:define-function-handler
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
   #:make-simple-core
   #:core-simple-add)
  ;; entity.lisp
  (:export
   #:matches
   #:entity
   #:id
   #:named-entity
   #:name
   #:data-entity
   #:data
   #:data-value)
  ;; event.lisp
  (:export
   #:advice
   #:event
   #:define-event)
  ;; standard-events.lisp
  (:export
   #:passive-event
   #:active-event
   #:instruction-event
   #:respond
   #:query-event
   #:response-event
   #:client-event
   #:client
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
   #:broadcast
   #:named-lambda
   #:universal-to-unix
   #:unix-to-universal
   #:get-unix-time
   #:format-relative-time
   #:format-absolute-time
   #:format-time
   #:find-consumer-in-package))

(defpackage #:maiden-user
  (:nicknames #:org.shirakumo.maiden.user)
  (:use #:cl #:maiden))
