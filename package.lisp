#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(defpackage #:maiden
  (:nicknames #:org.shirakumo.maiden)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
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
   #:effective-handlers
   #:instances
   #:consumer
   #:handlers
   #:cores
   #:lock
   #:core-handlers
   #:abstract-handler
   #:target-class
   #:options
   #:name
   #:add-to-consumer
   #:instantiate-handler
   #:define-handler
   #:remove-handler
   #:define-function-handler
   #:remove-function-handler
   #:define-instruction
   #:remove-instruction
   #:define-query
   #:remove-query
   #:define-consumer)
  ;; core.lisp
  (:export
   #:consumer
   #:add-consumer
   #:remove-consumer
   #:core
   #:abort-handling
   #:primary-loop
   #:block-loop
   #:consumers
   #:with-awaiting
   #:make-core
   #:add-to-core)
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
   #:consumer-removed)
  ;; toolkit.lisp
  (:export
   #:*root*
   #:*debugger*
   #:maybe-invoke-debugger
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
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  (:use #:cl #:maiden))
