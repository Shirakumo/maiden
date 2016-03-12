#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:colleen-user)
(define-module #:colleen-relay
  (:nicknames #:org.shirakumo.colleen.clients.relay)
  (:use #:cl #:colleen #:colleen-serialize)
  ;; client.lisp
  (:export
   #:relay-client
   #:remote
   #:process)
  ;; conditions.lisp
  (:export
   #:relay-condition
   #:carrier-condition
   #:message
   #:target-condition
   #:target
   #:no-relay-target-specified
   #:relay-route-not-found
   #:relay-link-not-found
   #:client-version-mismatch
   #:remote-version)
  ;; containers.lisp
  (:export
   #:subscription-update
   #:target
   #:subscriber
   #:subscription
   #:event-type
   #:filter
   #:unsubscription
   #:network-update
   #:new
   #:bad
   #:make-network-update
   #:transport
   #:make-transport)
  ;; events.lisp
  (:export
   #:relay-instruction-event
   #:query-event
   #:source
   #:response-event
   #:response
   #:slot-event
   #:slot
   #:object
   #:slot-value-event
   #:slot-setf-event
   #:slot-makunbound-event
   #:slot-boundp-event
   #:generic-call-event
   #:form)
  ;; relay.lisp
  (:export
   #:relay
   #:subscriptions
   #:my-subscriptions
   #:routable-p
   #:update
   #:relay)
  ;; virtual-client.lisp
  (:export
   #:virtual-client
   #:links
   #:make-virtual-client
   #:define-virtual-client-method))
