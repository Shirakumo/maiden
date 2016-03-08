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
   #:connection-initiated
   #:connection-closed
   #:relay-client
   #:remote
   #:relay-client-initiator)
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
   #:client-version-mismatch)
  ;; containers.lisp
  (:export
   #:subscription-update
   #:target
   #:subscriber
   #:subscription
   #:event-type
   #:filter
   #:unsubscription
   #:transport
   #:event
   #:target
   #:make-transport
   #:network-update
   #:new
   #:bad
   #:make-network-update
   #:virtual-client
   #:links
   #:make-virtual-clients)
  ;; relay.lisp
  (:export
   #:relay
   #:subscriptions
   #:my-subscriptions
   #:routable-p
   #:update
   #:relay
   #:connect
   #:subscribe
   #:unsubscribe))
