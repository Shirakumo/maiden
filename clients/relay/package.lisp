#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-relay
  (:nicknames #:org.shirakumo.maiden.clients.relay)
  (:use #:cl #:maiden #:maiden-serialize #:maiden-networking)
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
   #:data-response-event
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
   #:relay
   #:connect
   #:subscribe
   #:unsubscribe)
  ;; virtual-client.lisp
  (:export
   #:virtual-client
   #:links
   #:make-virtual-client
   #:define-virtual-client-method))
