#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.relay)

#|
## Todo:
- Document everything.
- Insurance that all the same modules are loaded across the
  different cores, otherwise serialising of event objects
  is going to fail.
  - This might need a better solution, one that does not
    require every module along the path to have the modules
    loaded and only the endpoints. The usefulness of this
    needs to be evaluated however.
  - Additionally, some objects simply cannot be serialised
    so there might have to be a mechanism to automate rmeoval
    or substitution thereof.
- Automate error handling.

## Testing Code
:: common-lisp
(ql:quickload '(colleen-logger colleen-relay))
(in-package :colleen-relay)
(defvar *core-a* (start (make-instance 'core :name "core-a")))
(defvar *core-b* (start (make-instance 'core :name "core-b")))
(defvar *logger-a* (start (make-instance 'colleen-logger:logger :name "logger-a")))
(defvar *relay-a* (start (make-instance 'relay :name "relay-a" :port 9486)))
(defvar *relay-b* (start (make-instance 'relay :name "relay-b" :port 9487)))
(add-consumer *logger-a* *core-a*)
(add-consumer *relay-a* *core-a*)
(add-consumer *relay-b* *core-b*)
(connect :port 9486 :loop *core-b*)

(issue (make-instance 'colleen-logger:log-event :client (consumer (id *logger-a*) *core-b*) :message "HI!!") *core-b*)

(deeds:define-handler (foo deeds:info-event) (ev)
  :loop (event-loop *core-b*)
  (v:info :test "~a" (deeds:message ev)))
(subscribe 'deeds:info-event T T *core-b*)
(issue (make-instance 'deeds:info-event :message "Hrm.") *core-b*)
(issue (make-instance 'deeds:info-event :message "Hrm.") *core-a*)
::
|#

(define-consumer relay (tcp-server agent)
  ((subscriptions :initform NIL :accessor subscriptions))
  (:default-initargs
   :host "127.0.0.1"
   :port 9486))

(defmethod make-network-update ((relay relay) (bad list))
  (let ((links ()))
    (dolist (core (cores relay))
      (push (list 0 (id core)) links)
      (dolist (consumer (consumers core))
        (unless (typep consumer 'virtual-client)
          (push (list 0 (id consumer)) links))))
    (make-network-update links bad)))

(defmethod make-network-update ((new list) (relay relay))
  (let ((links ()))
    (dolist (core (cores relay))
      (push (list 0 (id core)) links)
      (dolist (consumer (consumers core))
        (unless (typep consumer 'virtual-client)
          (push (id consumer) links))))
    (make-network-update new links)))

(defmethod handle-connection :before ((server tcp-server))
  (v:info :colleen.relay.server "~a waiting for clients." server))

(defmethod accept :before (socket (server tcp-server))
  (v:info :colleen.relay.server "~a accepting client." server))

(defmethod make-tcp-server-client ((server relay) socket)
  (make-instance 'relay-client
                 :server server
                 :socket socket
                 :host (usocket:get-peer-address socket)
                 :port (usocket:get-peer-port socket)
                 :name NIL))

(defgeneric routable-p (target network))

(defmethod routable-p (a b)
  NIL)

(defmethod routable-p (target (relay relay))
  (find-entity target relay))

(defmethod routable-p ((subscription subscription) (relay relay))
  (routable-p (subscriber subscription) relay))

(defmethod routable-p ((transport transport) (relay relay))
  (routable-p (target transport) relay))

(defmethod routable-p ((event client-event) (relay relay))
  (routable-p (client event) relay))

(defgeneric update (relay update-source update))

(defmethod update ((relay relay) source (update network-update))
  ;; FIXME: optimise bulk actions to a single pass if possible.
  ;; Remove links from virtual clients.  
  (dolist (core (cores relay))
    (loop for bad in (bad update)
          for consumer = (consumer bad core)
          do (when (and consumer (typep consumer 'virtual-client))
               (setf (links consumer) (remove source (links consumer) :key #'second))))
    ;; Insert new links and virtual clients.
    (loop for (hops new) in (new update)
          for consumer = (consumer new core)
          do (cond ((typep consumer 'virtual-client)
                    (pushnew (list hops new) (links consumer) :key #'second :test #'matches))
                   ((not consumer)
                    (add-consumer (make-virtual-client new `((,hops ,source))) core))))
    ;; Remove empty virtual clients
    (dolist (consumer (consumers core))
      (when (and (typep consumer 'virtual-client) (null (links consumer)))
        (remove-consumer consumer core))))
  ;; Remove unroutable subscriptions
  (setf (subscriptions relay)
        (remove-if-not (lambda (sub) (routable-p sub relay)) (subscriptions relay)))
  (v:info :colleen.relay.server "~a updated network by ~a" relay update)
  relay)

(defmethod update :after ((relay relay) source (update network-update))
  (dolist (client (clients relay))
    (unless (matches (remote client) source)
      (send update client))))

(defmethod update ((relay relay) (self null) (subscription subscription))
  (pushnew subscription (subscriptions relay) :test #'matches :key #'id))

(defmethod update ((relay relay) source (subscription subscription))
  (etypecase (target subscription)
    (null)
    (virtual-client
     (relay subscription (target subscription) relay))
    (relay
     (update (target subscription) NIL subscription))
    ((eql T)
     (update relay NIL subscription)
     (dolist (client (clients relay))
       (unless (or (matches (remote client) source)
                   (matches (subscriber subscription) source))
         (send subscription client))))))

(defgeneric relay (message target relay))

(defmethod relay (message (target null) relay)
  (error 'no-relay-target-specified :message message :client relay))

(defmethod relay (message target relay)
  (relay message (or (find-entity target relay)
                     (error 'relay-route-not-found :message message :target target :client relay)) relay))

(defmethod relay ((event event) (core core) (relay relay))
  (deeds:with-immutable-slots-unlocked ()
    (setf (slot-value event 'event-loop) core))
  (issue event core))

(defmethod relay ((transport transport) (core core) (relay relay))
  (relay (event transport) core relay))

(defmethod relay (message (client virtual-client) (relay relay))
  (let ((remote (find (second (first (links client)))
                      (clients relay) :key #'remote :test #'matches)))
    (unless remote (error 'relay-link-not-found :message message :target client :client relay))
    (issue message remote)))

(defmethod relay ((event event) (client virtual-client) (relay relay))
  (if (typep event 'client-event)
      (call-next-method)
      (relay (make-transport event client) client relay)))

(defmethod relay (message (consumer consumer) (relay relay))
  (let ((core (dolist (core (cores relay))
                (when (find consumer (consumers core))
                  (return core)))))
    (relay message core relay)))

(defmethod relay (message (all (eql T)) (relay relay))
  (dolist (client (clients relay))
    (send message client)))

(defmethod add-consumer :after ((relay relay) (core core))
  (update relay (id core) (make-network-update (loop for c in (consumers core) collect (list 0 (id c))) ())))

(defmethod remove-consumer :after ((relay relay) (core core))
  (update relay (id core) (make-network-update NIL (mapcar #'id (consumers core)))))

(define-handler (relay consumer-added consumer-added) (relay ev consumer event-loop)
  (unless (typep consumer 'virtual-client)
    (update relay (id event-loop) (make-network-update `((0 ,(id consumer))) ()))))

(define-handler (relay consumer-removed consumer-removed) (relay ev consumer event-loop)
  (unless (typep consumer 'virtual-client)
    (update relay (id event-loop) (make-network-update () `(,(id consumer))))))

(define-handler (relay relay deeds:event) (relay event)
  (when (and (typep event 'client-event)
             (typep (client event) 'virtual-client))
    (relay event (client event) relay))
  (loop for subscription in (subscriptions relay)
        do (when (and (typep event (event-type subscription))
                      (deeds:test-filter (filter subscription) event))
             (relay event (subscriber subscription) relay))))

(define-command (relay connect) (relay ev &key (host "127.0.0.1") (port 9486))
  (start (make-instance 'relay-client-initiator
                        :server relay
                        :port port
                        :host host)))

(define-command (relay subscribe) (relay ev event-type filter &optional (target T))
  (relay (make-instance 'subscription :event-type event-type
                                      :filter filter
                                      :subscriber (event-loop ev)
                                      :target target)
         T relay))
