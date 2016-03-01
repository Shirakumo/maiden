#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

#|
  Alright.

  Todo:
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
- Connection re-establishment should work properly now,
  but we still don't have a safe protocol for communicating
  such things.
- Currently the subscription system is missing
- Consider changing the system to not require a network on
  the relay at all and instead have it be realised through 
  virtual clients. This might even be absolutely necessary
  so that virtual clients can be accessed through the core
  from oblivious consumers.
  - Each virtual client must have an internal list of hops
    necessary to reach their actual client associated with
    each possible relay client to use.
  - Each virtual client must be removed when their network
    is empty and re-established when they appear anew.
  - I'm not sure whether it's better to have a client-tied
    handler for each virtual client to intercept the events
    (that seems natural) or for efficiency reason stick to
    the one "global" handler for the relay.
|#

(in-package #:org.shirakumo.colleen.clients.relay)

(define-event connection-initiated (client-event)
  ())

(define-event connection-closed (client-event)
  ())

(defclass transport ()
  ((event :initarg :event :accessor event)
   (target :initarg :target :accessor target)))

(defclass network-update ()
  ((new :initarg :new :accessor new)
   (bad :initarg :bad :accessor bad))
  (:default-initargs
   :new () :bad ()))

(defclass subscribe ()
  ((to :initarg :to :accessor to)
   (subscriber :initarg :subscriber :accessor subscriber)
   (event-type :initarg :event-type :accessor event-type)
   (filter :initarg :filter :accessor filter))
  (:default-initargs
   :to T
   :subscriber (error "SUBSCRIBER required.")
   :event-type (error "EVENT-TYPE required.")
   :filter T))

(define-consumer virtual-client (client)
  ())

(define-consumer relay (tcp-server agent)
  ((network :initform NIL :accessor network)
   (watchers :initform NIL :accessor watchers))
  (:default-initargs
   :host "127.0.0.1"
   :port 9486))

(defmethod handle-connection :before ((server tcp-server))
  (v:info :colleen.relay.server "~a waiting for clients." server))

(defmethod accept :before (socket (server tcp-server))
  (v:info :colleen.relay.server "~a accepting client." server))

(defmethod make-tcp-server-client ((server tcp-server) socket)
  (make-instance 'relay-client
                 :server server
                 :socket socket
                 :host (usocket:get-peer-address socket)
                 :port (usocket:get-peer-port socket)
                 :name NIL))

(defmethod make-network-update ((new relay) (bad list))
  (make-network-update (mapcar #'butlast (network new)) bad))

(defmethod make-network-update ((new list) (bad relay))
  (make-network-update new (mapcar #'second (network bad))))

(defmethod make-network-update ((new list) (bad list))
  (make-instance 'network-update :new new :bad bad))

(defmethod make-network-update ((new network-update) (special null))
  (make-instance 'network-update
                 :new (loop for (hops destination next) in (new new)
                            collect (list (1+ hops) destination next))
                 :bad (bad new)))

(defmethod update-network ((relay relay) source update)
  (let ((network ()))
    (loop for link in (network relay)
          for (hops destination next) = link
          do (unless (and (matches next source)
                          (find destination (bad update) :test #'matches))
               (push link network)))
    (loop for (hops destination) in (new update)
          do (pushnew (list hops destination source) network :test #'equal))
    (setf (network relay) (sort network #'< :key #'first)))
  (v:info :colleen.relay.server "~a updated network to:~%~s" relay (network relay))
  relay)

(defmethod update-network :after ((relay relay) source update)
  (let ((update (make-network-update update NIL)))
    (dolist (client (clients relay))
      (unless (matches source (remote client))
        (send update client)))))

(defmethod find-target (id (relay relay))
  (loop for (hops destination next) in (network relay)
        do (when (matches id destination)
             (return next))
        finally (return id)))

(defmethod find-target ((transport transport) (relay relay))
  (find-target (target transport) relay))

(defmethod find-target ((named-entity named-entity) (relay relay))
  (find-target (id named-entity) relay))

(defmethod relay ((event client-event) (relay relay))
  (let* ((target (find-target (client event) relay))
         (core (find target (cores relay) :test #'matches))
         (client (find target (clients relay) :key #'remote :test #'matches)))
    (cond (core
           (deeds:with-immutable-slots-unlocked ()
             (setf (slot-value event 'client) (consumer (id (client event)) core)))
           (issue event core))
          (client
           (deeds:with-immutable-slots-unlocked ()
             (when (typep (event-loop event) 'core)
               (setf (slot-value event 'event-loop) (id (event-loop event)))))
           (issue event client))
          (T (error "Don't know")))))

(defmethod relay ((transport transport) (relay relay))
  (let* ((target (find-target (target transport) relay))
         (core (find target (cores relay) :test #'matches))
         (client (find target (clients relay) :key #'remote :test #'matches)))
    (cond (core (issue (event transport) core))
          (client (issue transport client))
          (T (error "Don't know.")))))

(define-handler (relay relay deeds:event) (relay event)
  (when (and (typep event 'client-event) (typep (client event) 'virtual-client))
    (relay event relay))
  (loop for (event-type filter target) in (watchers relay)
        do (when (and (typep event event-type) (deeds:test-filter filter event))
             (relay (make-instance 'transport :target target :event event) relay))))

(defmethod add-consumer :after ((relay relay) (core core))
  (update-network relay (id core) (make-network-update (loop for c in (consumers core) collect (list 0 (id c))) ())))

(defmethod remove-consumer :after ((relay relay) (core core))
  (update-network relay (id core) (make-network-update NIL (mapcar #'id (consumers core)))))

(define-handler (relay consumer-added consumer-added) (relay ev consumer event-loop)
  (update-network relay (id event-loop) (make-network-update `((0 ,(id consumer))) ())))

(define-handler (relay consumer-removed consumer-removed) (relay ev consumer event-loop)
  (update-network relay (id event-loop) (make-network-update () `(,(id consumer)))))

(define-command (relay connect) (relay ev &key (host "127.0.0.1") (port 9486))
  (start (make-instance 'relay-client-initiator
                        :server relay
                        :port port
                        :host host)))

(define-consumer relay-client (tcp-server-client timeout-client)
  ((remote :initform NIL :accessor remote)))

(defmethod handle-connection-error :before (err (client relay-client))
  (v:warn :colleen.relay.client "~a Connection error: ~a" client err))

(defmethod handle-connection ((client relay-client))
  (handler-bind ((colleen:client-timeout-error
                   (lambda (err)
                     (ignore-errors (send `(:timeout ,(timeout err)) client)))))
    (call-next-method)))

(defmethod process (message (client relay-client))
  ;; (v:info :colleen.relay.client "~a Processing ~s" client message)
  (typecase message
    (list
     (case (first message)
       (:id
        (setf (remote client) (second message)))
       (:timeout
        (error 'colleen:client-timeout-error :client client :timeout (second message)))
       (:close
        (close-connection client))
       (:ping
        (send '(:pong) client))
       (:pong)))
    ((or deeds:event transport)
     (relay message (server client)))
    (network-update
     (update-network (server client) (remote client) message))
    (T
     (warn 'unknown-message-warning :message message :client client))))

(defmethod receive ((client relay-client))
  (deserialize (usocket:socket-stream (socket client))))

(defmethod send (message (client relay-client))
  (serialize message (usocket:socket-stream (socket client))))

(defmethod issue (thing (client relay-client))
  (send thing client))

(defmethod initiate-connection :after ((client relay-client))
  (broadcast 'connection-initiated :client client :loop (cores (server client)))
  (send (list :id (id (server client))) client)
  (send (make-network-update (server client) ()) client))

(defmethod close-connection :before ((client relay-client))
  ;; Remove all links through this client.
  (update-network (server client) (remote client)
                  (make-network-update
                   () (loop for (hops dest next) in (network (server client))
                            when (matches next (remote client))
                            collect dest)))
  (ignore-errors (send '(:close) client)))

(defmethod close-connection :after ((client relay-client))
  (broadcast 'connection-closed :client client :loop (cores (server client))))

(define-consumer relay-client-initiator (reconnecting-client relay-client)
  ())

(defmethod handle-connection-idle ((client relay-client-initiator))
  (send '(:ping) client))

#|
(ql:quickload '(colleen-logger colleen-relay))
(in-package :colleen-relay)
(defvar *core-a* (start (make-instance 'core :name "core-a")))
(defvar *core-b* (start (make-instance 'core :name "core-b")))
(defvar *logger-a* (start (make-instance 'colleen-logger::logger :name "logger-a")))
(defvar *relay-a* (start (make-instance 'relay :name "relay-a" :port 9486)))
(defvar *relay-b* (start (make-instance 'relay :name "relay-b" :port 9487)))
(add-consumer *logger-a* *core-a*)
(add-consumer *relay-a* *core-a*)
(add-consumer *relay-b* *core-b*)
(connect :port 9486 :loop *core-b*)
(issue (make-instance 'colleen-logger::log-event :client (make-instance 'virtual-client :id (id *logger-a*)) :message "HI!!") *core-b*)
|#
