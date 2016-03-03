#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

#|
  Todo:
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
- Currently the subscription system is missing.
|#

(in-package #:org.shirakumo.colleen.clients.relay)

(define-event connection-initiated (client-event)
  ())

(define-event connection-closed (client-event)
  ())

(defclass subscription ()
  ((target :initarg :target :accessor target)
   (subscriber :initarg :subscriber :accessor subscriber)
   (event-type :initarg :event-type :accessor event-type)
   (filter :initarg :filter :accessor filter))
  (:default-initargs
   :target T
   :subscriber (error "SUBSCRIBER required.")
   :event-type (error "EVENT-TYPE required.")
   :filter T))

(defclass transport ()
  ((event :initarg :event :accessor event)
   (target :initarg :target :accessor target)))

(defmethod print-object ((transport transport) stream)
  (print-unreadable-object (transport stream :type T)
    (format stream "~s ~s ~s" :to (target transport) (event transport))))

(defmethod make-transport ((event event) target)
  (make-instance 'make-transport :event event :target target))

(defclass network-update ()
  ((new :initarg :new :accessor new)
   (bad :initarg :bad :accessor bad))
  (:default-initargs
   :new () :bad ()))

(defmethod print-object ((update network-update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~s ~s ~s" :new (new update) :bad (bad update))))

(defmethod make-network-update ((new list) (bad relay))
  (make-network-update new (mapcar #'second (network bad))))

(defmethod make-network-update ((new list) (bad list))
  (make-instance 'network-update :new new :bad bad))

(defmethod make-network-update ((new network-update) (special null))
  (make-instance 'network-update
                 :new (loop for (hops destination) in (new new)
                            collect (list (1+ hops) destination))
                 :bad (bad new)))

(define-consumer virtual-client (client)
  ((links :initarg :links :accessor links))
  (:default-initargs
   :links ()))

(defmethod make-virtual-client ((target uuid:uuid) &optional links)
  (make-instance 'virtual-client :id target :links links))

(defmethod make-virtual-client ((target string) &optional links)
  (make-virtual-client (uuid:make-uuid-from-string target) links))

(defmethod make-virtual-client ((target named-entity) &optional links)
  (make-virtual-client (id target) links))

(define-consumer relay (tcp-server agent)
  ((subscriptions :initform NIL :accessor subscriptions))
  (:default-initargs
   :host "127.0.0.1"
   :port 9486))

(defmethod make-network-update ((relay relay) (bad list))
  (let ((clients ()))
    (dolist (core (cores relay))
      (dolist (consumer (consumers core))
        (unless (typep consumer 'virtual-client)
          (push (list 0 (id consumer)) clients))))
    (make-network-update clients bad)))

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

(defmethod routable-p (target (relay relay))
  (loop for core in (cores relay)
        thereis (or (matches target core)
                    (find target (consumers core) :test #'matches))))

(defmethod routable-p ((subscription subscription) (relay relay))
  (routable-p (subscriber subscription) relay))

(defmethod routable-p ((transport transport) (relay relay))
  (routable-p (target transport) relay))

(defmethod routable-p ((event client-event) (relay relay))
  (routable-p (client event) relay))

(defmethod update-network ((relay relay) source update)
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

(defmethod update-network :after ((relay relay) source update)
  (dolist (client (clients relay))
    (unless (matches (remote client) source)
      (send update client))))

(defmethod update-subscriptions ((relay relay) (self null) (subscription subscription))
  (pushnew subscription (subscriptions relay)))

(defmethod update-subscriptions ((relay relay) source (subscription subscription))
  (etypecase (target subscription)
    (null)
    (virtual-client
     (relay subscription (target subscription) relay))
    (relay
     (update-subscriptions (target subscription) NIL subscription))
    ((eql T)
     (update-subscriptions relay NIL subscription)
     (dolist (client (clients relay))
       (unless (or (matches (remote client) source)
                   (matches (subscriber subscription) source))
         (send subscription client))))))

(defmethod relay ((event event) (core core) (relay relay))
  (deeds:with-immutable-slots-unlocked ()
    (setf (slot-value event 'event-loop) core))
  (issue event core))

(defmethod relay (message (client virtual-client) (relay relay))
  (let ((remote (find (second (first (links client)))
                      (clients relay) :key #'remote :test #'matches)))
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
  (update-network relay (id core) (make-network-update (loop for c in (consumers core) collect (list 0 (id c))) ())))

(defmethod remove-consumer :after ((relay relay) (core core))
  (update-network relay (id core) (make-network-update NIL (mapcar #'id (consumers core)))))

(define-handler (relay consumer-added consumer-added) (relay ev consumer event-loop)
  (unless (typep consumer 'virtual-client)
    (update-network relay (id event-loop) (make-network-update `((0 ,(id consumer))) ()))))

(define-handler (relay consumer-removed consumer-removed) (relay ev consumer event-loop)
  (unless (typep consumer 'virtual-client)
    (update-network relay (id event-loop) (make-network-update () `(,(id consumer))))))

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
  (warn 'unknown-message-warning :message message :client client))

(defmethod process ((transport transport) (client relay-client))
  (relay transport (target transport) (server client)))

(defmethod process ((event client-event) (client relay-client))
  (relay event (client event) (server client)))

(defmethod process ((subscription subscription) (client relay-client))
  (update-subscriptions subscription (remote client) (server client)))

(defmethod process ((update network-update) (client relay-client))
  ;; Create new network update to increase hops
  (update-network (server client) (remote client)
                  (make-network-update update NIL)))

(defmethod process ((message list) (client relay-client))
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

(defmethod receive ((client relay-client))
  (deserialize (usocket:socket-stream (socket client))
               `((core . ,(lambda (id) (find id (cores (server client)) :test #'matches)))
                 (consumer . ,(lambda (id) (consumer id (cores (server client))))))))

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
                  (make-network-update () (list (remote client))))
  (ignore-errors (send '(:close) client)))

(defmethod close-connection :after ((client relay-client))
  (broadcast 'connection-closed :client client :loop (cores (server client))))

(define-consumer relay-client-initiator (reconnecting-client relay-client)
  ())

(defmethod handle-connection-idle ((client relay-client-initiator))
  (send '(:ping) client))

;; FIXME: On connection re-establishment, subscriptions need to be re-sent

#|
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
(consumer (id *logger-a*) *core-b*)
(issue (make-instance 'colleen-logger:log-event :client (consumer (id *logger-a*) *core-b*) :message "HI!!") *core-b*)
|#
