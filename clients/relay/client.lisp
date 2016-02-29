#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.relay)

(define-event connection-initiated (client-event)
  ())

(define-event connection-closed (client-event)
  ())

(define-event reachable-clients (client-event)
  ((clients :initarg :clients :reader clients)))

(define-event unreachable-clients (client-event)
  ((clients :initarg :clients :reader clients)))

(defclass transport ()
  ((event :initarg :event :accessor event)
   (target :initarg :target :accessor target)))

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

(defmethod relay ((event client-event) (relay relay))
  (let* ((target (find-target (client event) (network relay)))
         (core (find target (cores relay) :key #'name))
         (client (find target (clients relay) :key #'remote)))
    (when core
      (deeds:with-immutable-slots-unlocked ()
        (setf (slot-value event 'client) (consumer target core))))
    (cond (core (issue event core))
          (client (issue event client))
          (T (error "Don't know")))))

(defmethod relay ((transport transport) (relay relay))
  (let* ((target (find-target (target transport) (network relay)))
         (core (find target (cores relay) :test #'matches))
         (client (find target (clients relay) :key #'remote)))
    (cond (core (issue (event transport) core))
          (client (issue transport client))
          (T (error "Don't know.")))))

(define-handler (relay relay deeds:event) (relay event)
  (loop for (test target) in (watchers relay)
        do (when (watching event test)
             (relay event relay))))

(define-command (relay connect) (relay ev &key (host "127.0.0.1") (port 9486))
  (start (make-instance 'relay-client-initiator
                        :server relay
                        :port port
                        :host host)))

(define-consumer relay-client (tcp-server-client ping-client)
  ((remote :initform NIL :accessor remote)))

(defmethod handle-connection-error :before (err (client relay-client))
  (v:warn :colleen.relay.client "~a Connection error: ~a" client err))

(defmethod handle-connection ((client relay-client))
  (handler-bind ((colleen:client-timeout-error
                   (lambda (err)
                     (ignore-errors (send `(:timeout ,(timeout err)) client)))))
    (call-next-method)))

(defmethod process (message (client relay-client))
  (v:info :colleen.relay.client "~a Processing ~s" client message)
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
  (send (list :id (id (server client))) client))

(defmethod close-connection :before ((client relay-client))
  (ignore-errors (send '(:close) client)))

(defmethod close-connection :after ((client relay-client))
  (broadcast 'connection-closed :client client :loop (cores (server client))))

(define-consumer relay-client-initiator (reconnecting-client relay-client)
  ())

(defmethod handle-connection-idle ((client relay-client-initiator))
  (send '(:ping) client))

#|
(ql:quickload :colleen-relay)
(in-package :colleen-relay)
(defvar *core-a* (start (make-instance 'core :name "core-a")))
(defvar *core-b* (start (make-instance 'core :name "core-b")))
(defvar *relay-a* (start (make-instance 'relay :name "relay-a" :port 9486)))
(defvar *relay-b* (start (make-instance 'relay :name "relay-b" :port 9487)))
(add-consumer *relay-a* *core-a*)
(add-consumer *relay-b* *core-b*)
(connect :port 9486 :loop *core-b*)
|#
