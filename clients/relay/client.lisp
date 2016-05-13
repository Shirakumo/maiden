#|
 This file is a part of Maiden
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.relay)

(define-consumer relay-client (tcp-server-client timeout-client)
  ((remote :initform NIL :accessor remote)))

(defmethod make-network-update ((new list) (client relay-client))
  (let ((bad ()))
    (dolist (core (cores (server client)))
      (dolist (consumer (consumers core))
        (when (and (typep consumer 'virtual-client)
                   (find (remote client) (links consumer) :test #'matches :key #'second))
          (push (id consumer) bad))))
    (make-network-update new bad)))

(defmethod handle-connection-error :before (err (client relay-client))
  (v:warn :maiden.relay.client "~a Connection error: ~a" client err))

(defmethod handle-connection ((client relay-client))
  (handler-bind ((maiden:client-timeout-error
                   (lambda (err)
                     (ignore-errors (send `(:timeout ,(timeout err)) client)))))
    (call-next-method)))

(defmethod process (message (client relay-client))
  (warn 'unknown-message-warning :message message :client client))

(defmethod process ((transport transport) (client relay-client))
  (relay transport (target transport) (server client)))

(defmethod process ((event client-event) (client relay-client))
  (relay event (client event) (server client)))

(defmethod process ((subscription subscription-update) (client relay-client))
  (update (server client) (remote client) subscription))

(defmethod process ((update network-update) (client relay-client))
  ;; Create new network update to increase hops
  (update (server client) (remote client) (make-network-update update NIL)))

(defmethod process ((message list) (client relay-client))
  (case (first message)
    (:id
     (destructuring-bind (id version) (cdr message)
       (unless (equal version (asdf:component-version (asdf:find-system :maiden)))
         (warn 'client-version-mismatch :remote-version version :client client))
       (setf (remote client) id)))
    (:timeout
     (error 'maiden:client-timeout-error :client client :timeout (second message)))
    (:close
     (close-connection client))
    (:ping
     (send '(:pong) client))
    (:pong)))

(defmethod receive ((client relay-client))
  (let ((finder (lambda (id) (find-entity id (cores (server client))))))
    (deserialize (usocket:socket-stream (socket client))
                 `((core . ,finder) (consumer . ,finder)))))

(defmethod send (message (client relay-client))
  (let ((socket (usocket:socket-stream (socket client))))
    (when socket
      (serialize message socket))))

(defmethod issue (thing (client relay-client))
  (send thing client))

(defmethod initiate-connection :after ((client relay-client))
  (broadcast 'connection-initiated :client client :loop (cores (server client)))
  (send (list :id (id (server client)) (asdf:component-version (asdf:find-system :maiden))) client)
  (send (make-network-update (server client) ()) client))

(defmethod close-connection :before ((client relay-client))
  ;; Remove all links through this client.
  (with-simple-restart (continue "Ignore the update.")
    (update (server client) (remote client) (make-network-update () client)))
  (ignore-errors (send '(:close) client)))

(defmethod close-connection :after ((client relay-client))
  (broadcast 'connection-closed :client client :loop (cores (server client))))

(define-consumer relay-client-initiator (reconnecting-client relay-client)
  ())

(defmethod handle-connection-idle ((client relay-client-initiator))
  (send '(:ping) client))
