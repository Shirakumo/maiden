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
  (update (server client) (remote client) subscription))

(defmethod process ((update network-update) (client relay-client))
  ;; Create new network update to increase hops
  (update (server client) (remote client) (make-network-update update NIL)))

(defmethod process ((message list) (client relay-client))
  (case (first message)
    (:id
     (destructuring-bind (id version) (cdr message)
       (unless (equal version (asdf:component-version (asdf:find-system :colleen)))
         (warn 'client-version-mismatch :remote-version version :client client))
       (setf (remote client) id)))
    (:timeout
     (error 'colleen:client-timeout-error :client client :timeout (second message)))
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
  (serialize message (usocket:socket-stream (socket client))))

(defmethod issue (thing (client relay-client))
  (send thing client))

(defmethod initiate-connection :after ((client relay-client))
  (broadcast 'connection-initiated :client client :loop (cores (server client)))
  (send (list :id (id (server client)) (asdf:component-version (asdf:find-system :colleen))) client)
  (send (make-network-update (server client) ()) client))

(defmethod close-connection :before ((client relay-client))
  ;; Remove all links through this client.
  (update (server client) (remote client) (make-network-update () (list (remote client))))
  (ignore-errors (send '(:close) client)))

(defmethod close-connection :after ((client relay-client))
  (broadcast 'connection-closed :client client :loop (cores (server client))))

(define-consumer relay-client-initiator (reconnecting-client relay-client)
  ())

(defmethod handle-connection-idle ((client relay-client-initiator))
  (send '(:ping) client))

;; FIXME: On connection re-establishment, subscriptions need to be re-sent
