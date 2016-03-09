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
 (ql:quickload :verbose)
 (setf (v:repl-level) :trace)
 (ql:quickload '(colleen-logger colleen-relay))
 (in-package :colleen-relay)
 
 ;; A - B - C*
 (macrolet ((define-relay (name port)
             `(let ((core (start (make-instance 'core :name ,(string name))))
                    (relay (start (make-instance 'relay :name ,(string name) :port ,port))))
                (add-consumer relay core)
                (defvar ,(intern (format NIL "*CORE-~a*" name)) core)
                (defvar ,(intern (format NIL "*RELAY-~a*" name)) relay))))
  (define-relay "A" 9486)
  (define-relay "B" 9487)
  (define-relay "C" 9488))
 (connect *core-a* :port (port *relay-b*))
 (connect *core-b* :port (port *relay-c*))
 (defvar *logger-c* (start (make-instance 'colleen-logger:logger :name "logger-c")))
 (add-consumer *logger-c* *core-c*)
 
 ;; Test logger
 (issue (make-instance 'colleen-logger:log-event :client (consumer (id *logger-c*) *core-a*) :message "HI!!") *core-a*)
 ;; Test subscriptions
 (deeds:define-handler (foo deeds:info-event) (ev)
   :loop (event-loop *core-c*)
   (v:info :test "~a" (deeds:message ev)))
 (subscribe *core-c* 'deeds:info-event T T)
 (issue (make-instance 'deeds:info-event :message "Hrm.") *core-b*)
 (issue (make-instance 'deeds:info-event :message "Hrm.") *core-a*)
::
|#

(define-consumer relay (tcp-server agent)
  ((subscriptions :initform NIL :accessor subscriptions)
   (my-subscriptions :initform NIL :accessor my-subscriptions))
  (:default-initargs
   :host "127.0.0.1"
   :port 9486))

(defmethod initiate-connection :around ((relay relay))
  (when (and (host relay) (port relay))
    (call-next-method)))

(defmethod make-network-update ((relay relay) bad)
  (let ((links ()))
    (dolist (core (cores relay))
      (push (list 0 (id core)) links)
      (dolist (consumer (consumers core))
        (unless (typep consumer '(or agent virtual-client))
          (push `(0 ,(id consumer) ,(name consumer)) links))))
    (make-network-update links bad)))

(defmethod make-network-update (new (relay relay))
  (let ((links ()))
    (dolist (core (cores relay))
      (push (list 0 (id core)) links)
      (dolist (consumer (consumers core))
        (unless (typep consumer '(or agent virtual-client))
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
               (setf (links consumer) (remove source (links consumer) :key #'second :test #'matches))))
    ;; Insert new links and virtual clients.
    (loop for (hops new name) in (new update)
          for consumer = (consumer new core)
          do (cond ((typep consumer 'virtual-client)
                    (pushnew (list hops new) (links consumer) :key #'second :test #'matches))
                   ((not consumer)
                    (add-consumer (make-virtual-client new :name name :links `((,hops ,source))) core)
                    ;; Since we discovered a new client we have to check if it
                    ;; might need to be notified of one of our subscriptions
                    ;; and if so tell it about it.
                    (dolist (subscription (my-subscriptions relay))
                      (when (or (eql T (target subscription))
                                (matches new (target subscription)))
                        (relay subscription new relay))))))
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
    (when (and (client-connected-p client)
               (not (matches (remote client) source)))
      (send update client))))

(defmethod update ((relay relay) (self null) (subscription subscription))
  (pushnew subscription (subscriptions relay) :test #'matches))

(defmethod update ((relay relay) (self null) (unsubscription unsubscription))
  (setf (subscriptions relay) (remove unsubscription (subscriptions relay) :test #'matches)))

(defmethod update ((relay relay) source (subscription subscription-update))
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
                   (and (typep subscription 'subscription)
                        (matches (remote client) (subscriber subscription))))
         (send subscription client))))))

(defgeneric relay (message target relay))

(defmethod relay (message (target null) relay)
  (error 'no-relay-target-specified :message message :client relay))

(defmethod relay (message target relay)
  (relay message (or (find-entity target relay)
                     (error 'relay-route-not-found :message message :target target :client relay)) relay))

(defmethod relay (message (target entity) relay)
  (error "Don't know how to relay ~s to ~a over ~a" message target relay))

(defmethod relay ((event event) (core core) (relay relay))
  (deeds:with-immutable-slots-unlocked ()
    (setf (slot-value event 'event-loop) core)
    (setf (slot-value event 'deeds:origin) 'relay))
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
  (update relay (id core) (make-network-update core ())))

(defmethod remove-consumer :after ((relay relay) (core core))
  (update relay (id core) (make-network-update () core)))

(define-handler (relay consumer-added consumer-added) (relay ev consumer event-loop)
  (etypecase consumer
    ((or relay virtual-client))
    (consumer (update relay (id event-loop) (make-network-update consumer ())))))

(define-handler (relay consumer-removed consumer-removed) (relay ev consumer event-loop)
  (etypecase consumer
    ((or relay virtual-client))
    (consumer (update relay (id event-loop) (make-network-update () consumer)))))

(define-handler (relay relay deeds:event) (relay event)
  :filter '(not (eql origin 'relay))
  (when (and (typep event 'client-event)
             (typep (client event) 'virtual-client))
    (relay event (client event) relay))
  (loop for subscription in (subscriptions relay)
        do (when (and (or (eql T (target subscription))
                          (matches (event-loop event) (target subscription)))
                      (not (matches (event-loop event) (subscriber subscription)))
                      ;; Perform actual event test.
                      (typep event (event-type subscription))
                      (deeds:test-filter (filter subscription) event))
             (relay event (subscriber subscription) relay))))

(define-command (relay connect) (relay ev &key (host "127.0.0.1") (port 9486))
  (start (make-instance 'relay-client-initiator
                        :server relay
                        :port port
                        :host host)))

(define-command (relay subscribe) (relay ev event-type filter &optional (target T))
  (let ((subscription (make-instance 'subscription :event-type event-type
                                                   :filter filter
                                                   :subscriber (event-loop ev)
                                                   :target target)))
    (push subscription (my-subscriptions relay))
    (update relay relay subscription)))

(define-command (relay unsubscribe) (relay ev subscription)
  (let ((subscription (or (typecase subscription
                            (subscription subscription)
                            (uuid:uuid (find subscription (my-subscriptions relay) :test #'matches)))
                          (error "No matching subscription found for ~a" subscription))))
    (setf (my-subscriptions relay) (remove subscription (my-subscriptions relay)))
    (update relay relay (make-instance 'unsubscription :target (target subscription) :id (id subscription)))))
