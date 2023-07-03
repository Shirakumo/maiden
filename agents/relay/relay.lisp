(in-package #:maiden-user)
(defpackage #:maiden-channel-relay
  (:nicknames #:org.shirakumo.maiden.agents.channel-relay)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities)
  (:shadow #:relay)
  (:export
   #:relay
   #:mappings
   #:mapping
   #:id
   #:prefix-id
   #:prefix-user))
(in-package #:org.shirakumo.maiden.agents.channel-relay)

(define-consumer relay (agent)
  ((mappings :accessor mappings)))

(defclass mapping ()
  ((id :initarg :id :accessor id)
   (prefix-id :initarg :prefix-id :accessor prefix-id)
   (prefix-user :initarg :prefix-user :accessor prefix-user))
  (:default-initargs
   :id (error "id required")
   :prefix-id NIL
   :prefix-user T))

(defmethod initialize-instance :after ((relay relay) &key mappings)
  (with-storage (relay)
    (setf (mappings relay) (or mappings (value :mappings) (make-hash-table :test 'equalp)))))

(define-stored-accessor relay mappings :mappings)

(defun channel-id (channel)
  (list (name (client channel))
        (name channel)))

(defun id-channel (id consumer)
  (destructuring-bind (client channel) id
    (dolist (core (cores consumer))
      (let ((consumer (consumer client core)))
        (when consumer
          (return (find-channel channel consumer)))))))

(define-handler (relay message (and channel-event message-event)) (c ev user message channel)
  (dolist (mapping (gethash (channel-id channel) (mappings c)))
    (let ((other (id-channel (id mapping) c)))
      (when other
        ;; FIXME: This could be a lot more effectively done for protocols that support
        ;;        assuming identities like Lichat.
        (reply other "~@[~a~]~@[<~a>~] ~a"
               (when (prefix-id mapping) (id mapping))
               (when (prefix-user mapping) (name user))
               message)))))

(define-command (relay activate) (c ev client channel &key prefix-id (prefix-user T))
  :command "relay from"
  (let ((id (list client channel)))
    (unless (id-channel id c)
      (error "The requested channel is not known."))
    (when (find (channel-id (channel ev)) (gethash id (mappings c)) :test #'equalp :key #'id)
      (error "The requested channel is already relayed here."))
    (when (equalp id (channel-id (channel ev)))
      (error "Why would I relay what's already here?"))
    (push (make-instance 'mapping :id (channel-id (channel ev))
                                  :prefix-id prefix-id
                                  :prefix-user prefix-user)
          (gethash id (mappings c)))
    (setf (mappings c) (mappings c))
    (reply ev "The channel ~a/~a is now being relayed here." client channel)))

(define-command (relay deactivate) (c ev client channel)
  :command "stop relaying from"
  :advice (not public)
  (let ((id (list client channel)))
    (unless (id-channel id c)
      (error "The requested channel is not known."))
    (unless (find (channel-id (channel ev)) (gethash id (mappings c)) :test #'equalp :key #'id)
      (error "The requested channel is not relayed here."))
    (setf (gethash id (mappings c)) (remove (channel-id (channel ev)) (gethash id (mappings c)) :test #'equalp :key #'id))
    (setf (mappings c) (mappings c))
    (reply ev "The channel ~a/~a is no longer relayed here." client channel)))
