#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.lichat)

(defun parse-event (stream)
  (let ((sexpr (lichat-protocol:read-sexpr stream)))
    (when (consp sexpr)
      (unless (typep (first sexpr) 'symbol)
        (error 'lichat-protocol:malformed-wire-object :update sexpr))
      (setf (car sexpr) (or (find-symbol (string (car sexpr)) #:org.shirakumo.maiden.clients.lichat.rpl)
                            (error 'lichat-protocol:unknown-wire-object :update sexpr)))
      (lichat-protocol:check-update-options sexpr)
      (apply #'make-instance sexpr))))

(defun print-event (event stream)
  (lichat-protocol:print-sexpr
   `(,(find-symbol (string (class-name (class-of event))) #:lichat-protocol)
     ,@(loop for slot in (c2mop:class-slots (class-of wireable))
             for initarg = (car (last (c2mop:slot-definition-initargs slot)))
             when name collect initarg
             when name collect (slot-value wireable (c2mop:slot-definition-name slot))))
   stream))

(defmacro define-update (name superclasses args)
  (let ((name-cmd (intern (string name) '#:org.shirakumo.maiden.clients.lichat.cmd))
        (name-rpl (intern (string name) '#:org.shirakumo.maiden.clients.lichat.rpl))
        (pure-args (lambda-fiddle:extract-lambda-vars args))
        (client (gensym "CLIENT")))
    `(progn
       (define-event ,name (,@superclasses incoming-event passive-event)
         ,(maiden::slot-args->slots args))
       (define-event ,name-rpl (,name incoming-event passive-event)
         ())
       (define-event ,name-cmd (,name instruction-event outgoing-event active-event)
         ())
       (defun ,name-cmd (,client ,@(maiden::slot-args->args args))
         (do-issue (first (cores ,client)) ,name-cmd
           :from (username ,client)
           :client ,client
           ,@(loop for var in (rest pure-args) collect (kw var) collect var))))))

(defclass update (user-event lichat-protocol:wire-object)
  ((id :initarg :id :accessor id)
   (clock :initarg :clock :accessor clock)
   (user :initarg :from))
  (:default-initargs
   :id (lichat-protocol:next-id)
   :clock (get-universal-time)))

(defmethod initialize-instance :after ((event update) &key client user from)
  (unless (typep (or user from) 'user)
    (deeds:with-immutable-slots-unlocked ()
      (setf (slot-value event 'user) (ensure-user (or user from) client)))))

(defclass channel-update (update channel-event)
  ())

(defmethod initialize-instance :after ((event channel-update) &key client channel)
  (unless (typep channel 'channel)
    (deeds:with-immutable-slots-unlocked ()
      (setf (slot-value event 'channel) (ensure-channel channel client)))))

(defclass target-update (update)
  ((target :initarg :target :accessor target))
  (:default-initargs
   :target (error "TARGET required.")))

(defclass text-update (update message-event)
  ((message :initarg :text)))

(define-update ping (update)
  ())

(define-update pong (update)
  ())

(define-update connect (update)
  (password &optional (version (lichat-protocol:protocol-version))))

(define-update disconnect (update)
  ())

(define-update register (update)
  (password))

(define-update join (channel-update user-entered)
  (channel))

(define-update leave (channel-update user-left)
  (channel))

(define-update create (channel-update)
  (channel))

(define-update kick (channel-update target-update)
  (target channel))

(define-update pull (channel-update target-update)
  (target channel))

(define-update permissions (channel-update)
  (&optional (permissions ())))

(define-update message (channel-update text-update)
  (channel text))

(define-update users (channel-update)
  (&optional (users ())))

(define-update channels (update)
  (&optional (channels ())))

(define-update user-info (target-update)
  (&optional (registered NIL) (connections 1)))

(define-update failure (text-update)
  (&optional text))

(define-update malformed-update (lichat-rpl:failure)
  (&optional text))

(define-update connection-unstable (lichat-rpl:failure)
  (&optional text))

(define-update too-many-connections (lichat-rpl:failure)
  (&optional text))

(define-update update-failure (lichat-rpl:failure)
  (update-id &optional text))

(define-update invalid-update (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update username-mismatch (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update incompatible-version (lichat-rpl:update-failure)
  (compatible-versions update-id &optional text))

(define-update invalid-password (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update no-such-profile (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update username-taken (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update no-such-channel (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update already-in-channel (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update not-in-channel (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update channelname-taken (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update bad-name (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update insufficient-permissions (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update invalid-permissions (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update no-such-user (lichat-rpl:update-failure)
  (update-id &optional text))

(define-update too-many-updates (lichat-rpl:update-failure)
  (update-id &optional text))
