#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.lichat)

(defun parse-event (client stream)
  (let ((sexpr (lichat-protocol:read-sexpr stream)))
    (when (consp sexpr)
      (unless (typep (first sexpr) 'symbol)
        (error 'lichat-protocol:malformed-wire-object :update sexpr))
      (setf (car sexpr) (or (find-symbol (string (car sexpr)) '#:org.shirakumo.maiden.clients.lichat.rpl)
                            (error 'lichat-protocol:unknown-wire-object :update sexpr)))
      (lichat-protocol:check-update-options sexpr)
      (loop for char = (read-char stream NIL)
            until (or (not char) (char= #\Nul char)))
      (apply #'make-instance (first sexpr) :client client (rest sexpr)))))

(defun good-initarg-p (initarg)
  (member initarg '(:id :clock :from :password :version :channel :target :text
                    :permissions :users :channels :registered :connections
                    :update-id :compatible-versions)))

(defun print-event (event stream)
  (lichat-protocol:print-sexpr
   `(,(find-symbol (string (class-name (class-of event))) '#:lichat-protocol)
     ,@(loop for slot in (c2mop:class-slots (class-of event))
             for initarg = (car (remove-if-not #'good-initarg-p (c2mop:slot-definition-initargs slot)))
             for value = (slot-value event (c2mop:slot-definition-name slot))
             when initarg collect initarg
             when initarg collect (typecase value
                                    (lichat-client (username value))
                                    (named-entity (name value))
                                    ((or string list symbol real) value)
                                    (T (error 'lichat-protocol:unprintable-object :object value)))))
   stream)
  (write-char #\Nul stream))

(defmacro define-update (name superclasses args)
  (let ((name-cmd (intern (string name) '#:org.shirakumo.maiden.clients.lichat.cmd))
        (name-rpl (intern (string name) '#:org.shirakumo.maiden.clients.lichat.rpl))
        (pure-args (lambda-fiddle:extract-lambda-vars args))
        (client (gensym "CLIENT")))
    `(progn
       (define-event ,name (,@superclasses incoming-event passive-event)
         ,(maiden::slot-args->slots args))
       (define-event ,name-rpl (lichat-rpl:update ,name)
         ())
       (define-event ,name-cmd (lichat-cmd:update ,name)
         ())
       (defun ,name-cmd (,client ,@(maiden::slot-args->args args))
         (do-issue (first (cores ,client)) ,name-cmd
           :from (username ,client)
           :client ,client
           ,@(loop for var in pure-args
                   collect (intern (string var) :keyword)
                   collect var))))))

(define-event update (user-event lichat-protocol:wire-object)
  ((id :initarg :id :reader id)
   (clock :initarg :clock :reader clock)
   (user :initarg :from))
  (:default-initargs
   :user NIL
   :id (lichat-protocol:next-id)
   :clock (get-universal-time)))

(defmethod initialize-instance :after ((event update) &key client user from)
  (unless (typep (or user from) 'user)
    (deeds:with-immutable-slots-unlocked ()
      (setf (slot-value event 'user) (ensure-user (or user from) client)))))

(defmethod print-object ((update update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a" :from (name (slot-value update 'user))
                                 :id (slot-value update 'id))))

(define-event lichat-rpl:update (update incoming-event passive-event)
  ())

(define-event lichat-cmd:update (update instruction-event outgoing-event)
  ())

(define-event channel-update (update channel-event)
  ())

(defmethod initialize-instance :after ((event channel-update) &key client channel)
  (unless (typep channel 'channel)
    (deeds:with-immutable-slots-unlocked ()
      (setf (slot-value event 'channel) (ensure-channel channel client)))))

(define-event target-update (update)
  ((target :initarg :target :reader target))
  (:default-initargs
   :target (error "TARGET required.")))

(define-event text-update (update message-event)
  ((message :initarg :text))
  (:default-initargs
   :message NIL))

(defmethod print-object ((update text-update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~s" :from (name (slot-value update 'user))
                                       :id (slot-value update 'id)
                                       :text (slot-value update 'text))))

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

(defmethod print-object ((update message) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~s" :from (name (slot-value update 'user))
                                       :channel (name (slot-value update 'channel))
                                       :text (slot-value update 'text))))

(define-update users (channel-update)
  (&optional (users ())))

(define-update channels (update)
  (&optional (channels ())))

(define-update user-info (target-update)
  (&optional (registered NIL) (connections 1)))

(define-update failure (text-update)
  (&optional text))

(define-update malformed-update (failure)
  (&optional text))

(define-update connection-unstable (failure)
  (&optional text))

(define-update too-many-connections (failure)
  (&optional text))

(define-update update-failure (failure)
  (update-id &optional text))

(defmethod print-object ((update update-failure) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~a" :from (name (slot-value update 'user))
                                       :id (slot-value update 'id)
                                       :update-id (slot-value update 'update-id))))

(define-update invalid-update (update-failure)
  (update-id &optional text))

(define-update username-mismatch (update-failure)
  (update-id &optional text))

(define-update incompatible-version (update-failure)
  (compatible-versions update-id &optional text))

(define-update invalid-password (update-failure)
  (update-id &optional text))

(define-update no-such-profile (update-failure)
  (update-id &optional text))

(define-update username-taken (update-failure)
  (update-id &optional text))

(define-update no-such-channel (update-failure)
  (update-id &optional text))

(define-update already-in-channel (update-failure)
  (update-id &optional text))

(define-update not-in-channel (update-failure)
  (update-id &optional text))

(define-update channelname-taken (update-failure)
  (update-id &optional text))

(define-update bad-name (update-failure)
  (update-id &optional text))

(define-update insufficient-permissions (update-failure)
  (update-id &optional text))

(define-update invalid-permissions (update-failure)
  (update-id &optional text))

(define-update no-such-user (update-failure)
  (update-id &optional text))

(define-update too-many-updates (update-failure)
  (update-id &optional text))
