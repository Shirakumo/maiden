#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.lichat)

(defun parse-event (client stream)
  (unwind-protect
       (let ((sexpr (handler-bind ((lichat-protocol:unknown-symbol #'continue))
                      (lichat-protocol:read-sexpr stream))))
         (when (consp sexpr)
           (unless (typep (first sexpr) 'symbol)
             (error 'lichat-protocol:malformed-wire-object :update sexpr))
           (setf (car sexpr) (or (find-symbol (string (car sexpr)) '#:org.shirakumo.maiden.clients.lichat.rpl)
                                 (error 'lichat-protocol:unknown-wire-object :update sexpr)))
           (lichat-protocol:check-update-options sexpr)
           (apply #'make-instance (first sexpr) :client client (rest sexpr))))
    (loop for char = (read-char stream NIL)
          until (or (not char) (char= #\Nul char)))))

(defun good-initarg-p (initarg)
  (find initarg #(:id :clock :from :password :version :extensions :channel :target :text
                  :permissions :users :channels :registered :connections :update :permitted
                  :attributes :update-id :compatible-versions :content-type :filename
                  :bridge :link :payload :name :names :keys :key :ip :mask :by :identities)))

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

(defmethod initialize-instance :after ((event update) &key client user from &allow-other-keys)
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
  ((bridge :initarg :bridge :reader bridge))
  (:default-initargs :bridge NIL))

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

(define-update ping (update) ())

(define-update pong (update) ())

(define-update connect (update)
  (password &optional
            (version (lichat-protocol:protocol-version))
            (extensions '("shirakumo-backfill" "shirakumo-data"))))

(define-update disconnect (update) ())

(define-update register (update)
  (password))

(define-update join (channel-update user-entered)
  (channel))

(define-update leave (channel-update user-left)
  (channel))

(define-update create (channel-update)
  (&optional (channel NIL)))

(define-update kick (channel-update target-update)
  (target channel))

(define-update pull (channel-update target-update)
  (target channel))

(define-update permissions (channel-update)
  (channel &optional (permissions ())))

(define-update grant (channel-update target-update)
  (update))

(define-update deny (channel-update target-update)
  (update))

(define-update message (channel-update text-update)
  (channel text &optional link))

(defmethod print-object ((update message) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~s" :from (name (slot-value update 'user))
                                       :channel (name (slot-value update 'channel))
                                       :text (slot-value update 'text))))

(define-update users (channel-update)
  (channel &optional (users ())))

(define-update channels (update)
  (&optional (channel NIL) (channels ())))

(define-update user-info (target-update)
  (target &optional (registered NIL) (connections 1)))

(define-update capabilities (channel-update)
  (channel &optional (permitted ())))

(define-update server-info (target-update)
  (target &optional (attributes ()) (connections ())))

(define-update failure (text-update)
  (&optional text))

(define-update malformed-update (failure)
  (&optional text))

(define-update update-too-long (failure)
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

(define-update already-connected (update-failure)
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

(define-update registration-rejected (update-failure)
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

(define-update clock-skewed (update-failure)
  (update-id &optional text))

(define-update backfill (channel-update)
  (channel))

(define-update data (channel-update)
  (content-type payload &optional (filename NIL)))

(define-update emotes ()
  (content-type name payload))

(define-update emote ()
  (content-type name payload))

(define-update edit (message)
  (id channel text))

(define-update channel-info (channel-update)
  (&optional (keys T)))

(define-update set-channel-info (channel-update text-update)
  (key))

(define-update kill (target-update)
  (target))

(define-update destroy (channel-update)
  (channel))

(define-update ban (target-update)
  (target))

(define-update unban (target-update)
  (target))

(define-update ip-ban ()
  (ip mask))

(define-update ip-unban ()
  (ip mask))

(define-update pause (channel-update)
  (channel by))

(define-update quiet (channel-update target-update)
  (channel target))

(define-update unquiet (channel-update target-update)
  (channel target))

(define-update bridge (channel-update)
  (channel))

(define-update react (channel-update)
  (target update-id emote))

(define-update set-user-info (text-update)
  (key text))

(define-update typing (channel-update)
  (channel))

(define-update share-identity ()
  (&optional key))

(define-update unshare-identity ()
  (&optional key))

(define-update list-shared-identities ()
  (&optional identities))

(define-update assume-identity (target-update)
  (target key))

(define-update bad-content-type (update-failure)
  (update-id &optional allowed-content-types))

(define-update no-such-parent-channel (update-failure)
  (update-id &optional text))

(define-update no-such-channel-info (update-failure)
  (update-id &optional text key))

(define-update malformed-channel-info (update-failure)
  (update-id &optional text))

(define-update bad-ip-format (update-failure)
  (update-id &optional text))

(define-update malformed-user-info (update-failure)
  (update-id &optional text))

(define-update no-such-user-info (update-failure)
  (update-id &optional text key))
