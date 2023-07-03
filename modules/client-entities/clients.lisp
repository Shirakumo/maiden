(in-package #:org.shirakumo.maiden.modules.client-entities)

(define-consumer user-client (client)
  ())

(defmethod username ((client user-client))
  (name client))

(defmethod (setf username) (name (client user-client))
  (setf (name client) name))

(defmethod find-user (name (client user-client))
  NIL)

(defmethod users ((client user-client))
  ())

(defmethod ensure-user ((name string) (client user-client))
  (or (find-user name client)
      (make-instance 'user :name name :client client)))

(defmethod authenticate (user (client user-client))
  (authenticate (ensure-user user client) client))

(defmethod authenticate ((user user) (client user-client))
  NIL)

(define-consumer channel-client (client)
  ())

(defmethod find-channel (name (client channel-client))
  NIL)

(defmethod channels ((client channel-client))
  ())

(defmethod ensure-channel ((name string) (client channel-client))
  (or (find-channel name client)
      (make-instance 'channel :name name :client client)))

;; FIXME: lock for concurrent access
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass user-container ()
    ((users :initform (make-hash-table :test 'equalp) :accessor user-map))))

(defmethod users ((container user-container))
  (loop for v being the hash-values of (user-map container) collect v))

(defmethod find-user ((user user) (container user-container))
  (find-user (name user) container))

(defmethod find-user ((name string) (container user-container))
  (gethash name (user-map container)))

(defmethod (setf find-user) ((user user) (name string) (container user-container))
  (setf (gethash name (user-map container)) user))

(defmethod remove-user ((name string) (container user-container))
  (remhash name (user-map container)))

(defmethod remove-user ((user user) (container user-container))
  (remove-user (name user) container))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass channel-container ()
    ((channels :initform (make-hash-table :test 'equalp) :accessor channel-map))))

(defmethod channels ((container channel-container))
  (loop for v being the hash-values of (channel-map container) collect v))

(defmethod find-channel ((channel channel) (container channel-container))
  (find-channel (name channel) container))

(defmethod find-channel ((name string) (container channel-container))
  (gethash name (channel-map container)))

(defmethod (setf find-channel) ((channel channel) (name string) (container channel-container))
  (setf (gethash name (channel-map container)) channel))

(defmethod remove-channel ((name string) (container channel-container))
  (remhash name (channel-map container)))

(defmethod remove-channel ((channel channel) (container channel-container))
  (remove-channel (name channel) container))

(define-consumer simple-user-channel-client (user-container channel-container user-client channel-client)
  ())

(defclass simple-user (channel-container user)
  ())

(defclass simple-channel (user-container channel)
  ())

(defmethod remove-channel :after ((channel simple-channel) (client simple-user-channel-client))
  (loop for user being the hash-values of (user-map channel)
        do (remove-channel channel user)))

(defmethod remove-user :after ((user simple-user) (client simple-user-channel-client))
  (loop for channel being the hash-values of (channel-map user)
        do (remove-user user channel)))

(defmethod ensure-user ((name string) (client simple-user-channel-client))
  (or (find-user name client)
      (setf (find-user name client)
            (make-instance 'simple-user :name name :client client))))

(defmethod ensure-channel ((name string) (client simple-user-channel-client))
  (or (find-channel name client)
      (setf (find-channel name client)
            (make-instance 'simple-channel :name name :client client))))
