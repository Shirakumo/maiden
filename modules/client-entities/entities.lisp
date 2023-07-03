(in-package #:org.shirakumo.maiden.modules.client-entities)

(defclass client-entity (named-entity)
  ((client :initarg :client :accessor client))
  (:default-initargs
   :client (error "CLIENT required.")))

(defmethod matches ((a client-entity) (b client-entity))
  (and (call-next-method)
       (eql (client a) (client b))))

(defclass user (client-entity data-entity)
  ((authenticated :initarg :authenticated)))

(defmethod username ((user user))
  (name user))

(defmethod (setf username) (name (user user))
  (setf (name user) name))

(defmethod ensure-user ((user user) client)
  user)

(defmethod authenticated-p ((user user))
  (if (slot-boundp user 'authenticated)
      (slot-value user 'authenticated)
      (setf (slot-value user 'authenticated)
            (authenticate user (client user)))))

(defmethod authenticate :around ((user user) client)
  (let ((value (call-next-method)))
    (setf (slot-value user 'authenticated) value)))

;; Shitty default implementation
(defmethod channels ((user user))
  (when (typep (client user) 'channel-client)
    (loop for channel in (channels (client user))
          when (find user (users channel)) collect channel)))

(defclass channel (client-entity)
  ())

(defmethod ensure-channel ((channel channel) client)
  channel)

(defmethod users ((channel channel))
  ())
