#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defgeneric consumer (id target))
(defgeneric add-consumer (consumer target))
(defgeneric remove-consumer (consumer target))

(defclass core (named-entity)
  ((event-loop :initarg :event-loop :accessor event-loop)
   (block-loop :initarg :block-loop :accessor block-loop)
   (consumers :initform () :accessor consumers))
  (:default-initargs
   :event-loop (make-instance 'core-event-loop)
   :block-loop (make-instance 'core-block-loop)))

(defmethod start ((core core))
  (start (event-loop core))
  (start (block-loop core))
  core)

(defmethod stop ((core core))
  (stop (event-loop core))
  (stop (block-loop core))
  core)

(defmethod issue :before ((event event) (core core))
  (deeds:with-immutable-slots-unlocked ()
    (setf (slot-value event 'event-loop) core)))

(defmethod issue ((event event) (core core))
  (issue event (event-loop core)))

(defmethod handle ((event event) (core core))
  (handle event (event-loop core)))

(defmethod consumer (id (core core))
  (find id (consumers core) :test #'matches))

(defmethod add-consumer :around (consumer target)
  (call-next-method)
  consumer)

(defmethod add-consumer ((consumers list) target)
  (dolist (consumer consumers)
    (add-consumer consumer target)))

(defmethod add-consumer (consumer (targets list))
  (dolist (target targets)
    (add-consumer consumer target)))

(defmethod add-consumer (consumer (core core))
  (pushnew consumer (consumers core) :test #'matches))

(defmethod add-consumer :after (consumer (core core))
  (issue (make-instance 'consumer-added :consumer consumer) core))

(defmethod remove-consumer :around (consumer target)
  (call-next-method)
  consumer)

(defmethod remove-consumer ((consumers list) target)
  (dolist (consumer consumers)
    (remove-consumer consumer target)))

(defmethod remove-consumer (consumer (targets list))
  (dolist (target targets)
    (remove-consumer consumer target)))

(defmethod remove-consumer (id (core core))
  (setf (consumers core) (remove id (consumers core) :test #'matches)))

(defmethod remove-consumer :after (consumer (core core))
  (issue (make-instance 'consumer-removed :consumer consumer) core))

(defmethod handler (id (core core))
  (handler id (event-loop core)))

(defmethod (setf handler) ((handler handler) (core core))
  (setf (handler (event-loop core)) handler))

(defmethod register-handler (handler (core core))
  (register-handler handler (event-loop core)))

(defmethod deregister-handler (handler (core core))
  (deregister-handler handler (event-loop core)))

(defclass core-event-loop (compiled-event-loop)
  ())

(defclass core-block-loop (event-loop)
  ())

(defmethod handle :before ((event event) (event-loop core-event-loop))
  (v:trace :colleen.event "Handling event ~a" event))

