#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defmethod matches (a b)
  (equal a b))

(defmethod matches ((a uuid:uuid) (b uuid:uuid))
  (uuid:uuid= a b))

(defmethod matches (a (uuid uuid:uuid))
  (matches uuid a))

(defmethod matches ((a uuid:uuid) (b vector))
  (matches (uuid:uuid-to-byte-array a) b))

(defmethod matches ((a uuid:uuid) (b string))
  (matches (princ-to-string a) b))

(defclass entity () ())

(defclass named-entity (named-entity)
  ((name :initarg :name :accessor name)
   (id :initform (uuid:make-v4-uuid) :accessor id)))

(defmethod print-object ((named-entity named-entity) stream)
  (print-unreadable-object (named-entity stream :type T)
    (format stream "~a" (id named-entity))))

(defmethod matches ((a named-entity) (b named-entity))
  (eq a b))

(defmethod matches ((entity named-entity) b)
  (or (matches (id entity) b)
      (matches (name entity) b)))

(defmethod matches (a (entity named-entity))
  (matches entity a))

(defclass core (named-entity)
  ((event-loop :initarg :event-loop :accessor event-loop)
   (block-loop :initarg :block-loop :accessor block-loop)
   (consumers :initform () :accessor consumers))
  (:default-initargs
   :event-loop (make-instance 'core-event-loop)
   :block-loop (make-instance 'core-block-loop)))

(defmethod start ((core core))
  (start (event-loop core))
  (start (block-loop core)))

(defmethod stop ((core core))
  (stop (event-loop core))
  (stop (block-loop core)))

(defmethod issue ((event event) (core core))
  (issue event (event-loop core)))

(defmethod handle ((event event) (core core))
  (handle event (event-loop core)))

(defmethod consumer (id (core core))
  (find id (consumers core) :test #'matches))

(defmethod add-consumer (consumer (core core))
  (pushnew consumer (consumers core) :test #'matches))

(defmethod remove-consumer (id (core core))
  (setf (consumers core) (remove id (consumers core) :test #'matches)))

(defmethod handler (id (core core))
  (handler id (event-loop core)))

(defmethod (setf handler) ((handler handler) (core core))
  (setf (handler (event-loop core)) handler))

(defmethod register-handler ((handler handler) (core core))
  (register-handler handler (event-loop core)))

(defmethod deregister-handler ((handler handler) (core core))
  (deregister-handler handler (event-loop core)))

(defclass core-event-loop (compiled-event-loop)
  ())

(defclass core-block-loop (event-loop)
  ())

(defmethod deeds:handle :before ((event event) (event-loop core-event-loop))
  (v:trace :colleen.event "Handling event ~a" event))

