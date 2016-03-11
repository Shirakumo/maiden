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

(defmethod initialize-instance :after ((core core) &key)
  (register-handler
   (with-handler core-instruction-event (ev)
     :name 'core-instruction-executor
     (execute-instruction ev :core core))
   core))

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
  (v:trace :colleen.core.event "~a Issuing event ~a" core event)
  (issue event (event-loop core)))

(defmethod handle ((event event) (core core))
  (handle event (event-loop core)))

(defmethod consumer (id (core core))
  (find id (consumers core) :test #'matches))

(defmethod consumer (id (cores list))
  (loop for core in cores thereis (consumer id core)))

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
  (loop for current in (consumers core)
        do (when (matches (id current) (id consumer)) (return current))
           (when (and (name consumer) (matches (name current) (name consumer)))
             (warn 'consumer-name-duplicated-warning :new-consumer consumer :existing-consumer current :core core))
        finally (push consumer (consumers core))
                (when (typep consumer 'consumer)
                  (issue (make-instance 'consumer-added :consumer consumer) core))))

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
  (setf (consumers core)
        (loop for consumer in (consumers core)
              if (matches consumer id)
              do (when (typep consumer 'consumer)
                   (issue (make-instance 'consumer-removed :consumer consumer) core))
              else collect consumer)))

(defmethod handler (id (core core))
  (handler id (event-loop core)))

(defmethod (setf handler) ((handler handler) (core core))
  (setf (handler (event-loop core)) handler))

(defmethod register-handler (handler (core core))
  (register-handler handler (event-loop core)))

(defmethod deregister-handler (handler (core core))
  (deregister-handler handler (event-loop core)))

(defmethod find-entity (id (core core))
  (or (call-next-method)
      (consumer id core)))

(defclass core-event-loop (compiled-event-loop)
  ())

(defclass core-block-loop (event-loop)
  ())

(defmacro with-awaiting (response (core &rest args &key filter timeout) setup-form &body body)
  (declare (ignore filter timeout))
  `(deeds:with-awaiting ,response (:loop ,core ,@args)
                        ,setup-form
     ,@body))

(defmacro with-response (issue response (core &rest args &key filter timeout) &body body)
  (declare (ignore filter timeout))
  (let ((core-g (gensym "CORE")))
    `(let ((,core-g ,core))
       (deeds:with-response
           ,issue
           ,response
           (:issue-loop (event-loop ,core-g) :response-loop (block-loop ,core-g) ,@args)
         ,@body))))
