(in-package #:org.shirakumo.maiden)

(defgeneric consumer (id target))
(defgeneric add-consumer (consumer target))
(defgeneric remove-consumer (consumer target))

(defclass core (named-entity)
  ((primary-loop :initarg :primary-loop :accessor primary-loop)
   (block-loop :initarg :block-loop :accessor block-loop)
   (consumers :initform () :accessor consumers))
  (:default-initargs
   :primary-loop (make-instance 'primary-loop)
   :block-loop (make-instance 'block-loop)))

(defmethod running ((core core))
  (and (running (primary-loop core))
       (running (block-loop core))))

(defmethod start ((core core))
  (start (primary-loop core))
  (start (block-loop core))
  (mapc #'start (consumers core))
  core)

(defmethod stop ((core core))
  (mapc #'stop (consumers core))
  (stop (primary-loop core))
  (stop (block-loop core))
  core)

(defmethod issue :before ((event event) (core core))
  (deeds:with-immutable-slots-unlocked ()
    (setf (slot-value event 'event-loop) core)))

(defmethod issue ((event event) (core core))
  (v:trace :maiden.core.event "~a Issuing event ~a" core event)
  (issue event (primary-loop core))
  (issue event (block-loop core)))

(defmethod handle ((event event) (core core))
  (handle event (primary-loop core))
  (handle event (block-loop core)))

(defmethod handle :around ((event event) (delivery deeds:event-delivery))
  (with-simple-restart (abort-handling "Abort handling ~a on ~a." event delivery)
    (handler-bind ((error (lambda (err)
                            (maybe-invoke-debugger err 'abort-handling))))
      (call-next-method))))

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

(defmethod add-consumer ((consumer consumer) (core core))
  (loop for current in (consumers core)
        do (when (matches (id current) (id consumer)) (return current))
           (when (and (name consumer) (matches (name current) (name consumer)))
             (warn 'consumer-name-duplicated-warning :new-consumer consumer :existing-consumer current :core core))
        finally (push consumer (consumers core))
                (register-handler consumer (primary-loop core))
                (when (running core)
                  (do-issue core consumer-added :consumer consumer))))

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
              do  (deregister-handler consumer (primary-loop core))
                  (when (running core)
                   (do-issue core consumer-removed :consumer consumer))
              else collect consumer)))

(defmethod handler (id (core core))
  (handler id (primary-loop core)))

(defmethod (setf handler) ((handler handler) (core core))
  (setf (handler (primary-loop core)) handler))

(defmethod register-handler (handler (core core))
  (register-handler handler (primary-loop core)))

(defmethod deregister-handler (handler (core core))
  (deregister-handler handler (primary-loop core)))

(defmethod find-entity (id (core core))
  (or (call-next-method)
      (consumer id core)))

(defclass primary-loop (compiled-event-loop)
  ())

(defclass block-loop (event-loop)
  ())

(defmacro with-awaiting ((core event-type) args setup-form &body body)
  (form-fiddle:with-body-options (body options filter timeout) body
    (let ((coreg (gensym "CORE")) (loopg (gensym "LOOP")))
      `(let* ((,coreg ,core)
              (,loopg (etypecase ,coreg
                        (core (block-loop ,coreg))
                        (consumer (block-loop (first (cores ,coreg))))
                        (deeds:event-loop ,coreg))))
         (deeds:with-awaiting (,event-type ,@args)
             (:loop ,loopg :filter ,filter :timeout ,timeout)
             ,setup-form
           ,@options
           ,@body)))))

(trivial-indent:define-indentation with-awaiting (6 6 4 &body))

(defun make-core (&rest consumers)
  (apply #'add-to-core (start (make-instance 'core)) consumers))

(defun add-to-core (core &rest consumers)
  (flet ((init-consumer (class args)
           (apply #'make-instance
                  (etypecase class
                    ((or keyword string) (find-consumer-in-package class))
                    (symbol class))
                  args)))
    (let ((instances (loop for consumer in consumers
                           collect (etypecase consumer
                                     (consumer consumer)
                                     ((or symbol keyword string)
                                      (init-consumer consumer ()))
                                     (cons
                                      (init-consumer (first consumer)
                                                     (rest consumer)))))))
      (dolist (instance instances core)
        (start (add-consumer instance core))))))
