(in-package #:org.shirakumo.maiden)

;; Forwarding class definition
(defclass core () ())

(defclass consumer-class (standard-class)
  ((direct-handlers :initform () :accessor direct-handlers)
   (effective-handlers :initform () :accessor effective-handlers)
   (instances :initform () :accessor instances)))

(defmethod c2mop:validate-superclass ((class consumer-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass consumer-class))
  T)

(defmethod c2mop:validate-superclass ((class consumer-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class consumer-class) (superclass consumer-class))
  T)

(defun cascade-handler-changes (class)
  (let ((handlers (copy-list (direct-handlers class))))
    (loop for superclass in (c2mop:class-direct-superclasses class)
          when (c2mop:subclassp superclass 'consumer)
          do (loop for handler in (effective-handlers superclass)
                   unless (find (name handler) handlers :key #'name)
                   do (push handler handlers)))
    (setf (effective-handlers class) handlers))
  (loop for sub-class in (c2mop:class-direct-subclasses class)
        when (and (c2mop:subclassp sub-class 'consumer-class)
                  (c2mop:class-finalized-p sub-class))
        do (cascade-handler-changes sub-class)))

(defmethod c2mop:finalize-inheritance :after ((class consumer-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (cascade-handler-changes class))

(defmethod (setf effective-handlers) :after (handlers (class consumer-class))
  (setf (instances class)
        (loop for pointer in (instances class)
              for consumer = (tg:weak-pointer-value pointer)
              when consumer
              collect (prog1 pointer 
                        (reinitialize-handlers consumer handlers)))))

(defmethod (setf direct-handlers) :after (handlers (class consumer-class))
  (c2mop:finalize-inheritance class))

(defun update-handler (abstract-handler class-ish)
  (check-type abstract-handler abstract-handler)
  (etypecase class-ish
    (consumer-class (update-list abstract-handler (direct-handlers class-ish) :key #'name))
    (consumer (update-handler abstract-handler (class-of class-ish)))
    (symbol (update-handler abstract-handler (find-class class-ish)))))

(defun remove-handler (abstract-handler class-ish)
  (etypecase class-ish
    (consumer-class (setf (direct-handlers class-ish)
                          (etypecase abstract-handler
                            (abstract-handler (delete abstract-handler (direct-handlers class-ish)))
                            (symbol (delete abstract-handler (direct-handlers class-ish) :key #'name)))))
    (consumer (remove-handler abstract-handler (class-of class-ish)))
    (symbol (remove-handler abstract-handler (find-class class-ish)))))

(defclass consumer (named-entity compiled-event-loop handler)
  ((cores :initform () :accessor cores)
   (core-handlers :initform () :accessor core-handlers)
   (deeds:event-loop-lock :accessor lock))
  (:metaclass consumer-class))

(defmethod initialize-instance :after ((consumer consumer) &key)
  (push (tg:make-weak-pointer consumer) (instances (class-of consumer)))
  (reinitialize-handlers consumer (effective-handlers (class-of consumer))))

(defmethod reinitialize-handlers :around ((consumer consumer) handlers)
  (bt:with-recursive-lock-held ((lock consumer))
    (call-next-method)))

;; FIXME: Keeping book on what's started or not and retaining that.
(defmethod reinitialize-handlers ((consumer consumer) abstract-handlers)
  (v:debug :maiden.core.consumer "~a updating handlers." consumer)
  ;; Update direct handlers
  (let ((handlers (loop for v being the hash-values of (handlers consumer))))
    (deregister-handler handlers consumer)
    (dolist (abstract-handler abstract-handlers)
      (when (add-to-consumer abstract-handler)
        (register-handler (start (instantiate-handler abstract-handler consumer))
                          consumer)))
    (mapc #'stop handlers))
  ;; Update core handlers
  (let ((handlers (core-handlers consumer)))
    (dolist (core (cores consumer))
      (deregister-handler handlers core))
    (setf (core-handlers consumer) NIL)
    (dolist (abstract-handler abstract-handlers)
      (unless (add-to-consumer abstract-handler)
        (let ((handler (start (instantiate-handler abstract-handler consumer))))
          (push handler (core-handlers consumer))
          (dolist (core (cores consumer))
            (register-handler handler core)))))
    (mapc #'stop handlers)))

(defmethod handler ((consumer consumer) (event-loop event-loop))
  (gethash consumer (handlers event-loop)))

(defmethod (setf handler) ((consumer consumer) (event-loop event-loop))
  (setf (gethash consumer (handlers event-loop)) consumer))

(defmethod start :after ((consumer consumer))
  (mapc #'start (core-handlers consumer)))

(defmethod stop :after ((consumer consumer))
  (mapc #'stop (core-handlers consumer)))

(defmethod add-consumer :after ((consumer consumer) (core core))
  (push core (cores consumer))
  (register-handler (core-handlers consumer) core))

(defmethod remove-consumer ((consumer consumer) (everywhere (eql T)))
  (dolist (core (cores consumer))
    (remove-consumer consumer core)))

(defmethod remove-consumer :after ((consumer consumer) (core core))
  (setf (cores consumer) (remove core (cores consumer)))
  (deregister-handler (core-handlers consumer) core))

(defmethod find-entity (id (consumer consumer))
  (or (call-next-method)
      (find-entity id (cores consumer))))

(defclass abstract-handler ()
  ((target-class :initform 'locally-blocking-handler :accessor target-class)
   (add-to-consumer :initarg :add-to-consumer :accessor add-to-consumer)
   (options :initarg :options :accessor options)
   (name :initarg :name :accessor name))
  (:default-initargs
   :options ()
   :add-to-consumer T))

(defmethod initialize-instance :after ((handler abstract-handler) &rest args &key target-class &allow-other-keys)
  (setf (options handler) (deeds::removef args :target-class :options :add-to-consumer))
  (when target-class (setf (target-class handler) target-class)))

(defmethod instantiate-handler ((handler abstract-handler) (consumer consumer))
  (let* ((options (options handler))
         (filter (getf options :filter))
         (delivery (getf options :delivery-function))
         (match (getf options :match-consumer)))
    ;; Extend filter to match consumer.
    (when match
      (if (eql match T)
          (setf filter `(and (eq ,consumer consumer) ,(or filter T)))
          (setf filter `(and (eq ,consumer ,match) ,(or filter T)))))
    (apply #'make-instance
           (target-class handler)
           :delivery-function (lambda (event) (funcall delivery consumer event))
           :filter filter
           (deeds::removef options :delivery-function :filter :match-consumer))))

(defmacro define-handler ((consumer name event-type) args &body body)
  (destructuring-bind (compvar event &rest args) args
    (form-fiddle:with-body-options (body options class) body
      `(progn
         (update-handler
          (make-instance
           'abstract-handler
           :target-class ',(or class 'deeds:locally-blocking-handler)
           :name ',name
           :event-type ',event-type
           :delivery-function (named-lambda ,name (,compvar ,event)
                                (declare (ignorable ,compvar ,event))
                                (with-origin (',name)
                                  (with-fuzzy-slot-bindings ,args (,event ,event-type)
                                    ,@body)))
           ,@options)
          ',consumer)
         (list ',consumer ',name)))))

(defun slot-args->slots (args)
  (flet ((make-req-field (a)
           (destructuring-bind (name &rest kargs) (enlist a)
             `(,name :initarg ,(kw name) :initform (error ,(format NIL "~a required." name)) ,@kargs)))
         (make-opt-field (a)
           (destructuring-bind (name &optional value &rest kargs) (enlist a)
             `(,name :initarg ,(kw name) :initform ,value ,@kargs))))
    (lambda-fiddle:with-destructured-lambda-list (:required required :optional optional :rest rest :key key) args
      (append (mapcar #'make-req-field required)
              (mapcar #'make-opt-field optional)
              (when rest (list (make-req-field rest)))
              (mapcar #'make-opt-field key)))))

(defun slot-args->args (args)
  (loop with stage = '&req
        for arg in args
        collect (cond ((find arg lambda-list-keywords)
                       (setf stage arg) arg)
                      ((find stage '(&key &optional))
                       (destructuring-bind (name &optional value &rest kargs) (enlist arg)
                         (declare (ignore kargs))
                         `(,name ,value)))
                      (T
                       (unlist arg)))))

(defun args->initargs (args)
  (loop for arg in (lambda-fiddle:extract-lambda-vars args)
        collect (kw arg) collect arg))

(defmacro define-function-handler ((consumer name &optional (event-type name)) args &body body)
  (form-fiddle:with-body-options (body options superclasses extra-slots class-options documentation advice) body
    (destructuring-bind (consumer-var event-var &rest args) args
      (when documentation (push `(:documentation ,documentation) class-options))
      `(progn
         (define-event ,event-type ,superclasses
           (,@(slot-args->slots args)
            ,@extra-slots)
           (:advice ,advice)
           ,@class-options)
         (define-handler (,consumer ,name ,event-type) (,consumer-var ,event-var ,@(lambda-fiddle:extract-lambda-vars args))
           ,@options
           ,@body)))))

(defun remove-function-handler (consumer name &optional (event-type name))
  (setf (find-class event-type) NIL)
  (remove-handler name consumer))

(defmacro define-instruction ((consumer instruction &optional (event-type instruction)) args &body body)
  (form-fiddle:with-body-options (body options superclasses documentation) body
    (let ((core (gensym "CORE")))
      `(progn
         (define-function-handler (,consumer ,instruction ,event-type) ,args
           :documentation ,documentation
           :superclasses (,@superclasses instruction-event)
           ,@options
           ,@body)
         (defun ,instruction (,core ,@(slot-args->args (cddr args)))
           ,documentation
           (broadcast ,core ',event-type ,@(args->initargs (cddr args))))))))

(defun remove-instruction (consumer instruction &optional (event-type instruction))
  (remove-function-handler consumer instruction event-type)
  (fmakunbound instruction))

(defmacro define-query ((consumer instruction &optional (event-type instruction) event-response-type) args &body body)
  (form-fiddle:with-body-options (body options superclasses documentation) body
    (let ((thunk (gensym "THUNK"))
          (event (gensym "EVENT"))
          (core (gensym "CORE")))
      `(progn
         (define-function-handler (,consumer ,instruction ,event-type) ,args
           :documentation ,documentation
           :superclasses (,@superclasses instruction-event)
           ,@options
           (flet ((,thunk () ,@body))
             (respond ,(second args) :payload (multiple-value-list (,thunk)))))             
         ,@(when event-response-type
             `((define-event ,event-response-type (response-event) ())
               (defmethod respond ((event ,event-type) &key payload)
                 (issue (make-instance ',event-response-type :payload payload :identifier (identifier event))
                        (event-loop event)))))
         (defun ,instruction (core ,@(slot-args->args (cddr args)))
           ,documentation
           (let ((,event (make-instance ',event-type :identifier (uuid:make-v4-uuid) ,@(args->initargs (cddr args)))))
             (with-awaiting (,core ,(or event-response-type 'response-event)) (,(gensym "EVENT") payload)
                 (issue ,event core)
               :filter `(matches identifier ,(identifier ,event))
               (values-list payload))))))))

(defun remove-query (consumer instruction &optional (event-type instruction) event-response-type)
  (remove-function-handler consumer instruction event-type)
  (when event-response-type
    (setf (find-class event-response-type) NIL))
  (fmakunbound instruction))

(defmacro define-consumer (name direct-superclasses direct-slots &rest options)
  (when (loop for super in direct-superclasses
              never (c2mop:subclassp (find-class super) (find-class 'consumer)))
    (push 'consumer direct-superclasses))
  (unless (find :metaclass options :key #'first)
    (push `(:metaclass consumer-class) options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))
