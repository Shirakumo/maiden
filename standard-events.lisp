(in-package #:org.shirakumo.maiden)

(defgeneric respond (event &key &allow-other-keys))

(defmethod respond ((event event) &rest args &key (class (class-of event)) &allow-other-keys)
  (issue (apply #'make-instance class args)
         (event-loop event)))

(define-event passive-event ()
  ())

(define-event active-event ()
  ())

(define-event client-event ()
  ((client :initarg :client :reader client))
  (:default-initargs
   :client (error "CLIENT required.")))

(defmethod respond ((event client-event) &rest args &key (class (class-of event)) &allow-other-keys)
  (remf args :class)
  (issue (apply #'make-instance class :client (client event) args)
         (event-loop event)))

(define-event instruction-event (active-event)
  ())

(define-event query-event (identified-event instruction-event)
  ())

(defmethod respond ((event query-event) &key payload)
  (issue (make-instance 'response-event :identifier (identifier event)
                                        :payload payload)
         (event-loop event)))

(define-event response-event (identified-event payload-event passive-event)
  ()
  (:default-initargs :identifier (error "IDENTIFIER required.")))

(define-event core-event ()
  ())

(define-event consumer-added (core-event passive-event)
  ((consumer :initarg :consumer))
  (:default-initargs
   :consumer (error "CONSUMER required.")))

(defmethod print-object ((event consumer-added) stream)
  (print-unreadable-object (event stream :type T :identity T)
    (format stream "~a" (slot-value event 'consumer))))

(define-event consumer-removed (core-event passive-event)
  ((consumer :initarg :consumer))
  (:default-initargs
   :consumer (error "CONSUMER required.")))

(defmethod print-object ((event consumer-removed) stream)
  (print-unreadable-object (event stream :type T :identity T)
    (format stream "~a" (slot-value event 'consumer))))
