#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.relay)

(define-event connection-initiated (client-event)
  ())

(define-event connection-closed (client-event)
  ())

(define-event relay-instruction-event (instruction-event)
  ())

(define-event query-event (deeds:identified-event relay-instruction-event)
  ((source :initarg :source :reader source))
  (:default-initargs
   :source (error "SOURCE required.")
   :identifier (uuid:make-v4-uuid)))

(defmethod execute-instruction :around ((event query-event) &key relay)
  (relay (make-transport
          (make-instance 'response-event
                         :response (call-next-method)
                         :identifier (deeds:identifier event))
          (source event))
         (source event)
         relay))

(define-event response-event (deeds:identified-event)
  ((response :initarg :response :reader response))
  (:default-initargs
   :response (error "RESPONSE required.")
   :identifier (error "IDENTIFIER required.")))

(define-event slot-event (query-event relay-instruction-event)
  ((slot :initarg :slot :reader slot)
   (object :initarg :object :reader object))
  (:default-initargs
   :slot (error "SLOT required.")
   :object (error "OBJECT required.")))

(define-event slot-value-event (slot-event)
  ())

(defmethod execute-instruction ((event slot-value-event) &key)
  (slot-value (object event) (slot event)))

(define-event slot-setf-event (slot-event)
  ((value :initarg :value :reader value))
  (:default-initargs
   :value NIL))

(defmethod execute-instruction ((event slot-setf-event) &key)
  (setf (slot-value (object event) (slot event)) (value event)))

(define-event slot-makunbound-event (slot-event)
  ())

(defmethod execute-instruction ((event slot-makunbound-event) &key)
  (slot-makunbound (object event) (slot event)))

(define-event slot-boundp-event (slot-event)
  ())

(defmethod execute-instruction ((event slot-boundp-event) &key)
  (slot-boundp (object event) (slot event)))

(define-event generic-call-event (query-event relay-instruction-event)
  ((form :initarg :form :reader form))
  (:default-initargs
   :form (error "FORM required.")))

(defmethod execute-instruction ((event generic-call-event) &key)
  (apply #'funcall (form event)))
