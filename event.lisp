#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defgeneric respond (event &rest args &key class &allow-other-keys)
  (:method ((event event) &rest args &key (class (class-of event)) &allow-other-keys)
    (issue (apply #'make-instance class args)
           (event-loop event))))

(define-event client-event ()
  ((client :initarg :client :reader client))
  (:default-initargs
   :client (error "CLIENT required.")))

(defmethod respond ((event client-event) &rest args &key (class (class-of event)) &allow-other-keys)
  (issue (apply #'make-instance class :client (client event) args)
         (event-loop event)))

(define-event sender-event (client-event)
  ((sender :initarg :sender :reader sender))
  (:default-initargs
   :sender (error "SENDER required.")))

;; (define-event message-event (sender-event)
;;   ((message :initarg :message :reader message :mutable T))
;;   (:default-initargs
;;    :message (error "MESSAGE required.")))

(define-event core-event ()
  ())

(define-event consumer-added (core-event)
  ((consumer :initarg :consumer))
  (:default-initargs
   :consumer (error "CONSUMER required.")))

(defmethod print-object ((event consumer-added) stream)
  (print-unreadable-object (event stream :type T :identity T)
    (format stream "~a" (slot-value event 'consumer))))

(define-event consumer-removed (core-event)
  ((consumer :initarg :consumer))
  (:default-initargs
   :consumer (error "CONSUMER required.")))

(defmethod print-object ((event consumer-removed) stream)
  (print-unreadable-object (event stream :type T :identity T)
    (format stream "~a" (slot-value event 'consumer))))

(define-event instruction-event ()
  ())

(defgeneric execute-instruction (instruction &key))

(define-event core-instruction-event (core-event instruction-event)
  ())

(define-event add-consumer (core-instruction-event)
  ((consumer-type :initarg :consumer-type :reader consumer-type)
   (name :initarg :name :reader name)
   (initargs :initarg :initargs :reader initargs))
  (:default-initargs
   :consumer-type (error "CONSUMER-TYPE required.")
   :name (error "NAME required.")
   :initargs ()))

(defmethod execute-instruction ((instruction add-consumer) &key core)
  (add-consumer (start (apply #'make-instance
                              (consumer-type instruction)
                              :name (name instruction)
                              (initargs instruction)))
                core))

(define-event remove-consumer (core-instruction-event)
  ((name :initarg :name :reader name)
   (stop :initarg :stop :reader stop))
  (:default-initargs
   :name (error "NAME required.")
   :stop T))

(defmethod execute-instruction ((instruction remove-consumer) &key core)
  (let ((consumer (consumer (name instruction) core)))
    (when (stop instruction) (stop consumer))
    (remove-consumer consumer core)))
