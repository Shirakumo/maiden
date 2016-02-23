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

(define-event message-event (sender-event)
  ((message :initarg :message :reader message :mutable T))
  (:default-initargs
   :message (error "MESSAGE required.")))
