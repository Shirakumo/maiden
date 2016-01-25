#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(define-event client-event ()
  ((client :initarg :client :reader client))
  (:default-initargs
   :client (error "CLIENT required.")))

(define-event sender-event (client-event)
  ((sender :initarg :sender :reader sender))
  (:default-initargs
   :sender (error "SENDER required.")))

(define-event message-event (sender-event)
  ((message :initarg :message :reader message))
  (:default-initargs
   :message (error "MESSAGE required.")))

(defgeneric reply (message-event message &key &allow-other-keys))
