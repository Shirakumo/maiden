#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(define-condition colleen-condition (condition)
  ())

(define-condition client-condition (colleen-condition)
  ((client :initarg :client :reader client))
  (:default-initargs :client (error "CLIENT required.")))

(define-condition client-error (error client-condition)
  ())

(define-condition client-warning (warning client-condition)
  ())

(define-condition client-already-exists-error (client-error)
  ((existing-client :initarg :existing-client :reader existing-client))
  (:default-initargs :existing-client (error "EXISTING-CLIENT required."))
  (:report (lambda (c s) (format s "A client with the same name ~s (~a) already exists."
                                 (name (client c)) (existing-client c)))))

(define-condition client-connection-failed-error (client-error)
  ()
  (:report (lambda (c s) (format s "Client ~a failed to connect."
                                 (client c)))))

(define-condition client-still-connected-error (client-error)
  ()
  (:report (lambda (c s) (format s "The client ~a is still connected!"
                                 (client c)))))

(define-condition client-timeout-error (client-error)
  ((timeout :initarg :timeout :reader timeout))
  (:default-initargs :timeout NIL)
  (:report (lambda (c s) (format s "Client ~a timed out~@[ after ~d seconds~]."
                                 (client c) (timeout c)))))

(define-condition message-condition (condition)
  ((message :initarg :message :reader message))
  (:default-initargs :message (error "MESSAGE required.")))

(define-condition message-parse-error (message-condition client-error)
  ()
  (:report (lambda (c s) (format s "Failed to parse ~s from ~a."
                                 (message c) (client c)))))

(define-condition unknown-message-event-warning (message-condition client-warning)
  ()
  (:report (lambda (c s) (format s "Don't know any event to use for ~s from ~a."
                                 (message c) (client c)))))
