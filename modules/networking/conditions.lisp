#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.networking)

(define-condition client-connection-failed-error (client-condition error)
  ()
  (:report (lambda (c s) (format s "Client ~a failed to connect."
                                 (client c)))))

(define-condition client-still-connected-error (client-condition error)
  ()
  (:report (lambda (c s) (format s "The client ~a is still connected!"
                                 (client c)))))

(define-condition client-reconnection-exceeded-error (client-condition error)
  ()
  (:report (lambda (c s) (format s "Client ~a exceeded its reconnection attempts."
                                 (client c)))))

(define-condition client-connection-closed-uncleanly-warning (client-condition warning)
  ((closing-error :initarg :closing-error :reader closing-error))
  (:report (lambda (c s) (format s "Error ~s encountered while closing connection of ~a."
                                 (closing-error c) (client c)))))

(define-condition client-timeout-error (client-condition error)
  ((timeout :initarg :timeout :reader timeout))
  (:default-initargs :timeout NIL)
  (:report (lambda (c s) (format s "Client ~a timed out~@[ after ~d seconds~]."
                                 (client c) (timeout c)))))

(define-condition message-condition (condition)
  ((message :initarg :message :reader message))
  (:default-initargs :message (error "MESSAGE required.")))

(define-condition message-parse-error (message-condition client-condition error)
  ()
  (:report (lambda (c s) (format s "Failed to parse ~s from ~a."
                                 (message c) (client c)))))

(define-condition unknown-message-warning (message-condition client-condition warning)
  ()
  (:report (lambda (c s) (format s "Don't know what to do for ~s from ~a."
                                 (message c) (client c)))))
