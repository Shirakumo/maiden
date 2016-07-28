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

(define-condition data-condition (condition)
  ((data :initarg :data :reader data))
  (:default-initargs :data (error "DATA required.")))

(define-condition data-parse-error (data-condition client-condition error)
  ()
  (:report (lambda (c s) (format s "Failed to parse ~s from ~a."
                                 (data c) (client c)))))

(define-condition unknown-data-warning (data-condition client-condition warning)
  ()
  (:report (lambda (c s) (format s "Don't know what to do for ~s from ~a."
                                 (data c) (client c)))))

(define-condition data-too-long-warning (data-condition client-condition warning)
  ()
  (:report (lambda (c s) (format s "The data ~s might be truncated or dropped as it is too long for ~a."
                                 (data c) (client c)))))
