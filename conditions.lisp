#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(define-condition colleen-condition (condition)
  ())

(define-condition core-condition (colleen-condition)
  ((core :initarg :core :reader core))
  (:default-initargs :core (error "CORE required.")))

(define-condition consumer-name-duplicated-warning (core-condition warning)
  ((existing-consumer :initarg :existing-consumer :reader existing-consumer)
   (new-consumer :initarg :new-consumer :reader new-consumer))
  (:default-initargs
   :existing-consumer (error "EXISTING-CONSUMER required.")
   :new-consumer (error "NEW-CONSUMER required."))
  (:report (lambda (c s) (format s "A consumer with the name ~s (~a) ~
                                    already exists when adding ~a to ~a."
                                 (name (existing-consumer c)) (existing-consumer c)
                                 (new-consumer c) (core c)))))

(define-condition agent-condition (colleen-condition)
  ((agent :initarg :agent :reader agent))
  (:default-initargs :agent (error "AGENT required.")))

(define-condition agent-already-exists-error (agent-condition)
  ((existing-agent :initarg :existing-agent :reader existing-agent))
  (:default-initargs :existing-agent (error "EXISTING-AGENT required."))
  (:report (lambda (c s) (format s "An agent of the same class ~s (~a) already exists."
                                 (class-name (class-of (agent c))) (existing-agent c)))))

(define-condition client-condition (colleen-condition)
  ((client :initarg :client :reader client))
  (:default-initargs :client (error "CLIENT required.")))

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
