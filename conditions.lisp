#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden)

(define-condition maiden-condition (condition)
  ())

(define-condition core-condition (maiden-condition)
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

(define-condition agent-condition (maiden-condition)
  ((agent :initarg :agent :reader agent))
  (:default-initargs :agent (error "AGENT required.")))

(define-condition agent-already-exists-error (agent-condition)
  ((existing-agent :initarg :existing-agent :reader existing-agent))
  (:default-initargs :existing-agent (error "EXISTING-AGENT required."))
  (:report (lambda (c s) (format s "An agent of the same class ~s (~a) already exists."
                                 (class-name (class-of (agent c))) (existing-agent c)))))

(define-condition client-condition (maiden-condition)
  ((client :initarg :client :reader client))
  (:default-initargs :client (error "CLIENT required.")))
