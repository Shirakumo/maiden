#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(define-consumer client () ())

(define-consumer user-client (client)
  ())

(defgeneric authenticate (sender client)
  (:method (sender (client client))
    NIL))

(define-consumer remote-client (client)
  ())

(defmethod print-object ((client remote-client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~a~@[ ~s~]" (name client) (when (client-connected-p client) :connected))))

(defgeneric client-connected-p (client)
  (:method ((name string))
    (client-connected-p (client name)))
  (:method ((name symbol))
    (client-connected-p (client name))))

(defgeneric close-connection (client)
  (:method ((name string))
    (close-connection (client name)))
  (:method ((name symbol))
    (close-connection (client name))))

(defmethod remove-consumer :before ((client remote-client) target)
  (when (client-connected-p client)
    (cerror "Remove the client anyway." 'client-still-connected-error :client client)))

(define-consumer server-client (remote-client)
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port)
   (encoding :initarg :encoding :accessor encoding))
  (:default-initargs
   :host (error "HOST required.")
   :encoding :utf-8))

(defmethod print-object ((client server-client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~a~@[ ~s~] ~s ~a:~a"
            (name client) (when (client-connected-p client) :connected) :host (host client) (port client))))

(defgeneric initiate-connection (client)
  (:method ((name string))
    (initiate-connection (client name)))
  (:method ((name symbol))
    (initiate-connection (client name))))

(defmethod initiate-connection :around ((client server-client))
  (with-default-encoding ((encoding client))
    (call-next-method)))
