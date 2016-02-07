#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defvar *clients* ())
(defvar *clients-lock* (bt:make-lock "Colleen clients registry lock"))

(deeds:define-cached-slots-class client ()
  ((name :initarg :name :reader name))
  (:default-initargs
   :name (error "NAME required.")))

(defmethod shared-initialize :around ((client client) slot-names &rest args &key name)
  (etypecase name
    (string (apply #'call-next-method client slot-names :name (kw name) args))
    (symbol (call-next-method))
    (null (call-next-method))))

(defmethod initialize-instance :after ((client client) &key)
  (add-client client))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~a" (name client))))

(defgeneric client (client-ish))
(defgeneric add-client (client))
(defgeneric remove-client (client))

(defmethod client ((name string))
  (client (kw name)))

(defmethod client ((name symbol))
  (find name *clients* :test #'string-equal :key #'name))

(defmethod client ((client client))
  client)

(defun list-clients ()
  (bt:with-lock-held (*clients-lock*)
    (copy-list *clients*)))

(defmethod add-client ((client client))
  (bt:with-lock-held (*clients-lock*)
    (let ((existing (loop for cons on *clients*
                          when (string-equal (name (car cons)) (name client))
                          return cons)))
      (cond (existing
             (cerror "Replace the client." 'client-already-exists-error
                     :client client :existing (car existing))
             (setf (car existing) client))
            (T
             (push client *clients*))))))

(defmethod remove-client ((client client))
  (bt:with-lock-held (*clients-lock*)
    (setf *clients* (remove client *clients*))))

(defmacro define-client (name direct-superclasses direct-slots &rest options)
  (when (loop for super in direct-superclasses
              never (c2mop:subclassp (find-class super) (find-class 'client)))
    (push 'client direct-superclasses))
  `(deeds:define-cached-slots-class ,name ,direct-superclasses
     ,direct-slots
     ,@options))

(define-client user-client ()
  ())

(defgeneric authenticate (sender client)
  (:method (sender (client client))
    NIL))

(define-client remote-client ()
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

(defmethod remove-client :before ((client remote-client))
  (when (client-connected-p client)
    (cerror "Remove the client anyway." 'client-still-connected-error :client client)))

(define-client server-client (remote-client)
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
