#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defgeneric matches (a b))

(defmethod matches (a b)
  (equal a b))

(defmethod matches ((a uuid:uuid) (b uuid:uuid))
  (uuid:uuid= a b))

(defmethod matches (a (uuid uuid:uuid))
  (matches uuid a))

(defmethod matches ((a uuid:uuid) (b vector))
  (matches (uuid:uuid-to-byte-array a) b))

(defmethod matches ((a uuid:uuid) (b string))
  (matches (princ-to-string a) b))

(defclass entity () ())

(defclass named-entity (entity)
  ((name :initarg :name :accessor name)
   (id :initform (uuid:make-v4-uuid) :accessor id))
  (:default-initargs
   :name NIL))

(defmethod print-object ((named-entity named-entity) stream)
  (print-unreadable-object (named-entity stream :type T)
    (format stream "~:[~a~;~:*~a~]" (name named-entity) (id named-entity))))

(defmethod matches ((a named-entity) (b named-entity))
  (eq a b))

(defmethod matches ((entity named-entity) b)
  (or (matches (id entity) b)
      (matches (name entity) b)))

(defmethod matches (a (entity named-entity))
  (matches entity a))
