#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.accounts)

(defclass user ()
  ((name :initarg :name :accessor name)
   (identities :initform () :accessor identities)
   (data :initarg :data :accessor data))
  (:default-initargs
   :name (error "NAME required.")
   :data (make-hash-table)))

