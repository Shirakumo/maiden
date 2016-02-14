#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defclass component-class (standard-class)
  ((handlers :initform () :accessor handlers)))

;; handlers, initializing them

(defclass component ()
  ()
  (:metaclass component-class))

(defmacro define-component (name direct-superclasses direct-slots &rest options)
  (unless (find 'component direct-superclasses :test #'superclass-contains)
    (push 'component direct-superclasses))
  (unless (find :metaclass options :key #'first)
    (push `(:metaclass component-class) options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))
