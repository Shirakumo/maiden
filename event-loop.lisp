#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defclass event-loop (deeds:event-loop)
  ())

(defmethod deeds:handle :before ((event deeds:event) (event-loop event-loop))
  (v:trace :colleen.event "Handling event ~a" event))

(defvar *event-loop* (start (make-instance 'event-loop)))
