#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defclass colleen-event-loop (event-loop)
  ())

(defmethod handle :before ((event event) (event-loop colleen-event-loop))
  (v:trace :colleen.event "Handling event ~a" event))

(defvar *event-loop* (start (make-instance 'colleen-event-loop)))
