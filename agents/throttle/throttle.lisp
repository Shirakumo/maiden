#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.throttle)

(define-consumer throttle (agent)
  ())

(defmethod initialize-instance :after ((throttle throttle) &key (rules NIL rules-p))
  (unless rules-p
    (with-storage (throttle)
      (setf (rules throttle) (or (value :rules) (make-hash-table :test 'equal))))))

(defun record (ev c)
  )

(defun throttled (ev c)
  )

(define-handler (throttle block-commands command-event) (c ev dispatch-event)
  :before :main
  :class deeds:locally-blocking-handler
  (record ev c)
  (when (throttled ev c)
    (cancel ev)))

