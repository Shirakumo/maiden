#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.modules.serialize)

(defvar *event-code* (cl-store:register-code 100 'deeds:event))
(defvar *footer-buffer* (make-array 8 :initial-element 0 :element-type '(unsigned-byte 8)))

(cl-store:defstore-cl-store (object deeds:event stream)
  (cl-store:output-type-code *event-code* stream)
  (cl-store::store-type-object object stream))

(cl-store:defrestore-cl-store (deeds:event stream)
  (handler-bind ((deeds:immutable-event-slot-modified #'continue))
    (cl-store::restore-type-object stream)))

(defgeneric serialize (object target)
  (:method (object (target stream))
    (let ((target (gzip-stream:make-gzip-output-stream target)))
      (cl-store:store object target)
      (finish-output target))))

(defgeneric deserialize (source)
  (:method ((source stream))
    (prog1 (cl-store:restore (gzip-stream:make-gzip-input-stream source))
      (read-sequence *footer-buffer* source))))
