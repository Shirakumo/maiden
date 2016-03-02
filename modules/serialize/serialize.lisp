#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.modules.serialize)

(defvar *finders* ())
(defvar *event-code* (cl-store:register-code 100 'event))
(defvar *core-code* (cl-store:register-code 101 'core))
(defvar *consumer-code* (cl-store:register-code 102 'consumer))
(defvar *footer-buffer* (make-array 8 :initial-element 0 :element-type '(unsigned-byte 8)))

(defun find-instance (id type)
  (let ((finder (cdr (assoc type *finders*))))
    (when finder (funcall finder id))))

(cl-store:defstore-cl-store (object event stream)
  (cl-store:output-type-code *event-code* stream)
  (cl-store::store-type-object object stream))

(cl-store:defrestore-cl-store (event stream)
  (deeds:with-immutable-slots-unlocked ()
    (cl-store::restore-type-object stream)))

(cl-store:defstore-cl-store (object core stream)
  (cl-store:output-type-code *core-code* stream)
  (cl-store::store-type-object (id object) stream))

(cl-store:defrestore-cl-store (core stream)
  (let ((id (cl-store::restore-type-object stream)))
    (or (find-instance id 'core) id)))

(cl-store:defstore-cl-store (object consumer stream)
  (cl-store:output-type-code *consumer-code* stream)
  (cl-store::store-type-object (id object) stream))

(cl-store:defrestore-cl-store (consumer stream)
  (let ((id (cl-store::restore-type-object stream)))
    (or (find-instance id 'consumer) id)))

(defgeneric serialize (object target)
  (:method (object (target stream))
    (let ((target (gzip-stream:make-gzip-output-stream target)))
      (cl-store:store object target)
      (finish-output target))))

(defgeneric deserialize (source finders)
  (:method ((source stream) finders)
    (let ((*finders* finders))
      (prog1 (cl-store:restore (gzip-stream:make-gzip-input-stream source))
        (read-sequence *footer-buffer* source)))))
