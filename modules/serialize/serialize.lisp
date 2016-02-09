#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.modules.serialize)

(defvar *event-code* (cl-store:register-code 100 'deeds:event))

(cl-store:defstore-cl-store (object deeds:event stream)
  (cl-store:output-type-code *event-code* stream)
  (cl-store::store-type-object object stream))

(cl-store:defrestore-cl-store (deeds:event stream)
  (handler-bind ((deeds:immutable-event-slot-modified #'continue))
    (cl-store::restore-type-object stream)))

(defclass compressing-stream (trivial-gray-streams:fundamental-binary-output-stream)
  ((compressor :initarg :compressor :accessor compressor)
   (target-stream :initarg :target-stream :reader target-stream))
  (:default-initargs
   :compressor 'salza2:gzip-compressor
   :target-stream (error "TARGET-STREAM required.")))

(defmethod initialize-instance :after ((compressing-stream compressing-stream) &key)
  (when (symbolp (compressor compressing-stream))
    (setf (compressor compressing-stream)
          (make-instance (compressor compressing-stream)
                         :callback (salza2:make-stream-output-callback (target-stream compressing-stream))))))

(defmethod trivial-gray-streams:stream-write-byte ((compressing-stream compressing-stream) octet)
  (salza2:compress-octet octet (compressor compressing-stream)))

(defmethod trivial-gray-streams:stream-write-sequence ((compressing-stream compressing-stream) sequence start end &key)
  (salza2:compress-octet-vector sequence (compressor compressing-stream) :end end :start start))

(defmethod trivial-gray-streams:stream-finish-output ((compressing-stream compressing-stream))
  (finish-output (target-stream compressing-stream)))

(defmethod trivial-gray-streams:stream-clear-output ((compressing-stream compressing-stream))
  (clear-output (target-stream compressing-stream)))

(defmethod trivial-gray-streams:stream-force-output ((compressing-stream compressing-stream))
  (force-output (target-stream compressing-stream)))

(defmacro with-compressing-stream ((target output &optional (compressor ''salza2:gzip-compressor)) &body body)
  `(let ((,target (make-instance 'compressing-stream :target-stream ,output :compressor ,compressor)))
     (unwind-protect
          (progn ,@body)
       (salza2:finish-compression (compressor ,target))
       (finish-output ,target))))

(defgeneric serialize (object target)
  (:method (object (target stream))
    (with-compressing-stream (stream target)
      (cl-store:store object stream))))

(defgeneric deserialize (source)
  (:method ((source stream))
    (cl-store:restore (chipz:make-decompressing-stream 'chipz:gzip source))))
