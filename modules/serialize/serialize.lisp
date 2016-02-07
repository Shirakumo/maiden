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

