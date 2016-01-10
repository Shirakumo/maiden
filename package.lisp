#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:colleen
  (:nicknames #:org.shirakumo.colleen)
  (:use #:cl #:deeds)
  ;; event-loop.lisp
  (:export
   #:colleen-event-loop
   #:*event-loop*)
  ;; toolkit.lisp
  (:export
   #:with-default-encoding))
