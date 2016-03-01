#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:colleen-user)
(define-module #:colleen-logger
  (:nicknames #:org.shirakumo.colleen.clients.logger)
  (:use #:cl #:colleen)
  ;; 
  (:export
   #:log-event
   #:logger))
