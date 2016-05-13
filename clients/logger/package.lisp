#|
 This file is a part of Maiden
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(define-module #:maiden-logger
  (:nicknames #:org.shirakumo.maiden.clients.logger)
  (:use #:cl #:maiden)
  ;; 
  (:export
   #:log-event
   #:logger))
