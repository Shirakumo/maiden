#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-logger
  (:nicknames #:org.shirakumo.maiden.clients.logger)
  (:use #:cl #:maiden #:maiden-client-entitiesk)
  ;; 
  (:export
   #:log-event
   #:logger))
