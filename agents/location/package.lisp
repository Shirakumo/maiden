#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-location
  (:nicknames #:org.shirakumo.maiden.agents.location)
  (:use #:cl #:maiden #:maiden-api-access #:maiden-commands #:maiden-client-entities)
  ;; location.lisp
  (:export
   #:geo-information
   #:coordinates
   #:address
   #:location))
