#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-api-access
  (:nicknames #:org.shirakumo.maiden.modules.api-access)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  (:use #:cl #:maiden)
  (:export
   #:request
   #:parse-to
   #:request-as
   #:json-v))

(use-package '#:maiden-api-access '#:maiden-user)
