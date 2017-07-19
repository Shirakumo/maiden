#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:maiden-lookup
  (:nicknames #:org.shirakumo.maiden.agents.lookup)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities #:maiden-api-access)
  ;; interface.lisp
  (:export
   #:lookup))
