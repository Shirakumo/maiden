#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-blocker
  (:nicknames #:org.shirakumo.maiden.agents.blocker)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities)
  ;; blocker.lisp
  (:export
   #:blocker))
