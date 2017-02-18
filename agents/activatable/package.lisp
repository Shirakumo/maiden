#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-activatable
  (:nicknames #:org.shirakumo.maiden.agents.activatable)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities)
  ;; activatable.lisp
  (:export
   #:activate
   #:deactivate
   #:active-p
   #:list-active
   #:activatable-handler
   #:activatable))
