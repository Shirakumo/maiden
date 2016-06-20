#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-urlinfo
  (:nicknames #:org.shirakumo.maiden.agents.urlinfo)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities)
  ;; urlinfo.lisp
  (:export
   #:urlinfo))
