#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:maiden-lastfm
  (:nicknames #:org.shirakumo.maiden.agents.lastfm)
  (:use #:cl #:maiden #:maiden-client-entities #:maiden-commands #:maiden-api-access)
  ;; interface.lisp
  (:export
   #:lastfm))
