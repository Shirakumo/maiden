#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-trivia
  (:nicknames #:org.shirakumo.maiden.agents.trivia)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities)
  ;; generator.lisp
  (:export
   #:interface)
  ;; interface.lisp
  (:export
   #:trivia))
