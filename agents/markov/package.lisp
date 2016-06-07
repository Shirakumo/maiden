#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-markov
  (:nicknames #:org.shirakumo.maiden.agents.markov)
  (:use #:cl #:maiden #:maiden-activatable #:maiden-commands)
  ;; markov.lisp
  (:export
   #:markov))
