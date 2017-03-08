#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-vote
  (:nicknames #:org.shirakumo.maiden.agents.vote)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities)
  ;; vote.lisp
  (:export
   #:vote
   #:start-vote
   #:end-vote))
