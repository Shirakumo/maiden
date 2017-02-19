#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-counter
  (:nicknames #:org.shirakumo.maiden.agents.counter)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities #:maiden-activatable)
  ;; counter.lisp
  (:shadow #:remove #:list)
  (:export
   #:counter
   #:remove-counter
   #:set-counter
   #:list-counters
   #:add
   #:change
   #:remove
   #:list))
