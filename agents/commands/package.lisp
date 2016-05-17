#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-commands
  (:nicknames #:org.shirakumo.maiden.agents.commands)
  (:use #:cl #:maiden)
  ;; dispatch.lisp
  (:export
   #:commands)
  ;; extraction.lisp
  (:export
   #:command-extractor
   #:remove-command-extractor
   #:define-command-extractor
   #:extract-command
   #:prefix)
  ;; invocation.lisp
  (:export
   #:framework-message
   #:issue-message
   #:command-event
   #:dispatch-event
   #:command-invoker
   #:remove-command-invoker
   #:define-command-invoker
   #:define-command
   #:find-matching-command)
  ;; parsing.lisp
  (:export
   #:command-condition
   #:lexing-error
   #:expected-key-error
   #:destructuring-error
   #:not-enough-arguments-error
   #:with-command-destructuring-bind))
