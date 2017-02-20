#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-commands
  (:nicknames #:org.shirakumo.maiden.agents.commands)
  (:use #:cl #:maiden #:maiden-client-entities)
  ;; dispatch.lisp
  (:export
   #:commands)
  ;; extraction.lisp
  (:export
   #:command-extractor
   #:remove-command-extractor
   #:define-command-extractor
   #:extract-command
   #:command-p
   #:prefix)
  ;; invocation.lisp
  (:export
   #:framework-message
   #:issue-message
   #:command-event
   #:public
   #:dispatch-event
   #:relay
   #:command-invoker
   #:name
   #:docstring
   #:invoker
   #:lambda-list
   #:remove-command-invoker
   #:list-command-invokers
   #:find-command-invoker
   #:define-command-invoker
   #:command
   #:define-simple-command-invoker
   #:define-command
   #:remove-command
   #:consumer-commands
   #:find-matching-command)
  ;; parsing.lisp
  (:export
   #:&string
   #:command-condition
   #:lexing-error
   #:expected-key-error
   #:destructuring-error
   #:not-enough-arguments-error
   #:with-command-destructuring-bind))
