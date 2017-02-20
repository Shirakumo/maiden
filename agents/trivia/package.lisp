#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-trivia
  (:nicknames #:org.shirakumo.maiden.agents.trivia)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities)
  ;; game.lisp
  (:export
   #:game
   #:channel
   #:questions
   #:question-time
   #:question-limit
   #:scores
   #:start
   #:end
   #:answer
   #:skip
   #:winner
   #:hint
   #:make-game)
  ;; interface.lisp
  (:export
   #:trivia
   #:start
   #:hint
   #:skip
   #:end
   #:add-question
   #:update-question
   #:remove-question
   #:add-categories
   #:remove-categories)
  ;; trivia.lisp
  (:export
   #:question
   #:text
   #:answers
   #:hint
   #:id
   #:check
   #:question
   #:category
   #:add-category
   #:remove-category
   #:categories
   #:add-question
   #:update-question
   #:remove-question
   #:load-questions
   #:save-questions))
