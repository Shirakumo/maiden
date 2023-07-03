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
   #:start-game
   #:hint
   #:skip
   #:end-game
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
