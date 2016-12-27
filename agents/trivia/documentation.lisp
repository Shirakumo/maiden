#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.trivia)

(docs:define-docs
  (type trivia
    "Provides a standard trivia game.")

  (command start
    "Start a new trivia game. You need to specify a number of categories from which questions can be selected. Users don't have to explicitly join. Whoever replies with the correct answer first wins the point.")

  (command hint
    "Request a hint for the current question.")

  (command skip
    "Skip the current question.")

  (command end
    "End the current trivia game. This will show the winner with the most points.")

  (command add-question
    "Add a new trivia question. The answers should be a string that is comma-separated.")

  (command update-question
    "Update an existing question. You need to use the ID of the question exactly.")

  (command remove-question
    "Remove an existing question. You need to use the ID of the question exactly.")

  (command add-categories
    "Add new categories for questions.")

  (command remove-categories
    "Remove categories for questions."))
