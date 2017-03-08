#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.trivia)

;; game.lisp
(docs:define-docs
  (type game
    "This class represents a trivia game.

It holds all necessary state information for a game to be
played.

See CHANNEL
See QUESTIONS
See QUESTION-TIME
See QUESTION-LIMIT
See SCORES
See START
See END
See ANSWER
See SKIP
See WINNER
See HINT
See MAKE-GAME")

  (function channel
    "Accessor to the channel that the game is being played on.

See MAIDEN-CLIENT-ENTITIES:CHANNEL
See GAME")

  (function questions
    "Accessor to the list of questions that the game can use.

See QUESTION
See GAME")

  (function question-time
    "Accessor to the universal-time that denotes when the current question was asked.

See GAME")

  (function question-limit
    "Accessor to the number of seconds players have to answer a question and get bonus points for.

See GAME")

  (function scores
    "Accessor to the alist of users to their scores.

See MAIDEN-CLIENT-ENTITIES:USER
See GAME")

  (function end
    "Stop the game.

See GAME")

  (function answer
    "Submit an answer for the current question in the game on behalf of a user.

If the answer is incorrect or the game is not running, nothing happens.
Otherwise, the scores are adjusted and the next question is made current.

See GAME
See CHECK")

  (function skip
    "Skip the current question.

See GAME")

  (function winner
    "Return the winner of the game.

This only returns something if the game is not running.

See GAME
See SCORES")

  (function hint
    "Return a hint for the current question if there is one.

This will void all time bonus points.

See GAME
See QUESTION")

  (function make-game
    "Create a new trivia game for the given channels, using questions from the specified categories.

See CATEGORIES
See GAME"))

;; interface.lisp
(docs:define-docs
  (type trivia
    "Provides a standard trivia game.")

  (function games
    "Accessor to the list of games that are currently being played.

See TRIVIA
See GAME")

  (function game
    "Attempt to find a suitable game instance for the given client and event.

If error is non-NIL and no game could be found, an error is signalled.

See GAMES
See TRIVIA")

  (function handle-next
    "Handle the next round start in the game.

This will pose the next question if there is any, and otherwise
announce the winner of the game and remove it from the list of
active games.

See GAMES
See TRIVIA")

  (function split
    "Split the string by the given character.

Only non-empty substrings are gathered into the resulting list.")

  (command start-game
    "Start a new trivia game. You need to specify a number of categories from which questions can be selected. Users don't have to explicitly join. Whoever replies with the correct answer first wins the point.")

  (command hint
    "Request a hint for the current question.")

  (command skip
    "Skip the current question.")

  (command end-game
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

;; trivia.lisp
(docs:define-docs
  (variable *questions*
    "This is a hash table associating IDs to question instances.

See QUESTION
See REMQUESTION
See LOAD-QUESTIONS
See SAVE-QUESTIONS")

  (variable *categories*
    "This is a hash table associating category names to lists of questions.

See CATEGORY
See ADD-CATEGORY
See REMOVE-CATEGORY
See CATEGORIES")

  (type question
    "Class to represent a trivia question.

A question contains a question text and a list of possible answers.
It may also include an optional hint to help players find an answer.
Each question gets a unique ID to identify it by.

See TEXT
See ANSWERS
See HINT
See ID
See ADD-QUESTION
See UPDATE-QUESTION
See REMOVE-QUESTION")

  (function text
    "Accessor to the text of the question.

See QUESTION")

  (function answers
    "Accessor to the list of accepted answers for the question.

Each answer should represent a substring that a potential answer
must contain in order for it to be accepted as an answer to the
question. This is done to reduce the potential for speech artefacts
marking correct questions as invalid.

See QUESTION")

  (function hint
    "Accessor to the potential hint the question has to its solution.

May be a string or NIL.

See QUESTION")

  (function check
    "Returns true if the answer is an accepted answer for the question.

See QUESTION
See ANSWERS")

  (function question
    "Accessor to the question instance of the given ID.

See *QUESTIONS*
See QUESTION")

  (function remquestion
    "Remove the question instance of the given ID.

Note that this will not clean up potential categories the question was in.

See *QUESTIONS*
See QUESTION")

  (function category
    "Accessor to the list of question cards contained in the category.

If no such category is known, an error is signalled.

See *CATEGORIES*
See QUESTION")

  (function add-category
    "Create a new category or add a card to an existing category.

ID has to be either a question's ID or a question object.

See *CATEGORIES*")

  (function remove-category
    "Remove the category of the given name or the given card from the category.

ID has to be either NIL (for the entire category), or the
ID of a question, or a question object.

See *CATEGORIES*")

  (function categories
    "Return a list of all known categories, or a list of categories for the given question.

ID has to be either NIL (for all categories), or the ID of
question, or a question object.

See *CATEGORIES*")

  (function add-question
    "Add a new question to the system.

See ADD-CATEGORY
See QUESTION")

  (function update-question
    "Update an existing question with new fields.

If a question of the requested ID does not exist, an error
is signalled.

See QUESTION")

  (function remove-question
    "Remove the question of the given ID from the system.

This also makes sure to remove the ID references from all
categories the question was in previously.

See *CATEGORIES*
See REMQUESTION")

  (function load-questions
    "Load all question cards and categories from the disk storage.

See MAIDEN-STORAGE:RESTORE")

  (function save-questions
    "Save all question cards and categories to the disk storage.

See MAIDEN-STORAGE:OFFLOAD"))
