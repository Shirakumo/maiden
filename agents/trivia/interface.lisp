#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.trivia)

(define-consumer trivia (agent)
  ((games :initform () :accessor games)))

(defun game (c ev &optional (error T))
  (or (find (channel ev) (games c) :key #'channel)
      (and error
           (error "No trivia game going on here."))))

(defun handle-next (c ev game)
  (cond ((null (questions game))
         (setf (games c) (remove game (games c)))
         (multiple-value-bind (winner score) (winner game)
           (reply ev "The game is over. ~@[~a is the winner with ~a point~:p!~]"
                  (when winner (name winner)) score)))
        (T
         (reply ev "Question: ~a" (text (first (questions game)))))))

(define-command (trivia start) (c ev &rest categories)
  :command "start trivia game"
  (when (game c ev NIL)
    (error "There's already a trivia game going on here!"))
  (load-questions)
  (let ((game (make-game (channel ev) categories)))
    (unless (questions game)
      (error "Please pick categories that actually contain some questions."))
    (push game (games c))
    (start game)
    (reply ev "Game started! Quick answers get bonus points. Use the \"hint answer\" or \"skip question\" commands if you don't know further.")
    (handle-next c ev game)))

(define-command (trivia hint) (c ev)
  :command "hint answer"
  (let ((hint (hint (game c ev))))
    (if hint
        (reply ev "~a" hint)
        (reply ev "No hint available, sorry! Use \"skip question\" if you don't know further."))))

(define-command (trivia skip) (c ev)
  :command "skip question"
  (let* ((game (game c ev))
         (trivia (skip game)))
    (reply ev "The correct answer~p~:* would have been: ~{~a~^, ~}"
           (answers trivia))
    (handle-next c ev game)))

(define-command (trivia end) (c ev)
  :command "end trivia game"
  (let ((game (game c ev)))
    (end game)
    (handle-next c ev game)))

(define-handler (trivia handler (and message-event passive-event)) (c ev message)
  (unless (command-p ev)
    (let ((game (game c ev NIL)))
      (when game
        (let ((correct (answer (user ev) message game)))
          (when correct
            (reply ev "Correct! ~a wins this round." (name (user ev)))
            (handle-next c ev game)))))))


(defun split (string char)
  (let ((out (make-string-output-stream))
        (parts ()))
    (flet ((maybe-push (a)
             (when (and a (string/= a "")) (push a parts))))
      (loop for c across string
            do (if (char= c char)
                   (maybe-push (get-output-stream-string out))
                   (write-char c out)))
      (maybe-push (get-output-stream-string out))
      (nreverse parts))))

(define-command (trivia add-question) (c ev question answers &key hint (categories ""))
  :command "add trivia question"
  (let ((trivia (add-question question (split answers #\,) :hint hint :categories (split categories #\,))))
    (save-questions)
    (reply ev "Trivia question (#~a) added." (id trivia))))

(define-command (trivia update-question) (c ev id &key question answers hint categories)
  :command "update trivia question"
  (let ((id (parse-integer id)))
    (when question (update-question id :question question))
    (when answers (update-question id :answers (split answers #\,)))
    (when hint (update-question id :hint hint))
    (when categories (update-question id :categories (split categories #\,))))
  (save-questions)
  (reply ev "Trivia question updated."))

(define-command (trivia remove-question) (c ev id)
  :command "remove trivia question"
  (remove-question (or (question (parse-integer id)) (error "No such trivia question.")))
  (save-questions)
  (reply ev "Trivia question removed."))

(define-command (trivia add-categories) (c ev id &rest categories)
  :command "add trivia categories"
  (dolist (category categories)
    (add-category category (parse-integer id)))
  (save-questions)
  (reply ev "Categories added."))

(define-command (trivia remove-categories) (c ev id &rest categories)
  :command "remove trivia categories"
  (or (question id) (error "No such trivia question."))
  (dolist (category categories)
    (remove-category category (parse-integer id)))
  (save-questions)
  (reply ev "Categories removed."))
