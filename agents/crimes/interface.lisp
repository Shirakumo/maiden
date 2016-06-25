#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.crimes)

(define-consumer crimes (agent)
  ((games :initform () :accessor games)))

(defmethod reply ((game game) message &rest args)
  (apply #'reply (channel game) message args))

(defun find-game (c ev &optional (error T))
  (or (find (channel ev) (games c) :key #'channel)
      (and error
           (error "No crimes game going on here."))))

(defun find-player (c ev &optional (error T))
  (let ((game (find-game c ev error)))
    (and game
         (or (find (user ev) (players game) :key #'user)
             (not error)
             (error "You are not part of this game.")))))

(defun user-game (c ev)
  (loop for game in (games c)
        do (when (loop for player in (players game)
                       thereis (eql (user ev) (user player)))
             (return game))))

(defun handle-next (game)
  (cond ((in-session game)
         (reply game "~a is the officer!" (name (user (officer game))))
         (reply game "~a" (text (first (calls game))))
         (dolist (player (players game))
           (unless (eql player (officer game))
             (send-to (user player) "Your hand: ~{~{(~a) ~a~}~^ ~}"
                      (loop for card in (hand player)
                            for i from 0
                            collect (list i (text card)))))))
        (T
         (multiple-value-bind (winner score) (winner game)
           (reply game "The game is over. The winner is ~a with ~a point~:p!"
                  (name winner) score)))))

(defun handle-complete (game)
  (when (complete-p game)
    (reply game "~a, time to convict a criminal.")
    ()))

(define-command (crimes open-game) (c ev &key (winning-score "7") (hand-size "10"))
  :command "open crimes"
  (when (find-game c ev NIL)
    (error "A game is already going on."))
  (let ((game (make-instance 'game
                             :channel (channel ev)
                             :win-score (parse-integer winning-score)
                             :hand-size (parse-integer hand-size))))
    (push game (games c))
    (reply ev "Game opened. You can join with \"join crimes\", ~
                            add decks with \"add crimes deck\", ~
                            and finally start with \"start crimes\".")))

(define-command (crimes start-game) (c ev)
  :command "start crimes"
  (let ((game (find-game c ev)))
    (start game)
    (handle-next game)))

(define-command (crimes stop-game) (c ev)
  :command "end crimes"
  (let ((game (find-game c ev)))
    (end game)
    (setf (games c) (remove game (games c)))
    (handle-next game)))

(define-command (crimes join-game) (c ev)
  :command "join crimes"
  (when (user-game c ev)
    (error "You are already participating in a game!"))
  (let ((game (find-game c ev)))
    (join (user ev) game)
    (reply ev "Welcome, ~a. We now have ~a player~:p."
           (name (user ev)) (length (players game)))))

(define-command (crimes leave-game) (c ev)
  :command "leave crimes"
  (leave (user ev) (find-game c ev))
  (reply ev "Ok. See you later, ~a" (name (user ev))))

(define-command (crimes submit-card) (c ev card)
  :command "submit crime"
  (let* ((game (find-game c ev))
         (player (find-player c ev))
         (result (result player)))
    (submit (parse-integer card) player game)
    (reply ev "Your submission: ~a" (text result))
    (if (complete-p result)
        (reply ev "Your submission is complete.")
        (reply ev "~a card~:p left to submit." (remaining-responses result)))
    (handle-complete game)))

(define-command (crimes select-winner) (c ev winner)
  :command "convict criminal"
  (let ((game (find-game c ev)))
    (unless (eql (user ev) (officer game))
      (error "Only the officer (~a) may convict a criminal."
             (name (user (officer game)))))
    (unless (complete-p game)
      (error "Not everyone has submitted their crimes yet! Stay patient."))
    (let ((winner (finish-round (parse-integer winner) game)))
      (reply ev "~a has been convicted! Their score is now at ~a point~:p."
             (name (user winner)) (score winner)))
    (handle-next game)))

(define-command (crimes create-deck) (c ev name)
  :command "create crime deck"
  (let ((deck (deck name)))
    (when deck (error "A deck with the name ~s already exists." (name deck))))
  (let ((deck (make-instance 'deck :name name)))
    (setf (deck name) deck)
    (reply ev "Deck ~s created. Add cards to it with \"add crime call\" and \"add crime response\"."
           (name deck))))

(define-command (crimes remove-deck) (c ev name)
  :command "remove crime deck"
  (let ((deck (deck name)))
    (remove-deck deck)
    (reply ev "Deck ~s removed." (name deck))))

(define-command (crimes add-call) (c ev deck &rest text)
  :command "add crime call"
  (let* ((deck (deck deck))
         (card (add-call (cl-ppcre:split "__+" (format NIL "~{~a~^ ~}" text)) deck)))
    (reply ev "Card ~a added to ~s." (id card) (name deck))))

(define-command (crimes add-response) (c ev deck &rest text)
  :command "add crime response"
  (let* ((deck (deck deck))
         (card (add-call (format NIL "~{~a~^ ~}" text) deck)))
    (reply ev "Card ~a added to ~s." (id card) (name deck))))

(define-command (crimes remove-card) (c ev card deck)
  :command "remove crime card"
  (let ((deck (deck deck)))
    (remove-card card deck)
    (reply ev "Card ~a removed from ~s." card (name deck))))
