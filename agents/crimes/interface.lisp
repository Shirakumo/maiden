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

(defmethod reply ((player player) message &rest args)
  (apply #'reply (user player) message args))

(defun find-game (c ev &optional (error T))
  (or (find (channel ev) (games c) :key #'channel)
      (and error (error "No crimes game going on here."))))

(defun find-player (c ev &optional (error T))
  (let ((game (user-game c ev error)))
    (and game
         (or (find (user ev) (players game) :key #'user)
             (and error (error "You are not part of this game."))))))

(defun user-game (c ev &optional (error T))
  (or (loop for game in (games c)
            do (when (loop for player in (players game)
                           thereis (eql (user ev) (user player)))
                 (return game)))
      (and error (error "You are not part of this game."))))

(defun handle-next (game)
  (cond ((in-session game)
         (reply game "~a is the officer!" (name (user (officer game))))
         (reply game "Prompt: ~a" (text (result (officer game))))
         (dolist (player (players game))
           (loop for card in (hand player)
                 for i from 0
                 do (reply player "(~a) ~a" i (text card)))))
        (T
         (multiple-value-bind (winner score) (winner game)
           (reply game "The game is over.~@[ The winner is ~a with ~a point~:p!~]"
                  (when winner (name winner)) score)))))

(defun handle-complete (game)
  (when (complete-p game)
    (reply game "~a, time to convict a criminal."
           (name (user (officer game))))
    (loop for num in (scrambled game)
          for player = (elt (players game) num)
          for i from 0
          do (reply game "(~a): ~a" i (text (result player))))))

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

(define-command (crimes add-deck) (c ev name/id)
  :command "add crimes deck"
  (let ((game (find-game c ev)))
    (unless (find (user ev) (players game) :key #'user)
      (error "You are not a player of this game."))
    (let ((deck (deck (or (ignore-errors (deck name/id))
                          (ignore-errors (load-cardcast-deck name/id))
                          (error "No deck ~a found locally or on cardcast." name/id)))))
      (add-deck deck game)
      (reply game "Deck ~a has been added. The game now has ~a call~:p and ~a response~:p."
             (name deck) (length (calls game)) (length (responses game))))))

(define-command (crimes start-game) (c ev)
  :command "start crimes"
  (let ((game (find-game c ev)))
    (unless (find (user ev) (players game) :key #'user)
      (error "You are not a player of this game."))
    (start game)
    (dolist (player (players game))
      (reply player "Reminder: Use \"commit crime\" with the number of the response you want to use next. Some calls require multiple responses."))
    (handle-next game)))

(define-command (crimes end-game) (c ev)
  :command "end crimes"
  (let ((game (find-game c ev)))
    (unless (find (user ev) (players game) :key #'user)
      (error "You are not a player of this game."))
    (when (in-session game)
      (end game)
      (handle-next game))
    (setf (games c) (remove game (games c)))))

(define-command (crimes join-game) (c ev)
  :command "join crimes"
  (when (user-game c ev NIL)
    (error "You are already participating in a game!"))
  (let ((game (find-game c ev)))
    (join (user ev) game)
    (reply ev "Welcome, ~a. We now have ~a player~:p."
           (name (user ev)) (length (players game)))))

(define-command (crimes leave-game) (c ev)
  :command "leave crimes"
  (unless (find (user ev) (players game) :key #'user)
    (error "You are not a player of this game."))
  (leave (user ev) (find-game c ev))
  (reply ev "Ok. See you later, ~a" (name (user ev))))

(define-command (crimes submit-card) (c ev &rest cards)
  :command "commit crime"
  (let* ((game (user-game c ev))
         (player (find-player c ev))
         (result (result player)))
    (dolist (card cards)
      (submit (parse-integer card) player game))
    (reply player "Your submission: ~a" (text result))
    (if (complete-p result)
        (reply player "Your submission is complete.")
        (reply player "~a card~:p left to submit." (remaining-responses result)))
    (handle-complete game)))

(define-command (crimes select-winner) (c ev winner)
  :command "convict criminal"
  (let ((game (user-game c ev)))
    (unless (eql (user ev) (user (officer game)))
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
  (let ((deck (ignore-errors (deck name))))
    (when deck (error "A deck with the name ~a already exists." (name deck))))
  (let ((deck (make-instance 'deck :name name)))
    (setf (deck name) deck)
    (reply ev "Deck ~a created. Add cards to it with \"add crime call\" and \"add crime response\"."
           (name deck))))

(define-command (crimes remove-deck) (c ev name)
  :command "remove crime deck"
  (let ((deck (deck name)))
    (remove-deck deck)
    (reply ev "Deck ~a removed." (name deck))))

(define-command (crimes list-decks) (c ev)
  :command "list crime decks"
  (let ((files (uiop:directory-files
                (uiop:pathname-directory-pathname
                 (maiden-storage:config-pathname 'local-symbol)))))
    (reply ev "Local decks: ~{~a~^, ~}" (mapcar #'pathname-name files))))

(define-command (crimes search-deck) (c ev search)
  :command "search for crime deck"
  (let ((decks (find-cardcast-decks search)))
    (if decks
        (reply ev "Cardcast decks found: ~{~{~a (~a)~}~^, ~}" decks)
        (reply ev "No decks for that term found on cardcast."))))

(define-command (crimes download-deck) (c ev id &optional new-name)
  :command "download crime deck"
  (let* ((deck (load-cardcast-deck id))
         (name (or new-name (name deck))))
    (let ((deck (ignore-errors (deck name))))
      (when deck (error "A deck with the name ~a already exists." (name deck))))
    (setf (deck name) deck)
    (reply ev "Cardcast deck downloaded to name ~a." (name deck))))

(define-command (crimes add-call) (c ev deck &rest text)
  :command "add crime call"
  (let* ((deck (deck deck))
         (card (add-call (format NIL "~{~a~^ ~}" text) deck)))
    (save-deck deck)
    (reply ev "Card ~a added to ~a." (id card) (name deck))))

(define-command (crimes add-response) (c ev deck &rest text)
  :command "add crime response"
  (let* ((deck (deck deck))
         (card (add-call (format NIL "~{~a~^ ~}" text) deck)))
    (save-deck deck)
    (reply ev "Card ~a added to ~a." (id card) (name deck))))

(define-command (crimes remove-card) (c ev card deck)
  :command "remove crime card"
  (let ((deck (deck deck)))
    (remove-card card deck)
    (save-deck deck)
    (reply ev "Card ~a removed from ~a." card (name deck))))
