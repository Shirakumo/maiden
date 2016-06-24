#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.crimes)

(defclass player ()
  ((user :initarg :user :accessor user)
   (game :initarg :game :accessor game)
   (hand :initarg :hand :accessor hand)
   (score :initarg :score :accessor score)
   (result :initarg :result :accessor result))
  (:default-initargs
   :user (error "USER required.")
   :game (error "GAME required.")
   :hand ()
   :score 0
   :result NIL))

(defmethod complete-p ((player player))
  (complete-p (result player)))

(defmethod remaining-responses ((player player))
  (remaining-responses (result player)))

(defmethod draw-cards ((player player))
  (loop until (= (hand-size (game player)) (length (hand player)))
        do (push (pop (responses (game player))) (hand player)))
  player)

(defmethod prep-for-round ((player player))
  (draw-cards player)
  (setf (result player) (make-instance 'result
                                       :call (first (calls (game player))))))

(defclass game ()
  ((channel :initarg :channel :accessor channel)
   (calls :initarg :calls :accessor calls)
   (responses :initarg :responses :accessor responses)
   (players :initarg :players :accessor players)
   (scrambled :initarg :scrambled :accessor scrambled)
   (hand-size :initarg :hand-size :accessor hand-size)
   (win-score :initarg :win-score :accessor win-score)
   (in-session :initarg :in-session :accessor in-session))
  (:default-initargs
   :channel (error "CHANNEL required")
   :calls ()
   :responses ()
   :players ()
   :scrambled ()
   :hand-size 10
   :win-score 7
   :in-session NIL))

(defmethod add-deck ((deck deck) (game game))
  (when (in-session game)
    (error "You cannot add a deck while the game is in session!"))
  (setf (calls game) (nconc (calls game) (list-calls deck)))
  (setf (responses game) (nconc (responses game) (list-responses deck)))
  game)

(defmethod add-deck (name (game game))
  (add-deck (load-deck name) game))

(defmethod officer ((game game))
  (first (players game)))

(defmethod start ((game game))
  (unless (< 2 (length (players game)))
    (error "We can't start yet, there are not enough players!"))
  (unless (calls game)
    (error "We can't start yet, there are no call cards!"))
  (unless (responses game)
    (error "We can't start yet, there are no response cards!"))
  (setf (calls game) (alexandria:shuffle (calls game)))
  (setf (responses game) (alexandria:shuffle (responses game)))
  (setf (players game) (alexandria:shuffle (players game)))
  (setf (in-session game) T)
  (next-round game)
  game)

(defmethod end ((game game))
  (setf (in-session game) NIL))

(defmethod join ((user user) (game game))
  (when (find user (players game) :key #'user)
    (error "~a is already playing." (name user)))
  (let ((player (make-instance 'player :user user :game game)))
    ;; Late-joiner. Draw cards.
    (when (in-session game)
      (prep-for-round player))
    ;; We have to make sure to push to end, otherwise we'd mess up
    ;; the player indexes in SCRAMBLED and the current officer.
    (push-to-end player (players game))))

(defmethod leave ((user user) (game game))
  (let ((index (position user (players game) :key #'user)))
    (when index
      (setf (players game) (remove user (players game) :key #'user))
      ;; Leaving in the middle messes with the scrambling indexes.
      (setf (scrambled game) (loop for i in (scrambled game)
                                   unless (= i index)
                                   collect (if (< i index) i (1- index))))
      (unless (players game)
        (end game)))))

(defmethod submit ((response response) (player player) game)
  (unless (find response (hand player)) (error "~a is not a card in ~a's hand." response (name user)))
  (setf (hand player) (remove response (hand player)))
  (add-response response (result player)))

(defmethod submit ((index number) (player player) (game game))
  (unless (<= 0 index (length (hand player)))
    (error "Please choose a card between 0 and ~a from your hand." (length (hand player))))
  (submit (elt (hand player) index) player game))

(defmethod submit (response (user user) (game game))
  (let ((player (find user (players game) :key #'user)))
    (unless player (error "~a is not a player in this game." (name user)))
    (submit response player game)))

(defmethod complete-p ((game game))
  ;; Only check scrambled ones because those are players that joined
  ;; before the current round.
  (loop for index in (scrambled game)
        always (complete-p (elt (players game) index))))

(defmethod winner ((game game))
  (when (complete-p game)
    (setf (players game) (sort (players game) #'> :key #'score))
    (let ((winner (first (players game))))
      (values (user winner) (score winner)))))

(defmethod finish-round ((winner player) (game game))
  (unless (eql winner (officer game))
    (error "The officer cannot elect themselves as the winner."))
  (incf (score winner))
  (next-round game)
  winner)

(defmethod finish-round ((winner user) game)
  (let ((player (find winner (players game) :key #'user)))
    (unless player (error "~a is not a player in this game." (name winner)))
    (submit response player game)))

(defmethod finish-round ((winner string) game)
  (let ((player (find winner (players game) :key #'user :test #'matches)))
    (unless player (error "~a is not a player in this game." winner))
    (finish-round response player game)))

(defmethod finish-round ((winner integer) (game game))
  ;; Translate back from scrambled.
  (finish-round (elt (players game) (elt (scrambled game) winner)) game))

(defmethod next-round ((game game))
  (pop (calls game))
  (when (or (not (calls game))
            (find (win-score game) players :key #'score))
    (end game))
  (mapc #'prep-for-round (rotatef-list (players game)))
  (setf (scrambled game) (alexandria:shuffle (loop for i from 0 below (length (players game)) collect i)))
  game)
