(in-package #:org.shirakumo.maiden.agents.trivia)

(defclass game ()
  ((channel :initarg :channel :accessor channel)
   (questions :initarg :questions :accessor questions)
   (question-time :initarg :question-time :accessor question-time)
   (question-limit :initarg :question-limit :accessor question-limit)
   (scores :initarg :scores :accessor scores))
  (:default-initargs
   :channel (error "CHANNEL required.")
   :questions (error "QUESTIONS required.")
   :question-time (get-universal-time)
   :question-limit 30
   :scores ()))

(defmethod start ((game game))
  (setf (questions game) (alexandria:shuffle (questions game)))
  (setf (question-time game) (get-universal-time))
  (setf (scores game) ())
  game)

(defmethod end ((game game))
  (setf (questions game) ())
  game)

(defmethod answer (user answer (game game))
  (when (and (questions game)
             (check answer (first (questions game))))
    (let* ((diff (- (get-universal-time) (question-time game)))
           (score (+ 1 (max 0 (- (question-limit game) diff))))
           (cons (assoc user (scores game) :test #'matches)))
      (if cons
          (incf (cdr cons) score)
          (push (cons user score) (scores game)))
      (skip game))))

(defmethod skip ((game game))
  (setf (question-time game) (get-universal-time))
  (pop (questions game))
  game)

(defmethod winner ((game game))
  (unless (questions game)
    (setf (scores game) (sort (scores game) #'> :key #'cdr))
    (let ((winner (first (scores game))))
      (values (car winner) (cdr winner)))))

(defmethod hint ((game game))
  (when (questions game)
    (let ((hint (hint (first (questions game)))))
      ;; Make extra points void if hint used.
      (when hint (setf (question-time game) 0))
      hint)))

(defun make-game (channel categories &key (limit 30))
  (make-instance 'game :channel channel
                       :questions (loop for category in categories
                                        append (category category))
                       :question-limit limit))
