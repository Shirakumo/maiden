#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.vote)

(define-consumer vote (agent)
  ((votes :initform (make-hash-table :test 'eq) :accessor votes)))

(defun integer->letter (int)
  (code-char (+ int (1- (char-code #\a)))))

(defun letter->integer (char)
  (- (char-code (char-downcase char)) (1- (char-code #\a))))

(defun end-vote (c ev)
  (let ((options (gethash (channel ev) (votes c))))
    (when options
      (remhash (channel ev) (votes c))
      (let* ((options (sort (rest options) #'> :key #'length))
             (winning (first options)))
        (reply ev "Voting is now closed. The winning option, with ~d out of ~d votes, is:"
               (length (rest winning))
               (loop for option in options
                     summing (length (rest option))))
        (reply ev "  ~a" (first winning))))))

(define-command (vote start-vote) (c ev &rest options)
  :command "vote between"
  (when (gethash (channel ev) (votes c))
    (error "There's already a vote going on in this channel"))
  (when (= 0 (length options))
    (error "You'll have to provide at least one option, come on."))
  (when (< 26 (length options))
    (error "More than 26 options for a vote is not possible. If this was not your intention, please make sure to enclose each option in double-quotes like so:~%  ::vote between \"egg\" \"chicken\""))
  (setf (gethash (channel ev) (votes c))
        (list* (user ev)
               (mapcar #'list options)))
  (reply ev "Please vote using the letters corresponding to the option you like.")
  (loop for i from 1
        for option in options
        do (reply ev "~c. ~a" (integer->letter i) option)))

(define-handler (vote vote-counter (and message-event passive-event)) (c ev message)
  (cl-ppcre:register-groups-bind (option) ("\\s*(\\w)[.)!]\\s*" message)
    (let* ((options (rest (gethash (channel ev) (votes c))))
           (option (nth (1- (letter->integer (elt option 0))) options)))
      (when (and option (not (find (user ev) (cdr option))))
        (setf (cdr option) (cons (user ev) (cdr option)))
        ;; If all users in the channel voted, end it automatically.
        (when (loop for user in (users (channel ev))
                    always (or (eql (name user) (username c))
                               (loop for option in options
                                     thereis (find user (rest option)))))
          (end-vote c ev))))))

(define-command (vote end-vote) (c ev option)
  :command "end vote"
  (let ((options (gethash (channel ev) (votes c))))
    (unless options
      (error "There is no vote going on in this channel."))
    (unless (eql (first options) (user ev))
      (error "Only the user who started the vote may end it."))
    (end-vote c ev)))
