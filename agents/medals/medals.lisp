#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.medals)

(defun maybe-account (name)
  (typecase name
    (user (or (maiden-accounts:account (maiden-accounts:identity name) :error NIL)
              (maiden-accounts:account name :error NIL)))
    (T (maiden-accounts:account name :error NIL))))

(defun user-name (name)
  (typecase name
    (user (name name))
    (T (string-downcase name))))

(defun medals (name)
  (let ((account (maybe-account name)))
    (union (when account
             (data-value 'medals account))
           (with-storage ('medals)
             (value (user-name name)))
           :test #'string-equal)))

(defun (setf medals) (value name)
  (let ((account (maybe-account name)))
    (cond (account
           (setf (data-value 'medals account) value)
           ;; Clear previous storage if any.
           (with-storage ('medals) (setf (value (user-name name)) ())))
          (T
           (with-storage ('medals)
             (setf (value (user-name name)) value))))))

(defun add-medals (name &rest medals)
  (setf (medals name) (union (medals name) medals :test #'string-equal)))

(defun remove-medals (name &rest medals)
  (setf (medals name) (set-difference (medals name) medals :test #'string-equal)))

(define-consumer medals (agent)
  ())

(define-command (medals show) (c ev &optional user)
  :command "show medals of"
  (let* ((user (or user (name (user ev))))
         (medals (medals user)))
    (if medals
        (reply ev "~a has been awarded the ~{~a~^, ~} medal~p." user medals (length medals))
        (reply ev "~a has not been awarded for anything yet!" user))))

(define-command (medals award) (c ev user &rest medals)
  :command "award"
  :advice (not public)
  (apply #'add-medals user medals)
  (reply ev "Congratulations, ~a! You have been awarded the ~{~a~^, ~} medal~p."
         user medals (length medals)))

(define-command (medals take) (c ev user &rest medals)
  :command "take medals from"
  :advice (not public)
  (apply #'remove-medals user medals)
  (reply ev "Medal~p ~{~a~^, ~} have been taken from ~a."
         (length medals) medals user))
