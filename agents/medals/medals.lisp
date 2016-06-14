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
  :command "show medals"
  (reply ev "Medals for ~a: ~{~a~^, ~}"
         (or user (name (user ev))) (medals (or user (user ev)))))

(define-command (medals award) (c ev user &rest medals)
  :command "award medals"
  :advice (not public)
  (apply #'add-medals user medals)
  (reply ev "Medals ~{~a~^, ~} awarded to ~a."
         medals user))

(define-command (medals take) (c ev user &rest medals)
  :command "take medals"
  :advice (not public)
  (apply #'remove-medals user medals)
  (reply ev "Medals ~{~a~^, ~} taken from ~a."
         medals user))
