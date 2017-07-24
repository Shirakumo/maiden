#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.notify)

(define-consumer notify (agent)
  ())

(defun handle-note-notification (ev user trigger)
  (dolist (note (sort (user-notes user) #'< :key #'date))
    (when (eql (trigger note) trigger)
      (remove-note note)
      (reply ev "~a: ~a said ~a: ~a"
             (name user) (from note) (format-time (date note)) (message note)))))

(defun handle-note-creation (ev target message trigger)
  (cond ((string= "" target)
         (reply ev "I'll have to know someone to tell the message to."))
        ((string-equal (name (user ev)) target)
         (reply ev "Are you feeling lonely?"))
        (T
         (make-note (name (user ev))
                    (normalize-user-name target)
                    message
                    :trigger trigger)
         (reply ev "~a: Got it. I'll let ~a know as soon as possible."
                (name (user ev)) target))))

(define-handler (notify new-message message-event) (c ev user)
  (unless (maiden-commands:command-p ev)
    (handle-note-notification ev user :message)))

(define-handler (notify user-enter user-entered) (c ev user)
  (handle-note-notification ev user :join))

(define-command (notify forget-notes) (c ev &optional target)
  :command "forget notes"
  :before '(new-message)
  (if target
      (dolist (note (user-notes target))
        (when (string-equal (name (user ev)) (from note))
          (remove-note note)))
      (clear-notes (user ev)))
  (reply ev "Ok, I forgot all about the notes."))

(define-command (notify send-join-note) (c ev target &string message)
  :command "notify on join"
  (handle-note-creation ev target message :join))

(define-command (notify send-note) (c ev target &string message)
  :command "notify"
  (handle-note-creation ev target message :message))
