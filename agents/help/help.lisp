#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.help)

(define-consumer help (agent)
  ((start-time :initarg :start-time :initform (get-universal-time) :accessor start-time)))

(define-command (help about) (c ev &rest about)
  :command "help"
  (let ((command (format NIL "~{~(~a~)~^ ~}" about)))
    (cond ((string= command "")
           (reply ev "See 'help about' for general information. Try 'help X' to search for or retrieve information about a command."))
          ((string= command "uptime")
           (relay ev 'about-uptime))
          ((string= command "about")
           (relay ev 'about-self))
          ((not (find-command-invoker command))
           (relay ev 'about-term :term command))
          (T
           (relay ev 'about-command :command command)))))

(define-command (help about-self) (c ev)
  :command "about"
  (reply ev "I'm an installation of the Maiden ~a chat framework. The core is running ~d consumer~:p, with ~d command~:p registered. I have been running for approximately ~a."
         (asdf:component-version (asdf:find-system :maiden T))
         (length (consumers (core ev)))
         (length (list-command-invokers))
         (format-relative-time (- (get-universal-time) (start-time c)))))

(define-command (help about-uptime) (c ev)
  :command "uptime"
  (reply ev "I have been running for approximately ~a since ~a."
         (format-relative-time (- (get-universal-time) (start-time c)))
         (format-absolute-time (start-time c))))

(define-command (help about-term) (c ev term)
  :command "search"
  (let ((ranks (sort (loop for command in (list-command-invokers)
                           for prefix = (prefix command)
                           collect (list prefix (maiden-commands::levenshtein-distance prefix term)))
                     #'< :key #'second)))
    (reply ev "I found the following commands: ~{~a~^, ~}"
           (loop for (command rank) in ranks
                 repeat 10
                 collect command))))

(define-command (help about-command) (c ev command)
  :command "explain"
  (let ((invoker (find-command-invoker command)))
    (if invoker
        (reply ev "Command Syntax: ~a ~(~{~a~^ ~}~)~%~
                   Documentation:  ~:[None.~;~:*~a~]"
               (prefix invoker) (lambda-list invoker) (docstring invoker))
        (reply ev "No such command found."))))
