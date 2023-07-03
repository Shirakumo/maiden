(in-package #:org.shirakumo.maiden.agents.help)

(define-consumer help (agent)
  ((start-time :initarg :start-time :initform (get-universal-time) :accessor start-time)))

(defun find-consumer (core name)
  (loop for consumer in (consumers core)
        do (when (or (matches name consumer)
                     (string-equal name (name consumer))
                     (string-equal name (class-name (class-of consumer))))
             (return consumer))))

(define-command (help about) (c ev &string command)
  :command "help"
  (cond ((string= command "")
         (reply ev "See 'help about' for general information. Try 'help X' to search for or retrieve information about a command."))
        ((string= command "uptime")
         (relay ev 'about-uptime))
        ((string= command "about")
         (relay ev 'about-self))
        ((find-command-invoker command)
         (relay ev 'about-command :command command))
        ((find-consumer (core ev) command)
         (relay ev 'about-consumer :consumer command))
        (T
         (relay ev 'about-term :term command))))

(define-command (help about-self) (c ev)
  :command "about self"
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

(define-command (help about-command) (c ev &string command)
  :command "about command"
  (let ((invoker (find-command-invoker command))
        (*print-case* :downcase))
    (unless invoker
      (reply ev "No such command found."))
    (reply ev "Command Syntax: ~a ~{~:@(~a~)~^ ~}~%~
               Documentation:  ~:[None.~;~:*~a~]"
           (prefix invoker) (lambda-list invoker) (documentation invoker T))))

(define-command (help list-consumers) (c ev)
  :command "list consumers"
  (reply ev "Active consumers on the current core: ~{~a~^, ~}"
         (loop for consumer in (consumers (core ev))
               when (running consumer)
               collect (or (name consumer)
                           (class-name (class-of consumer))))))

(define-command (help about-consumer) (c ev consumer)
  :command "about consumer"
  (let ((consumer (find-consumer (core ev) consumer)))
    (unless consumer
      (error "No consumer of that name or ID found on the current core."))
    (unless (documentation (class-of consumer) T)
      (error "No documentation for ~a is available." (name consumer)))
    (reply ev "~a" (documentation (class-of consumer) T))
    (reply ev "~:[This consumer does not provide any commands.~;~
               ~:*This consumer provides the following commands: ~{~a~^, ~}~]"
           (sort (mapcar #'prefix (consumer-commands consumer)) #'string<))))

(define-command (help about-term) (c ev &string term)
  :command "search"
  (let ((ranks (sort (loop for command in (list-command-invokers)
                           for prefix = (prefix command)
                           collect (list prefix (maiden-commands::levenshtein-distance prefix term)))
                     #'< :key #'second)))
    (reply ev "I found the following commands: ~{~a~^, ~}"
           (loop for (command rank) in ranks
                 repeat 10
                 collect command))))
