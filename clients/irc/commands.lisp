#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.irc)

;; This leaves 152 characters to encode the preamble and CRLF of a message.
;; That should be enough while still sending a fairly long message over the net.
;; Calculating the limit precisely would involve querying the hostname as seen
;; by others as well as the precise manner in which things are encoded on the wire.
;; Since that is somewhat too complicated and 360 is still plenty long we settle
;; for this compromise. The protocols don't actually say anything about the length
;; restrictions of the username, so we cannot make a worst-case estimate limit.
;; However, we do know that the hostname is limited to 63 characters, we need 2 for
;; the line ending, ~10 for the command, and ~36 for the nickname. The nick length
;; here being overly long especially considering the initial RFCs limit it to 9,
;; which is way too short to be relied upon. Either way, 152 should be a good limit.
(defvar *message-length-limit* 360)

(define-event send-event (irc-event outgoing-event active-event)
  ((message :initarg :message :reader message)))

(defmacro define-irc-command (name args &body options-and-body)
  (let ((name (intern (string name) '#:org.shirakumo.maiden.clients.irc.events))
        (pure-args (lambda-fiddle:extract-lambda-vars args))
        (client (gensym "CLIENT")))
    (form-fiddle:with-body-options (body options superclasses) options-and-body
      `(progn
         (define-event ,name (instruction-event send-event ,@superclasses)
           ,(maiden::slot-args->slots (rest args))
           ,@options)
         (defun ,name (,client ,@(maiden::slot-args->args (rest args)))
           (do-issue (first (cores ,client)) ,name
             :client ,client ,@(loop for var in (rest pure-args) collect (kw var) collect var)))
         (defmethod message ((,(first args) ,name))
           ,@body)))))

(defmacro define-simple-irc-command (name args &body options-and-body)
  (let ((name (intern (string name) '#:org.shirakumo.maiden.clients.irc.events))
        (pure-args (lambda-fiddle:extract-lambda-vars args))
        (ev (gensym "EVENT")))
    (form-fiddle:with-body-options (body options) options-and-body
      `(define-irc-command ,name (,ev ,@args)
         ,@options
         (deeds:with-fuzzy-slot-bindings ,pure-args (,ev ,name)
           (format NIL ,@body))))))

(defun splittable-char-p (char)
  (or (not (graphic-char-p char))
      ;; This isn't great, but hey.
      (find char " .,;:?!　。、：；？！")))

(defun reasonable-message-end (string start end max-backtrack)
  (let ((pos (position NIL string :start start :end end :from-end T :test-not #'eql :key #'splittable-char-p)))
    (or (when (and pos (<= (- end pos) max-backtrack)) (1+ pos))
        end)))

(defun split-message-smartly (length message &key (max-backtrack 10))
  (cond ((<= (length message) length)
         (list message))
        (T
         (let ((parts ()))
           (loop for start = 0 then real-end
                 for end = length then (min (length message) (+ start length))
                 for real-end = (reasonable-message-end message start end max-backtrack)
                 while (< end (length message))
                 do (push (subseq message start real-end) parts)
                 finally (push (subseq message start end) parts))
           (nreverse parts)))))

;; The way we split here is Not Great™ since we do not take into account the other
;; arguments nor the command string itself and instead just rely on the hope that
;; *mesage-length-limit* will be conservative enough to suffice for an estimate. This
;; is obviously not always the case depending on how many arguments there are, how long
;; they are and how long the bot's nick, username, and hostname are on the server side.
(defmacro define-message-irc-command (name args &body options-and-body)
  (let* ((name (intern (string name) '#:org.shirakumo.maiden.clients.irc.events))
         (pure-args (lambda-fiddle:extract-lambda-vars args))
         (message (car (last pure-args)))
         (ev (gensym "EVENT")))
    (form-fiddle:with-body-options (body options) options-and-body
      `(define-irc-command ,name (,ev ,@args)
         ,@options
         (deeds:with-fuzzy-slot-bindings ,pure-args (,ev ,name)
           (dolist (,message (split-message-smartly *message-length-limit* ,message))
             (format NIL ,@body)))))))

(define-simple-irc-command pass (password)
  "PASS ~a" password)

(define-simple-irc-command nick (nickname &key hopcount)
  "NICK ~a~@[ ~a~]" nickname hopcount)

(define-simple-irc-command user (username hostname servername realname)
  "USER ~a ~a ~a :~a" username hostname servername realname)

(define-simple-irc-command server (servername hopcount info)
  "SERVER ~a ~d :~a" servername hopcount info)

(define-simple-irc-command oper (user password)
  "OPER ~a ~a" user password)

(define-simple-irc-command quit (&optional comment)
  :superclasses (deeds:blocking-event)
  "QUIT~@[ :~a~]" comment)

(define-simple-irc-command squit (server comment)
  "SQUIT ~a :~a" server comment)

(define-simple-irc-command join (channels)
  "JOIN ~{~a~^,~} ~{~a~^,~}"
  (loop for chan in (enlist channels) collect (if (listp chan) (first chan) chan))
  (loop for chan in (enlist channels) collect (if (listp chan) (second chan) "")))

(define-simple-irc-command part (channels)
  "PART ~{~a~^,~}" (enlist channels))

(define-simple-irc-command mode (target mode &key limit user ban-mask)
  "MODE ~a ~a~@[ ~a~@[ ~a~@[ ~a~]~]~]" target mode limit user ban-mask)

(define-simple-irc-command topic (channel &optional topic)
  "TOPIC ~a~@[ :~a~]" channel topic)

(define-simple-irc-command names (channels)
  "NAMES ~{~a~^,~}" (enlist channels))

(define-simple-irc-command list (channels &key server)
  "LIST~@[ ~{~a~^,~}~@[ ~a~]~]" (enlist channels) server)

(define-simple-irc-command invite (nickname channel)
  "INVITE ~a ~a" nickname channel)

(define-simple-irc-command kick (channel user &optional comment)
  "KICK ~a ~a~@[ :~a~]" channel user comment)

(define-simple-irc-command version (&key server)
  "VERSION~@[ ~a~]" server)

(define-simple-irc-command stats (&key query server)
  "STATS~@[ ~a~@[ ~a~]~]" query server)

(define-simple-irc-command links (&key remote-server server-mask)
  "LINKS~*~@[~:*~@[ ~a~] ~a~]" remote-server server-mask)

(define-simple-irc-command time (&key server)
  "TIME~@[ ~a~]" server)

(define-simple-irc-command connect (target &key port remote)
  "CONNECT ~a~@[ ~a~@[ ~a~]~]" target port remote)

(define-simple-irc-command trace (&key server)
  "TRACE~@[ ~a~]" server)

(define-simple-irc-command admin (&key server)
  "ADMIN~@[ ~a~]" server)

(define-simple-irc-command info (&key server)
  "INFO~@[ ~a~]" server)

(define-message-irc-command privmsg (receivers message)
  "PRIVMSG ~{~a~^,~} :~a" (enlist receivers) message)

(define-message-irc-command notice (nickname text)
  "NOTICE ~a ~a" nickname text)

(define-simple-irc-command who (&key name opers-only)
  "WHO~@[ ~a~@[ o~]~]" name opers-only)

(define-simple-irc-command whois (nickmasks &key server)
  "WHOIS~@[ ~a~] ~{~a~^,~}" server (enlist nickmasks))

(define-simple-irc-command whowas (nickname &key count server)
  "WHOWAS ~a~@[ ~a~@[ ~a~]~]" nickname count server)

(define-simple-irc-command kill (nickname comment)
  "KILL ~a :~a" nickname comment)

(define-simple-irc-command ping (server &optional other-server)
  "PING ~a~@[ ~a~]" server other-server)

(define-simple-irc-command pong (daemon &optional other-daemon)
  "PONG ~a~@[ ~a~]" daemon other-daemon)

(define-message-irc-command error (message)
  "ERROR :~a" message)

(define-simple-irc-command away (&optional message)
  "AWAY~@[ :~a~]" message)

(define-simple-irc-command rehash ()
  "REHASH")

(define-simple-irc-command restart ()
  "RESTART")

(define-simple-irc-command summon (user &key server)
  "SUMMON ~a~@[ ~a~]" user server)

(define-simple-irc-command users (&key server)
  "USERS~@[ ~a~]" server)

(define-simple-irc-command wallops (message)
  "WALLOPS :~a" message)

(define-simple-irc-command userhost (nicknames)
  "USERHOST~{ ~a~}" (enlist nicknames))

(define-simple-irc-command ison (nicknames)
  "ISON~{ ~a~}" (enlist nicknames))
