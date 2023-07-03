(in-package #:org.shirakumo.maiden.clients.irc)

(defvar *message-length-limit* 360)

(define-event send-event (irc-event outgoing-event active-event)
  ())

(defmacro define-irc-command (name args &body options-and-body)
  (let ((name (intern (string name) '#:org.shirakumo.maiden.clients.irc.events))
        (pure-args (lambda-fiddle:extract-lambda-vars args))
        (client (gensym "CLIENT")))
    (form-fiddle:with-body-options (body options superclasses) options-and-body
      `(progn
         (define-event ,name (,@superclasses instruction-event send-event)
           ,(maiden::slot-args->slots (rest args))
           ,@options)
         (defun ,name (,client ,@(maiden::slot-args->args (rest args)))
           (do-issue (first (cores ,client)) ,name
             :client ,client ,@(loop for var in (rest pure-args) collect (kw var) collect var)))
         (defmethod update-message ((,(first args) ,name))
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

(define-event send-message-event (send-event message-event)
  ()
  (:default-initargs :user NIL))

(defmethod print-object ((event send-message-event) stream)
  (print-unreadable-object (event stream :type T :identity T)
    (format stream "~s ~a" :user (name (user event)))))

(defmethod initialize-instance :around ((ev send-message-event) &rest args &key client)
  (apply #'call-next-method ev :user (ensure-user (nickname client) client) args))

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

(defun split-message-considering-newlines (length message &key (max-backtrack 10))
  (loop for line in (cl-ppcre:split " *(\\n|\\r)+ *" message)
        nconc (split-message-smartly length line :max-backtrack max-backtrack)))

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
    (form-fiddle:with-body-options (body options superclasses) options-and-body
      `(define-irc-command ,name (,ev ,@args)
         :superclasses ,(list* 'send-message-event superclasses)
         ,@options
         (deeds:with-fuzzy-slot-bindings ,pure-args (,ev ,name)
           (mapcar
            (lambda (,message) (format NIL ,@body))
            (split-message-considering-newlines *message-length-limit* ,message)))))))

(define-simple-irc-command cap (command &key sasl)
  "CAP ~a~@[ :sasl~]" command sasl)

(define-simple-irc-command authenticate (arg)
  "AUTHENTICATE ~a" arg)

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

(define-simple-irc-command mode (target &key mode limit user ban-mask)
  "MODE ~a~@[ ~a~@[ ~a~@[ ~a~@[ ~a~]~]~]~]" target mode limit user ban-mask)

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
  :superclasses (channel-event)
  "PRIVMSG ~{~a~^,~} :~a" (enlist receivers) message)

(defmethod initialize-instance :around ((ev irc:privmsg) &rest args &key receivers client)
  (apply #'call-next-method ev :channel (coerce-irc-object (first (enlist receivers)) NIL NIL client) args))

(define-message-irc-command notice (nickname message)
  "NOTICE ~a :~a" nickname message)

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

(defun irc:nick* (client nickname &key (max-attempts 5) (timeout 2))
  (let ((attempts 0))
    (tagbody
     try-nick
       (with-awaiting (client irc:err-nicknameinuse) (ev)
           (irc:nick client nickname)
         :timeout timeout
         (v:warn :maiden.client.irc "Failed to change nick to ~a, retrying with ~:*~a_." nickname)
         (setf nickname (format NIL "~a_" nickname))
         (incf attempts)
         (when (<= attempts max-attempts)
           (go try-nick))))
    (when (<= attempts max-attempts)
      nickname)))
