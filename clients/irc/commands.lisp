#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.irc)

(define-event send-event (irc-event)
  ((message :initarg :message :reader message)))

(define-handler (sender send-event) (ev)
  (send-connection (colleen:client ev) (message ev)))

(deeds:define-command connect (ev name &key
                                  (nick "Colleen")
                                  (username (machine-instance))
                                  (realname (machine-instance))
                                  (host "irc.freenode.net")
                                  (port 6667)
                                  password
                                  (encoding :utf-8)
                                  (services :unknown))
  :loop *event-loop*
  (initiate-connection
   (make-instance
    'client
    :name name
    :nickname nick
    :username username
    :realname realname
    :host host
    :port port
    :password password
    :encoding encoding
    :services services)))

(deeds:define-command disconnect (ev name)
  :loop *event-loop*
  (close-connection name))

(defmacro define-irc-command (name args &body options-and-body)
  (labels ((lambda-keyword-p (a) (find a lambda-list-keywords))
           (make-req-field (a)
             (destructuring-bind (name &rest kargs) (ensure-list a)
               `(,name :initarg ,(kw name) :initform (error ,(format NIL "~a required." name)) ,@kargs)))
           (make-opt-field (a)
             (destructuring-bind (name &optional value &rest kargs) (ensure-list a)
               `(,name :initarg ,(kw name) :initform ,value ,@kargs)))
           (make-opt-arg (a)
             (destructuring-bind (name &optional value &rest kargs) (ensure-list a)
               (declare (ignore kargs))
               `(,name ,value))))
    (let ((name (intern (string name) '#:org.shirakumo.colleen.clients.irc.events))
          (pure-args (mapcar #'unlist (remove-if #'lambda-keyword-p args))))
      (lambda-fiddle:with-destructured-lambda-list (:required required :optional optional :rest rest :key key) args
        (multiple-value-bind (options body) (deeds::parse-into-kargs-and-body options-and-body)
          (destructuring-bind (&key superclasses (loop '*event-loop*)) options
            `(progn
               (deeds:define-event ,name (deeds:command-event send-event ,@superclasses)
                 (,@(mapcar #'make-req-field required)
                  ,@(mapcar #'make-opt-field optional)
                  ,@(when rest (list (make-req-field rest)))
                  ,@(mapcar #'make-opt-field key)))
               (defun ,name (client
                             ,@(mapcar #'unlist required)
                             ,@(when optional `(&optional ,@(mapcar #'make-opt-arg optional)))
                             ,@(when rest `(&rest ,rest))
                             ,@(when key `(&key ,@(mapcar #'make-opt-arg key))))
                 (deeds:do-issue ,name 
                   :loop ,loop :client client
                   ,@(loop for var in pure-args collect (kw var) collect var)))
               (defmethod message ((ev ,name))
                 (deeds:with-fuzzy-slot-bindings ,pure-args (ev ,name)
                   (format NIL ,@body))))))))))

(define-irc-command pass (password)
  "PASS ~a" password)

(define-irc-command nick (nickname &key hopcount)
  "NICK ~a~@[ ~a~]" nickname hopcount)

(define-irc-command user (username hostname servername realname)
  "USER ~a ~a ~a :~a" username hostname servername realname)

(define-irc-command server (servername hopcount info)
  "SERVER ~a ~d :~a" servername hopcount info)

(define-irc-command oper (user password)
  "OPERA ~a ~a" user password)

(define-irc-command quit (&optional comment)
  :superclasses (deeds:blocking-event)
  "QUIT~@[ :~a~]" comment)

(define-irc-command squit (server comment)
  "SQUIT ~a :~a" server comment)

(define-irc-command join (&rest channels)
  "JOIN ~{~a~^,~} ~{~a~^,~}"
  (loop for chan in channels collect (if (listp chan) (first chan) chan))
  (loop for chan in channels collect (if (listp chan) (second chan) "")))

(define-irc-command part (&rest channels)
  "PART ~{~a~^,~}" channels)

(define-irc-command mode (target mode &key limit user ban-mask)
  "MODE ~a ~a~@[ ~a~@[ ~a~@[ ~a~]~]~]" target mode limit user ban-mask)

(define-irc-command topic (channel &optional topic)
  "TOPIC ~a~@[ :~a~]" channel topic)

(define-irc-command names (&rest channels)
  "NAMES ~{~a~^,~}" channels)

(define-irc-command list (channels &key server)
  "LIST~@[ ~{~a~^,~}~@[ ~a~]~]" (ensure-list channels) server)

(define-irc-command invite (nickname channel)
  "INVITE ~a ~a" nickname channel)

(define-irc-command kick (channel user &optional comment)
  "KICK ~a ~a~@[ :~a~]" channel user comment)

(define-irc-command version (&key server)
  "VERSION~@[ ~a~]" server)

(define-irc-command stats (&key query server)
  "STATS~@[ ~a~@[ ~a~]~]" query server)

(define-irc-command links (&key remote-server server-mask)
  "LINKS~*~@[~:*~@[ ~a~] ~a~]" remote-server server-mask)

(define-irc-command time (&key server)
  "TIME~@[ ~a~]" server)

(define-irc-command connect (target &key port remote)
  "CONNECT ~a~@[ ~a~@[ ~a~]~]" target port remote)

(define-irc-command trace (&key server)
  "TRACE~@[ ~a~]" server)

(define-irc-command admin (&key server)
  "ADMIN~@[ ~a~]" server)

(define-irc-command info (&key server)
  "INFO~@[ ~a~]" server)

(define-irc-command privmsg (receivers message)
  "PRIVMSG ~{~a~^,~} :~a" (ensure-list receivers) message)

(define-irc-command notice (nickname text)
  "NOTICE ~a ~a" nickname text)

(define-irc-command who (&key name opers-only)
  "WHO~@[ ~a~@[ o~]~]" name opers-only)

(define-irc-command whois (nickmasks &key server)
  "WHOIS~@[ ~a~] ~{~a~^,~}" server (ensure-list nickmasks))

(define-irc-command whowas (nickname &key count server)
  "WHOWAS ~a~@[ ~a~@[ ~a~]~]" nickname count server)

(define-irc-command kill (nickname comment)
  "KILL ~a :~a" nickname comment)

(define-irc-command ping (server &optional other-server)
  "PING ~a~@[ ~a~]" server other-server)

(define-irc-command pong (daemon &optional other-daemon)
  "PONG ~a~@[ ~a~]" daemon other-daemon)

(define-irc-command error (message)
  "ERROR :~a" message)

(define-irc-command away (&optional message)
  "AWAY~@[ :~a~]" message)

(define-irc-command rehash ()
  "REHASH")

(define-irc-command restart ()
  "RESTART")

(define-irc-command summon (user &key server)
  "SUMMON ~a~@[ ~a~]" user server)

(define-irc-command users (&key server)
  "USERS~@[ ~a~]" server)

(define-irc-command wallops (message)
  "WALLOPS :~a" message)

(define-irc-command userhost (&rest nicknames)
  "USERHOST~{ ~a~}" nicknames)

(define-irc-command ison (&rest nicknames)
  "ISON~{ ~a~}" nicknames)
