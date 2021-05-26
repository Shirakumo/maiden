#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.irc)

(defvar *send-length-limit* 512)

(define-consumer irc-client (text-tcp-client reconnecting-client timeout-client simple-user-channel-client)
  ((nickname :initarg :nickname :accessor nickname :accessor maiden-client-entities:username)
   (username :initarg :username :accessor username)
   (password :initarg :password :accessor password)
   (realname :initarg :realname :accessor realname)
   (intended-nickname :initarg :intended-nickname :accessor intended-nickname)
   (services :initarg :services :accessor services)
   (services-password :initarg :services-password :accessor services-password)
   (sasl :initarg :sasl :accessor sasl))
  (:default-initargs
   :nickname "Maiden"
   :username (machine-instance)
   :password NIL
   :realname (machine-instance)
   :port 6667
   :services :generic
   :services-password NIL
   :sasl NIL))

(defmethod initialize-instance :after ((client irc-client) &key channels)
  (dolist (channel channels)
    (setf (find-channel channel client)
          (make-instance 'irc-channel :client client :name channel)))
  (unless (name client)
    (setf (name client) (host client)))
  (unless (slot-boundp client 'intended-nickname)
    (setf (intended-nickname client) (nickname client))))

(defmethod initiate-connection :after ((client irc-client))
  (with-slots (nickname username password realname sasl) client
    (when sasl (irc:cap client "REQ" :sasl (getf sasl :method "PLAIN")))
    (when password (irc:pass client password))
    (setf (nickname client) (irc:nick* client nickname))
    (irc:user client username 0 "*" realname)
    (when sasl (irc:authenticate client (getf sasl :method "PLAIN")))))

(defmethod close-connection :before ((client irc-client))
  (when (client-connected-p client)
    (ignore-errors
     (send "QUIT" client))))

(defmethod close-connection :after ((client irc-client))
  (clrhash (user-map client))
  (loop for channel being the hash-values of (channel-map client)
        do (clrhash (user-map channel))))

(defmethod handle-connection :around ((client irc-client))
  (with-simple-restart (abort "Exit the connection handling.")
    (handler-bind ((data-parse-error
                     (lambda (err)
                       (v:error :maiden.client.irc.connection "Parse error: ~a" err)
                       (invoke-restart 'continue)))
                   (unknown-data-warning
                     (lambda (err)
                       (v:warn :maiden.client.irc.connection "Parse error: ~a" err))))
      (call-next-method))))

(defmethod process (message (client irc-client))
  (let ((events (when (and message (string/= message ""))
                  (parse-reply client message))))
    (dolist (core (cores client))
      (dolist (event events)
        (issue event core)))))

(defmethod send ((list list) (client irc-client))
  (dolist (item list list)
    (send item client)))

(defmethod send ((message string) (client irc-client))
  ;; FIXME: Better handling for multilines
  ;; FIXME: Queue messages or sleep to avoid flood
  (let ((message (format NIL "~a~c~c" (cl-ppcre:regex-replace-all "\\n" message " ") #\Return #\Linefeed)))
    (when (< *send-length-limit* (length (babel:string-to-octets message :encoding (encoding client))))
      (warn 'data-too-long-warning :data message))
    (call-next-method message client))
  client)

(defmethod receive ((client irc-client))
  (with-output-to-string (out)
    ;; Slow character by character copy, but since we have to do
    ;; CRLF detection portably, we cannot use READ-LINE.
    (loop with stream = (usocket:socket-stream (socket client))
          for char = (read-char stream) 
          do (case char
               (#\Return
                (let ((next (read-char stream)))
                  (if (eql next #\Newline)
                      (return)
                      (unread-char char stream))))
               (T (write-char char out))))))

(defmethod handle-connection-idle ((client irc-client))
  (irc:ping client (host client)))

(define-handler (irc-client sender send-event) (client ev)
  :match-consumer 'client
  (send (update-message ev) client))

(define-handler (irc-client ping irc:msg-ping) (client ev server other-server)
  :match-consumer 'client
  (irc:pong client server other-server))

(defun services-logon (client)
  (when (services-password client)
    (irc:privmsg client "NickServ" (format NIL "IDENTIFY ~a" (services-password client)))))

(define-handler (irc-client nick-change irc:msg-nick) (client ev user nickname)
  :match-consumer 'client
  (when (string-equal (name user) (nickname client))
    (v:info :maiden.clients.irc.connection "Detected nick change from ~s to ~s." user nickname)
    (setf (nickname client) nickname)
    ;; If we reclaimed our proper nick, we can try re-identifying.
    (when (string= (intended-nickname client) nickname)
      (services-logon client))))

(define-handler (irc-client yank-nick irc:msg-quit) (client ev user)
  :match-consumer 'client
  (when (and (string-equal (name user) (intended-nickname client))
             (not (string-equal (name user) (nickname client))))
    (v:info :maiden.clients.irc.connection "Detected nick drop for our intended nick ~s." user)
    (irc:nick client user)))

(defun rejoin-channels (client)
  (loop with channels = ()
        for k being the hash-keys of (channel-map client)
        do (push k channels)
           (when (= 5 (length channels))
             (irc:join client channels)
             (setf channels ()))
        finally (when channels (irc:join client channels))))

(define-handler (irc-client handle-init irc:rpl-welcome) (client ev)
  :match-consumer 'client
  (when (string= (nickname client) (intended-nickname client))
    (services-logon client))
  ;; Attempt to join all channels. This might fail due to delayed +i on channels +r.
  (rejoin-channels client))

(define-handler (irc-client handle-mode irc:msg-mode) (client ev target mode)
  :match-consumer 'client
  ;; We are now identified, attempt to rejoin channels.
  (when (and (string-equal target (nickname client))
             (find #\i mode))
    (rejoin-channels client)))

(define-handler (irc-client handle-authenticate irc:msg-authenticate) (client ev message)
  :match-consumer 'client
  (when (and (string= "+" message) (sasl client))
    (destructuring-bind (&key identity username password method) (sasl client)
      (etypecase method
        ((NIL :plain)
         (irc:authenticate client (base64:base64-encode (format NIL "~@[~a~]~c~a~c~a" identity #\Nul username #\Nul password))))))))

(define-handler (irc-client handle-auth-complete irc:rpl-loggedin) (client ev)
  :match-consumer 'client
  (irc:cap client "END"))

(define-handler (irc-client version-reply irc:msg-version) (client ev)
  :match-consumer 'client
  (reply ev "maiden-irc ~a" (asdf:component-version (asdf:find-system :maiden-irc))))

(defmethod authenticate ((nick string) (client irc-client))
  (authenticate-with (services client) nick client))

(defmethod authenticate ((user user) (client irc-client))
  (authenticate (name user) client))

(defun authenticate-with (method nick client &key (timeout 2))
  (case method
    ((:acc :freenode)
     (with-awaiting (client irc:msg-notice) (ev message)
         (irc:privmsg client "NickServ" (format NIL "ACC ~a" nick))
       :filter `(and (search "ACC" message)
                     (search ,nick message :test ,#'char-equal))
       :timeout timeout
       (cl-ppcre:register-groups-bind (status-nick code) ("^([^ ]+) ACC (\\d)" message)
         (and (string= nick status-nick)
              (string= code "3")))))
    ((:status :anope :tynet :rizon)
     (with-awaiting (client irc:msg-notice) (ev message)
         (irc:privmsg client "NickServ" (format NIL "STATUS ~a" nick))
       :filter `(and (search "STATUS" message)
                     (search ,nick message :test #'char-equal))
       :timeout timeout
       (cl-ppcre:register-groups-bind (status-nick code) ("^STATUS ([^ ]+) (\\d)" message)
         (and (string= nick status-nick)
              (string= code "3")))))
    ((:r-mode)
     ;; Not sure if this is actually correct.
     (with-awaiting (client irc:mode) (ev target mode)
         (irc:mode client nick)
       :filter `(string-equal ,nick target)
       :timeout timeout
       (and (string= nick target)
            (find #\r mode))))
    (:generic
     (or (authenticate-with :acc nick client)
         (authenticate-with :status nick client)
         (authenticate-with :r-mode nick client)))
    ((NIL)
     T)))
