#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.irc)

(defvar *send-length-limit* 512)

(define-consumer irc-client (text-tcp-client reconnecting-client timeout-client user-client channel-client)
  ((nickname :initarg :nickname :accessor nickname)
   (username :initarg :username :accessor username)
   (password :initarg :password :accessor password)
   (realname :initarg :realname :accessor realname)
   (intended-nickname :initarg :intended-nickname :accessor intended-nickname)
   (services :initarg :services :accessor services)
   (channels :initform (make-hash-table :test 'equalp) :accessor channel-map)
   (users :initform (make-hash-table :test 'equalp) :accessor user-map))
  (:default-initargs
   :nickname "Maiden"
   :username (machine-instance)
   :password NIL
   :realname (machine-instance)
   :port 6667
   :services :unknown))

;; FIXME: nickserv password

(defmethod initialize-instance :after ((client irc-client) &key)
  (unless (name client)
    (setf (name client) (host client)))
  (unless (slot-boundp client 'intended-nickname)
    (setf (intended-nickname client) (nickname client))))

(defmethod initiate-connection :after ((client irc-client))
  (with-slots (nickname username password realname) client
    (when password (irc:pass client password))
    (irc:nick client nickname)
    (irc:user client username 0 "*" realname)
    ;; Rejoin.
    (loop for k being the hash-keys of (channel-map client)
          do (irc:join client k))))

(defmethod close-connection :before ((client irc-client))
  (when (client-connected-p client)
    (irc:quit client)))

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
  (send (message ev) client))

(define-handler (irc-client ping irc:msg-ping) (client ev server other-server)
  :match-consumer 'client
  (irc:pong client server other-server))

(define-handler (irc-client nick-change irc:msg-nick) (client ev user nickname)
  :match-consumer 'client
  (when (string-equal (name user) (nickname client))
    (v:info :maiden.clients.irc.connection "Detected nick change from ~s to ~s." user nickname)
    (setf (nickname client) nickname)))

(define-handler (irc-client yank-nick irc:msg-quit) (client ev user)
  :match-consumer 'client
  (when (and (string-equal (name user) (intended-nickname client))
             (not (string-equal user (nickname client))))
    (v:info :maiden.clients.irc.connection "Detected nick drop for our intended nick ~s." user)
    (irc:nick client user)))

(defmethod authenticate (nick (client irc-client))
  (case (services client)
    (:unknown
     NIL)
    (:anope
     ;; (with-response
     ;;     (irc:privmsg client "NickServ" (format NIL "STATUS ~a" nick))
     ;;     (irc:rpl-notice ev message)
     ;;     (:filter '(eql 0 (search "STATUS" message))
     ;;      :timeout 5)
     ;;   (cl-ppcre:register-groups-bind (status-nick code) ("^STATUS ([^ ]+) (\\d)" message)
     ;;     (and (string= nick status-nick)
     ;;          (string= code "3"))))
     )
    ((NIL)
     T)))
