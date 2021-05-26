#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.chatlog)

(define-consumer chatlog (agent)
  ((back-queue :initform () :accessor back-queue)))

(define-handler (chatlog message message-event) (c ev user message)
  (let ((channel (if (typep ev 'channel-event) (channel ev) user)))
    (maybe-record-message c :message channel user "~a" message)))

(define-handler (chatlog enter user-entered) (c ev user channel)
  (maybe-record-message c :enter channel user "** JOIN"))

(define-handler (chatlog leave user-left) (c ev user channel)
  (maybe-record-message c :leave channel user "** PART"))

;; FIXME
;; (define-handler (chatlog kick user-kick) (c ev user channel target)
;;   (maybe-record-message c :kick channel user "** KICKED ~a" target))

(define-handler (chatlog quit user-removed) (c ev user)
  (dolist (channel (channels user))
    (maybe-record-message c :disconnect channel user "** QUIT")))

(define-handler (chatlog name-change user-name-changed) (c ev user old-name)
  (dolist (channel (channels user))
    (maybe-record-message c :name channel old-name "** NICK ~a" (name user))))

;; (define-handler (chatlog topic channel-topic-changed) (c ev channel)
;;   (let ((user (if (typep ev 'user-event) (user ev) channel)))
;;     (maybe-record-message c :name channel user  "** TOPIC ~a" (topic channel))))

(define-command (chatlog activate) (c ev)
  :command "activate chatlog"
  :advice (not public)
  (let ((channel (cons (name (client ev)) (name (channel ev)))))
    (add-channel channel)
    (reply ev "Activated logging for ~a." channel)))

(define-command (chatlog activate-on) (c ev client channel)
  :command "activate chatlog on"
  :advice (not public)
  (let ((channel (cons client channel)))
    (add-channel channel)
    (reply ev "Activated logging for ~a." channel)))

(define-command (chatlog deactivate) (c ev)
  :command "deactivate chatlog"
  :advice (not public)
  (let ((channel (cons (name (client ev)) (name (channel ev)))))
    (del-channel channel)
    (reply ev "Deactivated logging for ~a." channel)))

(define-command (chatlog deactivate-on) (c ev client channel)
  :command "deactivate chatlog on"
  :advice (not public)
  (let ((channel (cons client channel)))
    (del-channel channel)
    (reply ev "Deactivated logging for ~a." channel)))

(define-command (chatlog initialize) (c ev &key (host "localhost") (db "chatlog") (user "chatlog") password (port 5432))
  :command "initialize chatlog"
  :advice (not public)
  (initialize-database :host host
                       :database db
                       :user user
                       :password password
                       :port (etypecase port
                               (integer port)
                               (string (parse-integer port))
                               (null 5432)))
  (reply ev "Initialised the chatlog database."))

(defun url-encode (thing &key (stream NIL) (external-format :utf-8) (allowed "-._~"))
  (flet ((%url-encode (stream)
           (loop for octet across (babel:string-to-octets thing :encoding external-format)
                 for char = (code-char octet)
                 do (cond ((or (char<= #\0 char #\9)
                               (char<= #\a char #\z)
                               (char<= #\A char #\Z)
                               (find char allowed :test #'char=))
                           (write-char char stream))
                          (T (format stream "%~2,'0x" (char-code char)))))))
    (if stream
        (%url-encode stream)
        (with-output-to-string (stream)
          (%url-encode stream)))))

(define-command (chatlog url) (client ev)
  :command "show log"
  (let ((channel (channel-designator ev)))
    (with-db ()
      (if (channel-exists-p channel)
          (reply ev "~a/~a/~a" (maiden-storage:value :base-url) (url-encode (car channel)) (url-encode (cdr channel)))
          (reply ev "This channel is not logged.")))))
