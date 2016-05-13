#|
 This file is a part of Maiden
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.circ)

(defvar *nothing* (gensym "NOTHING"))
(defvar *core* (make-instance 'core))

(defun format-name (stream arg &rest extra)
  (declare (ignore extra))
  (cond ((< 10 (length arg))
         (write-string arg stream :end 9)
         (write-char #\~ stream))
        (T
         (format stream "~10a" arg))))

(defun starts-with (start string)
  (and (<= (length start) (length string))
       (string= start string :end2 (length start))))

(defun status (message &rest format-args)
  (format T "~&  * ~?~%" message format-args))

(defun record (user message)
  (format T "~&<~/circ::format-name/> ~a~%" user message))

(define-consumer circ (agent)
  ((window :initform NIL :accessor window)
   (windows :initform () :accessor windows)
   (nicks :initform () :accessor nicks)))

(defun circ ()
  (consumer 'circ *core*))

(defun server ()
  (car (window (circ))))

(defun channel ()
  (cdr (window (circ))))

(defun nick (server)
  (or (cdr (assoc (name server) (nicks (circ))))
      "?"))

(defun init (&key relay remote)
  (start *core*)
  (start (add-consumer (make-instance 'circ) *core*))
  (cond (relay (apply #'init-relay (when (listp relay) relay)))
        (remote (apply #'init-remote (when (listp remote) remote)))))

(defun init-relay (&optional (host "127.0.0.1") (port 9486))
  (add-consumer (start (make-instance 'maiden-relay:relay :name 'relay :host host :port port)) *core*)
  (maiden-relay:subscribe *core* 'connect T)
  (maiden-relay:subscribe *core* 'disconnect T))

(defun init-remote (&optional (host "127.0.0.1") (port 9486))
  (add-consumer (start (make-instance 'maiden-relay:relay :name 'remote :host NIL)) *core*)
  (maiden-relay:subscribe *core* 'maiden-irc:reply-event T)
  (maiden-relay:subscribe *core* 'connection-event T)
  (maiden-relay:connect *core* :host host :port port))

(define-command (circ connect) (circ ev name host nickname &key
                                               (port 6667)
                                               (username (machine-instance))
                                               (realname (machine-instance)))
  (push (cons name nickname) (nicks circ))
  (unless (or (consumer 'remote *core*)
              (consumer name *core*))
    (start (add-consumer (make-instance 'maiden-irc:client
                                        :name name
                                        :host host :port port
                                        :nickname nickname
                                        :username username
                                        :realname realname) *core*))))

(define-command (circ disconnect) (circ ev name)
  (unless (or (consumer 'remote *core*)
              (not (consumer name *core*)))
    (remove-consumer (stop (consumer name *core*)) *core*)))

(define-handler (circ connection-initiated connection-initiated) (circ ev client)
  (push (cons client NIL) (windows (circ)))
  (w NIL client))

(define-handler (circ connection-closed connection-closed) (circ ev client)
  (setf (windows (circ)) (remove client (windows (circ)) :key #'car))
  (setf (nicks (circ)) (remove (name client) (nicks (circ)) :key #'car))
  (when (eql (server) client)
    (let ((window (first (windows (circ)))))
      (w (cdr window) (car window)))))

(define-handler (circ privmsg irc:msg-privmsg) (circ ev sender receivers message)
  (when (or (find (channel) receivers :test #'string-equal)
            (equalp sender (channel)))
    ;; Cheap, but eh.
    (if (starts-with "ACTION " message)
        (status "~a~a" sender (subseq message (length "ACTION") (1- (length message))))
        (record sender message))))

(define-handler (circ join irc:msg-join) (circ ev client sender channels)
  (when (and (eql (server) client)
             (find (channel) channels :test #'equalp))
    (status "~a joined ~{~a~^, ~}" sender channels)))

(define-handler (circ part irc:msg-part) (circ ev client sender channels)
  (when (and (eql (server) client)
             (find (channel) channels :test #'equalp))
    (status "~a parted ~{~a~^, ~}" sender channels)))

(define-handler (circ quit irc:msg-quit) (circ ev client sender comment)
  (when (eql (server) client)
    (status "~a quit (~a)" sender comment)))

(define-handler (circ nick irc:msg-nick) (circ ev client sender nickname)
  (when (eql (server) client)
    (status "~a changed nick to ~a" sender nickname)))

(define-handler (circ kick irc:msg-kick) (circ ev client sender channel user comment)
  (when (and (eql (server) client)
             (equalp (channel) channel))
    (status "~a kicked ~a (~a)" sender user comment)))

(define-handler (circ topic irc:msg-topic) (circ ev client channel topic)
  (when (and (eql (server) client)
             (equalp (channel) channel))
    (status "Topic: ~a" topic)))

(define-handler (circ mode irc:msg-mode) (circ ev client sender target mode)
  (when (eql (server) client)
    (status "~a set mode ~a on ~a" sender mode target)))

(define-handler (circ notice irc:msg-notice) (circ ev client sender message)
  (when (eql (server) client)
    (status "~a notice: ~a" sender message)))

(define-handler (circ nick irc:msg-nick) (circ ev client sender nickname)
  (when (string-equal (nick client) sender)
    (status "Changing nick ~a -> ~a" sender nickname)
    (setf (cdr (assoc (name client) (nicks circ))) nickname)))

(defun p (&rest channels)
  (status "Parting ~{~a~^, ~}" channels)
  (irc:part (server) channels)
  (setf (windows (circ))
        (loop for window in (windows (circ))
              unless (and (eql (car window) (server))
                          (find (cdr window) channels :test #'equalp))
              collect window))
  *nothing*)

(defun j (&rest channels)
  (status "Joining ~{~a~^, ~}" channels)
  (irc:join (server) channels)
  (setf (cdr (window (circ))) (first channels))
  (dolist (channel channels)
    (pushnew (cons (server) channel) (windows (circ)) :test #'equalp))
  *nothing*)

(defun w (channel &optional (server (server)))
  (let ((server (consumer server *core*)))
    (cond ((find (cons server channel) (windows (circ)) :test #'equalp)
           (status "Changing window to ~a/~a" (name server) channel)
           (setf (window (circ)) (cons server channel)))
          (T
           (status "No such window."))))
  *nothing*)

(defun lw ()
  (cond ((windows (circ))
         (status "Windows:")
         (loop for (server . channel) in (windows (circ))
               when channel
               do (format T "~&      ~a ~a" (name server) channel)))
        (T
         (status "No windows open right now.")))
  *nothing*)

(defun n (new-nick)
  (irc:nick (server) new-nick)
  *nothing*)

(defun r (message)
  ;; We have to be careful here. There's three ways to get the nick.
  ;; 
  ;; First by explicit method call. This is "dangerous" in the general
  ;; case since the client must not necessarily be an actual irc-client
  ;; instance. As such, we need to define a wrapper first if we want
  ;; to make this plausible while taking relays into account. Either way
  ;; it is still not a great idea, since the client may be yet something
  ;; else that we cannot anticipate. Either way, the approach is listed
  ;; for completeness:
  ;; (maiden-relay:define-virtual-client-method maiden-irc::nickname
  ;;     ((client maiden-relay:virtual-client)))
  ;; (maiden-irc:nickname (server))
  ;; 
  ;; Second by slot-value access. This is automatically solved for
  ;; virtual-clients by the relay for us, so it should be "safe" in that
  ;; case by default, but again it is not great to assume a type of the
  ;; client. Again, for completeness:
  ;; (slot-value (server) 'maiden-irc:nickname)
  ;;
  ;; Third by recording the nick ourselves via events. We choose this
  ;; approach here, as it is the only sure-fire one.
  (record (nick (server)) message)
  (irc:privmsg (server) (channel) message)
  *nothing*)

;; Prevent SWANK from printing the *NOTHING* symbol.
(when (and (find-package :swank)
           (find-package :swank-repl))
  (labels ((symb (name package)
             (find-symbol (string name) package))
           (maybe-repl-print (values)
             (unless (eq (first values) *nothing*)
               (funcall (symb :present-repl-results :swank) values))))
    (setf (symbol-value (symb :*send-repl-results-function* :swank-repl)) #'maybe-repl-print)))
