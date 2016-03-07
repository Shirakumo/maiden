(ql:quickload '(colleen-irc))

(defpackage #:circ
  (:use #:cl #:colleen))
(in-package #:circ)

(defvar *nothing* (gensym "NOTHING"))
(defvar *core* (make-instance 'core :name 'core))

(define-consumer circ (agent)
  ((window :initform NIL :accessor window)
   (windows :initform NIL :accessor windows)))

(defun init ()
  (start *core*)
  (start (add-consumer (make-instance 'circ) *core*)))

(defun connect (conn &rest initargs)
  (let ((client (apply #'make-instance 'colleen-irc:client :name conn initargs)))
    (add-consumer client *core*)
    (start client)
    (setf (window (circ)) (cons client NIL))
    (push (cons client NIL) (windows (circ)))
    client))

(defun disconnect (conn)
  (let ((client (consumer conn *core*)))
    (assert (not (null client)))
    (stop (consumer conn *core*))
    (remove-consumer conn *core*)
    (setf (windows (circ)) (remove client (windows (circ)) :key #'car))
    client))

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

(defun circ ()
  (consumer 'circ *core*))

(defun server ()
  (car (window (circ))))

(defun channel ()
  (cdr (window (circ))))

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

(defun r (message)
  (record (colleen-irc:nickname (server)) message)
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
