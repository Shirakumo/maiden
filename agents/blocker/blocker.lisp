#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.blocker)

(defun parse-command (in)
  (when (char/= #\( (read-char in))
    (error "Invalid list."))
  (let ((first (parse-word in)))
    (cons (cond ((string-equal first "or") :or)
                ((string-equal first "and") :and)
                ((string-equal first "not") :not)
                ((string-equal first "channel") :channel)
                ((string-equal first "client") :client)
                ((string-equal first "user") :user)
                (T (error "Invalid rule command: ~s" first)))
          (loop for char = (peek-char T in)
                until (char= char #\))
                collect (parse-token in)))))

(defun parse-word (in)
  (with-output-to-string (out)
    (loop for char = (read-char in NIL)
          do (case char
               (#\\ (write-char (read-char in) out))
               ((NIL #\" #\( #\) #\ ) (unread-char char in) (return))
               (T (write-char char out))))))

(defun parse-string (in)
  (when (char/= #\" (read-char in))
    (error "Invalid string."))
  (with-output-to-string (out)
    (loop for char = (read-char in)
          do (case char
               (#\\ (write-char (read-char in) out))
               (#\" (return))
               (T (write-char char out))))))

(defun parse-token (in)
  (case (peek-char T in)
    (#\( (parse-command in))
    (#\" (parse-string in))
    (T (parse-word in))))

(defun parse-rule (string)
  (handler-case
      (with-input-from-string (in string)
        (parse-command in))
    (error (err)
      (error "The rule ~s is malformed: ~a" string err))))

(defun match-rule (rule event)
  (ecase (first rule)
    (:or
     (loop for r in (cdr rule)
           thereis (match-rule r event)))
    (:and
     (loop for r in (cdr rule)
           always (match-rule r event)))
    (:not
     (not (match-rule (second rule) event)))
    (:channel
     (and (typep event 'channel-event)
          (matches (channel event) (second rule))))
    (:client
     (and (typep event 'client-event)
          (matches (client event) (second rule))))
    (:user
     (and (typep event 'user-event)
          (matches (user event) (second rule))))))

(define-consumer blocker (agent)
  ((rules :initarg :rules :accessor rules)))

(defmethod initialize-instance :after ((blocker blocker) &key (rules NIL rules-p))
  (unless rules-p
    (with-storage (blocker)
      (setf (rules blocker) (or (value :rules) (make-hash-table :test 'equal))))))

(defmethod (setf rules) :after (rules (blocker blocker))
  (with-storage (blocker)
    (setf (value :rules) rules)))

(defun blocking (event blocker)
  (loop for rule being the hash-values of (rules blocker)
        thereis (match-rule rule event)))

(defun add-rule (blocker name rule)
  (when (gethash name (rules blocker))
    (error "A rule named ~s already exists." name))
  (setf (gethash name (rules blocker))
        (etypecase rule
          (cons rule)
          (string (parse-rule rule)))))

(define-handler (blocker block-commands command-event) (c ev dispatch-event)
  :before :main
  (when (blocking dispatch-event c)
    (cancel ev)))

(define-command (blocker add-rule) (c ev name rule)
  :command "block"
  (add-rule c name rule))

(define-command (blocker block-channel) (c ev channel &key client name)
  :command "block channel"
  (add-rule c (or name channel)
            `(:and (:channel ,channel)
                   (:client ,(or client (name (client ev)))))))

(define-command (blocker block-channel) (c ev user &key client name)
  :command "block user"
  (add-rule c (or name user)
            `(:and (:user ,user)
                   (:client ,(or client (name (client ev)))))))

(define-command (blocker update-rule) (c ev name)
  :command "update block rule"
  (unless (gethash name (rules c))
    (error "No such rule exists."))
  (setf (gethash name (rules c))
        (parse-rule rule)))

(define-command (blocker remove-rule) (c ev name)
  :command "unblock"
  (unless (gethash name (rules c))
    (error "No such rule exists."))
  (remhash name (rules c)))

(define-command (blocker view-rule) (c ev name)
  :command "view block rule"
  (unless (gethash name (rules c))
    (error "No such rule exists."))
  (let ((*print-case* :downcase))
    (reply ev "~s" (gethash name (rules c)))))

(define-command (blocker view-rules) (c ev)
  :command "view block rules"
  (reply ev "The following rules are known: ~{~a~^, ~}"
         (loop for k being the hash-keys of (rules c) collect k)))
