#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.blocker)

(defvar *clauses* ())

(defun clause (name)
  (cdr (assoc name *clauses* :test #'string-equal)))

(defun (setf clause) (handler name)
  (remove-clause name)
  (push (cons (intern (string-upcase name) :keyword) handler) *clauses*))

(defun remove-clause (name)
  (setf *clauses* (remove name *clauses* :key #'car :test #'string-equal)))

(defmacro define-clause (name (ev &rest args) &body body)
  `(setf (clause ,(string name))
         (lambda (,ev ,@args)
           ,@body)))

(define-clause or (event &rest clauses)
  (loop for clause in clauses
        thereis (match-rule clause event)))

(define-clause and (event &rest clauses)
  (loop for clause in clauses
        always (match-rule clause event)))

(define-clause not (event clause)
  (not (match-rule clause event)))

(define-clause channel (event channel)
  (and (typep event 'channel-event)
       (matches (channel event) channel)))

(define-clause client (event client)
  (and (typep event 'client-event)
       (matches (client event) client)))

(define-clause user (event user)
  (and (typep event 'user-event)
       (matches (user event) user)))

(define-clause regex (event regex)
  (and (typep event 'message-event)
       (cl-ppcre:scan regex (message event))))

(define-clause prefix (event prefix)
  (and (typep event 'message-event)
       (<= (length prefix) (length (message event)))
       (string= prefix (message event) :end2 (length prefix))))

(defun match-rule (rule event)
  (apply (or (clause (first rule))
             (error "No such rule clause ~s." (first rule)))
         event
         (rest rule)))

(defun parse-clause (in)
  (when (char/= #\( (read-char in))
    (error "Invalid list."))
  (let ((first (parse-word in)))
    (list* (or (car (find first *clauses* :key #'car :test #'string-equal))
               (error "Invalid rule command: ~s" first))
           (loop for char = (peek-char T in)
                 do (when (char= char #\))
                      (read-char in)
                      (loop-finish))
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
    (#\( (parse-clause in))
    (#\" (parse-string in))
    (T (parse-word in))))

(defun parse-rule (string)
  (handler-case
      (with-input-from-string (in string)
        (parse-clause in))
    (error (err)
      (error "The rule ~s is malformed: ~a" string err))))

(defun ensure-rule (rule)
  (etypecase rule
    (string (ensure-rule (parse-rule rule)))
    (cons
     (handler-case
         (match-rule rule (make-instance 'framework-message))
       (error (err)
         (error "Malformed rule: ~a" err)))
     rule)))

(define-consumer blocker (agent)
  ((rules :initarg :rules :accessor rules)))

(defmethod initialize-instance :after ((blocker blocker) &key (rules NIL rules-p))
  (declare (ignore rules))
  (unless rules-p
    (with-storage (blocker)
      (setf (rules blocker) (or (value :rules) (make-hash-table :test 'equal))))))

(defmethod (setf rules) :after (rules (blocker blocker))
  (with-storage (blocker)
    (setf (value :rules) rules)))

(defmethod rule (name (blocker blocker))
  (gethash name (rules blocker)))

(defmethod (setf rule) (rule name (blocker blocker))
  (setf (gethash name (rules blocker)) (ensure-rule rule)))

(defun remove-rule (name blocker)
  (remhash name (rules blocker)))

(defun add-rule (name rule blocker)
  (when (gethash name (rules blocker))
    (error "A rule named ~s already exists." name))
  (setf (rule name blocker) rule))

(defun blocked-p (event blocker)
  (loop for rule being the hash-values of (rules blocker)
        thereis (match-rule rule event)))

(define-handler (blocker block-commands command-event) (c ev dispatch-event)
  :before '(:main)
  :class deeds:locally-blocking-handler
  :add-to-consumer NIL
  (when (blocked-p dispatch-event c)
    (cancel ev)
    (reply dispatch-event "I can't let you do that, ~a."
           (name (user dispatch-event)))))

(define-command (blocker add-rule) (c ev name rule)
  :command "block"
  :advice (not public)
  (add-rule name rule c)
  (reply ev "Rule ~s added." name))

(define-command (blocker block-channel) (c ev channel &key client name)
  :command "block channel"
  :advice (not public)
  (add-rule (or name channel)
            `(:and (:channel ,channel)
                   (:client ,(or client (name (client ev)))))
            c)
  (reply ev "Rule ~s added." (or name channel)))

(define-command (blocker block-user) (c ev user &key client name)
  :command "block user"
  :advice (not public)
  (add-rule c (or name user)
            `(:and (:user ,user)
                   (:client ,(or client (name (client ev))))))
  (reply ev "Rule ~s added." (or name user)))

(define-command (blocker block-regex) (c ev regex &key client name)
  :command "block regex"
  :advice (not public)
  (add-rule (or name regex)
            `(:and (:regex ,regex)
                   (:client ,(or client (name (client ev)))))
            c)
  (reply ev "Rule ~s added." (or name regex)))

(define-command (blocker block-prefix) (c ev prefix &key client name)
  :command "block prefix"
  :advice (not public)
  (add-rule (or name prefix)
            `(:and (:prefix ,prefix)
                   (:client ,(or client (name (client ev)))))
            c)
  (reply ev "Rule ~s added." (or name prefix)))

(define-command (blocker update-rule) (c ev name rule)
  :command "update block rule"
  :advice (not public)
  (unless (rule name c)
    (error "No such rule exists."))
  (setf (rule name c) (parse-rule rule))
  (reply ev "Rule ~s updated." name))

(define-command (blocker remove-rule) (c ev name)
  :command "unblock"
  :advice (not public)
  (unless (rule name c)
    (error "No such rule exists."))
  (remove-rule name c)
  (reply ev "Rule ~s removed." name))

(define-command (blocker view-rule) (c ev name)
  :command "view block rule"
  :advice (not public)
  (unless (rule name c)
    (error "No such rule exists."))
  (let ((*print-case* :downcase))
    (reply ev "~s" (gethash name (rules c)))))

(define-command (blocker view-rules) (c ev)
  :command "view block rules"
  :advice (not public)
  (reply ev "~:[No rules are defined.~;~:*The following rules are known: ~{~a~^, ~}~]"
         (loop for k being the hash-keys of (rules c) collect k)))
