#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.counter)

(defun counter (name)
  (with-storage ('counters)
    (value (string-downcase name))))

(defun (setf counter) (value name)
  (with-storage ('counters)
    (setf (value (string-downcase name)) value)))

(defun remove-counter (name)
  (with-storage ('counters)
    (remvalue (string-downcase name))))

(defun list-counters ()
  (with-storage ('counters)
    (loop for v being the hash-values of (value) collect v)))

(defun set-counter (name match &key response count)
  (setf (counter name) `(:name ,name
                         :match ,match
                         :response ,(or response (format NIL "~a counter: ~~a" name))
                         :count ,(or count 0))))

(define-consumer counter (agent)
  ())

(define-command (counter add) (c ev name match &optional response)
  :command "add counter"
  (when (counter name)
    (error "An counter named ~a already exists. Use 'change counter' or remove it first." name))
  (set-counter name match :response response)
  (reply ev "Counter ~a added." name))

(define-command (counter change) (c ev name &key match response)
  :command "change counter"
  (let ((counter (counter name)))
    (unless counter (error "A counter named ~a does not exist." name))
    (when match (setf (getf counter :match) match))
    (when response (setf (getf counter :response) response)))
  (reply ev "Counter ~a changed." name))

(define-command (counter remove) (c ev name)
  :command "remove counter"
  (remove-counter name)
  (reply ev "Counter ~a removed." name))

(define-command (counter list) (c ev)
  :command "list counters"
  (reply ev "~{~a~^, ~}" (loop for c in (list-counters) collect (getf c :name))))

(define-handler (counter respond message-event) (c ev message)
  :class activatable-handler
  (dolist (c (list-counters))
    (when (cl-ppcre:scan (getf c :match) (string-downcase message))
      (reply ev (getf c :response) (incf (getf c :count))))))
