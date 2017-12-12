#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.emoticon)

(defun emoticon (name)
  (with-storage ('emoticons)
    (value (string-downcase name))))

(defun (setf emoticon) (value name)
  (with-storage ('emoticons)
    (setf (value (string-downcase name)) value)))

(defun remove-emoticon (name)
  (with-storage ('emoticons)
    (remvalue (string-downcase name))))

(defun list-emoticons ()
  (with-storage ('emoticons)
    (sort (loop for k being the hash-keys of (value)
                collect k) #'string<)))

(define-consumer emoticon (agent)
  ((maximum :initarg :maximum :initform 5 :accessor maximum)))

(define-command (emoticon add) (c ev name &string emoticon)
  :command "add emoticon"
  (when (emoticon name)
    (error "An emoticon named :~a: already exists. Use 'change emoticon' or remove it first." name))
  (when (string=  "" name)
    (error "The name can't be empty."))
  (setf (emoticon name) emoticon)
  (reply ev "Emoticon :~a: added." name))

(define-command (emoticon change) (c ev name &string emoticon)
  :command "change emoticon"
  (setf (emoticon name) emoticon)
  (reply ev "Emoticon :~a: changed." name))

(define-command (emoticon remove) (c ev name)
  :command "remove emoticon"
  (remove-emoticon name)
  (reply ev "Emoticon :~a: removed." name))

(define-command (emoticon list) (c ev)
  :command "list emoticons"
  (let ((emoticons (list-emoticons)))
    (if emoticons
        (reply ev "~{:~a:~^ ~}" (list-emoticons))
        (reply ev "No emoticons have been defined yet."))))

(define-handler (emoticon respond (and passive-event message-event)) (c ev message)
  :class maiden-activatable:activatable-handler
  :module #.*package*
  (unless (matches (username (client ev)) (user ev))
    (let ((counter 0))
      (cl-ppcre:do-register-groups (name) (":(.*?):" message)
        (let ((emoticon (emoticon name)))
          (when emoticon
            (when (= (incf counter) (maximum c))
              (return))
            (reply ev "~a" emoticon)))))))
