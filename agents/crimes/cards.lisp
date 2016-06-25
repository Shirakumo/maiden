#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.crimes)

(defvar *decks* (make-hash-table :test 'equal))

(defun normalize-name (name)
  (intern
   (remove-if-not
    (lambda (c) (find c "abcdefghijklmnopqrstuvwxyz0123456789-_ " :test #'char-equal))
    (typecase name
      (deck (name name))
      (T (string-downcase name))))
   #.*package*))

(defun deck (name)
  (let ((name (normalize-name name)))
    (or (gethash name *decks*)
        (setf (gethash name *decks*) (load-deck name)))))

(defun (setf deck) (deck name)
  (let ((name (normalize-name name)))
    (setf (name deck) name)
    (setf (gethash name *decks*) deck)
    (save-deck deck)))

(defun remove-deck (name)
  (let ((deck (deck name)))
    (delete-file (maiden-storage:config-pathname (name deck)))
    (remhash (name deck) *decks*)
    deck))

(defclass deck ()
  ((name :initarg :name :accessor name)
   (calls :initarg :calls :accessor calls)
   (responses :initarg :responses :accessor responses))
  (:default-initargs
   :name (error "NAME required.")
   :calls (make-hash-table :test 'equalp)
   :responses (make-hash-table :test 'equalp)))

(defmethod print-object ((deck deck) stream)
  (print-unreadable-object (deck stream :type T)
    (format stream "~a ~a call~:p, ~a response~:p"
            (name deck) (hash-table-count (calls deck)) (hash-table-count (responses deck)))))

(defmethod list-calls ((deck deck))
  (alexandria:hash-table-values (calls deck)))

(defmethod list-responses ((deck deck))
  (alexandria:hash-table-values (responses deck)))

(defun save-deck (deck)
  (maiden-storage:with-storage ((name deck))
    (setf (maiden-storage:value :calls) (calls deck))
    (setf (maiden-storage:value :responses) (responses deck)))
  deck)

(defun load-deck (name)
  (let ((name (normalize-name name)))
    (unless (probe-file (maiden-storage:config-pathname name))
      (error "No such deck ~s found." name))
    (maiden-storage:with-storage (name)
      (make-instance 'deck :name name
                           :calls (maiden-storage:value :calls)
                           :responses (maiden-storage:value :responses)))))

(defmethod remove-card (id (deck deck))
  (remhash id (calls deck))
  (remhash id (responses deck))
  deck)

(defmethod remove-card ((card card) (deck deck))
  (remove-card (id card) deck))

(defmethod card (id (deck deck))
  (or (gethash id (calls deck))
      (gethash id (responses deck))))

(defclass card (entity)
  ((text :initarg :text :accessor text))
  (:default-initargs
   :text (error "TEXT required.")))

(defmethod print-object ((card card) stream)
  (print-unreadable-object (card stream :type T)
    (format stream "~a ~s" (id card) (text card))))

(defclass call (card)
  ())

(defmethod required-responses ((call call))
  (1- (length (text call))))


(defmethod add-call ((call call) (deck deck))
  (unless (gethash call (calls deck))
    (setf (gethash (id call) (calls deck)) call))
  call)

(defmethod add-call ((text list) (deck deck))
  (add-call (make-instance 'call :text text) deck))

(defun parse-call (string)
  (let ((parts (cl-ppcre:split "__+" string)))
    (if (cdr parts)
        parts
        (list (car parts) ""))))

(defmethod add-call ((text string) (deck deck))
  (add-call (parse-call text) deck))

(defclass response (card)
  ())

(defmethod add-response ((response response) (deck deck))
  (unless (gethash response (responses deck))
    (setf (gethash (id response) (responses deck)) response))
  response)

(defmethod add-response ((text string) (deck deck))
  (add-response (make-instance 'response :text text) deck))

(defclass result ()
  ((call :initarg :call :accessor call)
   (responses :initarg :responses :accessor responses))
  (:default-initargs
   :call (error "CALL required.")
   :responses ()))

(defmethod print-object ((result result) stream)
  (print-unreadable-object (result stream :type T)
    (format stream "~a" (text result))))

(defmethod required-responses ((result result))
  (required-responses (call result)))

(defmethod remaining-responses ((result result))
  (- (required-responses result)
     (length (responses result))))

(defmethod complete-p ((result result))
  (<= (remaining-responses result) 0))

(defmethod (setf responses) :before (resp (result result))
  (when (<= (length (text (call result)))
            (length resp))
    (error "You must submit exactly ~a responses."
           (required-responses result))))

(defmethod add-response ((response response) (result result))
  (when (complete-p result)
    (error "The result is already complete."))
  (push-to-end response (responses result)))

(defmethod text ((result result))
  (with-output-to-string (out)
    (let ((call-parts (text (call result)))
          (resp-parts (mapcar #'text (responses result))))
      (loop for call = (pop call-parts)
            for resp = (pop resp-parts)
            do (format out " ~a " (string-trim " " call))
               (write-string (or resp "___") out)
            while (cdr call-parts))
      (format out " ~a" (car call-parts)))))
