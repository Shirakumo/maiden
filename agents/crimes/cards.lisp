#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.crimes)

(defun normalize-name (name)
  (remove-if-not
   (lambda (c) (find c "abcdefghijklmnopqrstuvwxyz0123456789-_ " :test #'char-equal))
   (etypecase name
     (string (intern name #.*package*))
     (symbol name))))

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
            (name deck) (length (calls deck)) (length (responses deck)))))

(defmethod list-calls ((deck deck))
  (alexandria:hash-table-values (calls deck)))

(defmethod list-responses ((deck deck))
  (alexandria:hash-table-values (responses deck)))

(defun save-deck (deck)
  (maiden-storage:with-storage ((name deck))
    (setf (maiden-storage:value :calls) (calls deck))
    (setf (maiden-storage:value :responses) (responses deck))))

(defun load-deck (name)
  (let ((name (normalize-name name)))
    (maiden-storage:with-storage (name)
      (make-instance 'deck :name name
                           :calls (or (maiden-storage:value :calls)
                                      (make-hash-table :test 'equalp))
                           :responses (or (maiden-storage:value :responses)
                                          (make-hash-table :test 'equalp))))))

(defclass card ()
  ((text :initarg :text :accessor text)
   (id :initarg :id :accessor id))
  (:default-initargs
   :text (error "TEXT required.")
   :id (princ-to-string (uuid:make-v4-uuid))))

(defmethod print-object ((card card) stream)
  (print-unreadable-object (card stream :type T)
    (format stream "~a ~s" (id card) (text card))))

(defclass call (card)
  ())

(defmethod required-responses ((call call))
  (1- (length (text call))))

(defclass response (card)
  ())

(defclass blank (response)
  ()
  (:default-initargs
   :text "This idiot didn't fill in his blank card."))

(defclass result ()
  ((call :initarg :call :accessor call)
   (responses :initarg :responses :accessor responses))
  (:default-initargs
   :call (error "CALL required.")
   :responses ()))

(defmethod required-responses ((result result))
  (required-responses (call result)))

(defmethod remaining-responses ((result result))
  (- (required-responses result)
     (length (responses result))))

(defmethod complete-p ((result result))
  (= 0 (remaining-responses result)))

(defmethod (setf responses) :before (resp (result result))
  (when (<= (length (text (call result)))
            (length resp))
    (error "You must submit exactly ~a responses."
           (required-responses result))))

(defmethod add-response ((response response) (result result))
  (push-to-end response (responses result)))

(defmethod text ((result result))
  (with-output-to-string (out)
    (let ((call-parts (text (call result)))
          (resp-parts (mapcar #'text (responses result))))
      (loop for call = (pop call-parts)
            for resp = (pop resp-parts)
            while call
            do (write-string call out)
               (write-string (or resp "___") out)))))
