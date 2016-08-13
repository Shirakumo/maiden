#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.markov)

(define-consumer markov (agent)
  ((generator :initform NIL)
   (save-counter :initform 0 :accessor save-counter)
   (storage :initform (make-hash-table :test 'equal) :accessor storage)))

(defmacro with-storage ((c) &body body)
  `(ubiquitous:with-transaction (:storage (storage ,c)
                                 :designator 'markov)
     ,@body))

(defun file (c)
  (with-storage (c)
    (ubiquitous:defaulted-value (maiden-storage:config-pathname 'dictionary) :dictionary)))

(defun save-frequency (c)
  (with-storage (c)
    (ubiquitous:defaulted-value 20 :save-frequency)))

(defun ramble-chance (c)
  (with-storage (c)
    (ubiquitous:defaulted-value 1 :ramble-chance)))

(defun (setf ramble-chance) (val c)
  (assert (<= 0.0 val 100.0) () "The chance must be in [0, 100].")
  (with-storage (c)
    (setf (ubiquitous:value :ramble-chance) val)))

(defmethod generator ((markov markov))
  (or (slot-value markov 'generator)
      (setf (slot-value markov 'generator)
            (read-generator (file markov)))))

(defun maybe-save (markov &key force)
  (incf (save-counter markov))
  (when (or force (<= (save-frequency markov) (save-counter markov)))
    (setf (save-counter markov) 0)
    (ensure-directories-exist (file markov))
    (write-generator (generator markov) (file markov))))

(define-handler (markov handle (and message-event passive-event)) (c ev message)
  :class activatable-handler
  (learn message (generator c))
  (maybe-save c)
  (when (< (random 100.0) (ramble-chance c))
    (let ((topic (find-topic message (generator c))))
      (reply ev "~a" (or (find-sentence (generator c) topic)
                         (make-sentence (generator c)))))))

(define-command (markov ramble) (c ev &optional topic)
  :command "ramble"
  (reply ev "~a" (if topic
                     (find-sentence (generator c) topic)
                     (make-sentence (generator c)))))

(define-command (markov ramble-chance) (c ev &optional new-value)
  :command "ramble chance"
  (cond (new-value
         (setf (ramble-chance c) (parse-number:parse-real-number new-value))
         (reply ev "The rambling chance has been set to ~a." new-value))
        (T
         (reply ev "The current chance of replying to a message is ~a." (ramble-chance c)))))
