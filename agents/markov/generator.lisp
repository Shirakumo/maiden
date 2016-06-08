#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.markov)

(defclass generator ()
  ((words :initform (make-array 1 :adjustable T :fill-pointer 1 :initial-contents '(".")) :accessor words)
   (word-map :initform (make-hash-table :test 'equal) :accessor word-map)
   (chains :initform (make-hash-table :test 'equal) :accessor chains)
   (end :initform 0 :accessor end)))

(defmethod print-object ((generator generator) stream)
  (print-unreadable-object (generator stream :type T)
    (format stream "~a words, ~a chains"
            (length (words generator))
            (hash-table-count (chains generator)))))

(defun word (thing generator)
  (etypecase thing
    (number (elt (words generator) thing))
    (symbol (word (string thing) generator))
    (string (gethash (string-downcase thing) (word-map generator)))))

(defun (setf word) (thing generator)
  (etypecase thing
    (number thing)
    (symbol (setf (word generator) (string thing)))
    (string (let ((thing (string-downcase thing)))
              (setf (gethash thing (word-map generator))
                    (vector-push-extend thing (words generator)))))))

(defun word-index (generator word)
  (if (numberp word) word
      (or (word word generator)
          (setf (word generator) word))))

(defun chain (generator first second)
  (gethash (cons (word-index generator first)
                 (word-index generator second))
           (chains generator)))

(defun (setf chain) (possibilities generator first second)
  (setf (gethash (cons (word-index generator first)
                       (word-index generator second))
                 (chains generator))
        possibilities))

(defun ensure-chain (generator first second)
  (or (chain generator first second)
      (setf (chain generator first second)
            (make-array 0 :adjustable T :fill-pointer 0))))

(defun add-chain (generator first second &rest possibilities)
  (let ((chain (ensure-chain generator first second)))
    (dolist (pos possibilities generator)
      (vector-push-extend (word-index generator pos) chain))))

(defun next-word-index (generator first second)
  (let ((chain (chain generator first second)))
    (when chain
      (aref chain (random (length chain))))))

(defun random-token (generator)
  (let ((n (random (hash-table-count (chains generator)))))
    (loop for k being the hash-keys of (chains generator)
          repeat n finally (return (values (car k) (cdr k))))))

(defun make-sentence (generator)
  (with-output-to-string (out)
    (multiple-value-bind (f s) (random-token generator)
      (loop for first = f then second
            for second = s then third
            for third = (next-word-index generator first second)
            do (write-string (word first generator) out)
               (cond ((= second (end generator))
                      (write-char #\. out)
                      (return))
                     (T
                      (write-char #\Space out)))))))

(defun learn-sentence (sentence generator)
  (let ((tokens (cl-ppcre:split "[\\s,、\\-_:：；]+" (string-downcase sentence))))
    (when (cddr tokens)
      (loop for first = (pop tokens) then second
            for second = (pop tokens) then third
            for third in tokens
            do (add-chain generator first second third)
            finally (add-chain generator first second (end generator)))))
  generator)

(defun learn (string generator)
  (dolist (sentence (cl-ppcre:split "[.!?¿¡̣;:<>(){}\\[\\]\"”。！？：；]+"string))
    (learn-sentence sentence generator)))

(defun learn-from-file (file generator)
  (learn (alexandria:read-file-into-string file) generator))
