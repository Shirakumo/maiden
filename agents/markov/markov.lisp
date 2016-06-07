#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.markov)

(defvar *storage* (make-instance 'storage))

(defclass storage ()
  ((word-index :initform (make-array 1 :adjustable T :fill-pointer 1 :initial-contents '(".")) :accessor word-index)
   (word-refs :initform (make-hash-table :test 'equal) :accessor word-refs)
   (chains :initform (make-hash-table :test 'equal) :accessor chains)
   (end :initform 0 :accessor end)))

(defmethod print-object ((storage storage) stream)
  (print-unreadable-object (storage stream :type T)
    (format stream "~a words, ~a chains"
            (length (word-index storage))
            (hash-table-count (chains storage)))))

(defun word (thing storage)
  (etypecase thing
    (number (elt (word-index storage) thing))
    (symbol (word (string thing) storage))
    (string (gethash (string-downcase thing) (word-refs storage)))))

(defun (setf word) (thing storage)
  (etypecase thing
    (number thing)
    (symbol (setf (word storage) (string thing)))
    (string (let ((thing (string-downcase thing)))
              (setf (gethash thing (word-refs storage))
                    (vector-push-extend thing (word-index storage)))))))

(defun ensure-word-index (storage word)
  (if (numberp word) word
      (or (word word storage)
          (setf (word storage) word))))

(defun chain (storage first second)
  (gethash (cons (ensure-word-index storage first)
                 (ensure-word-index storage second))
           (chains storage)))

(defun (setf chain) (possibilities storage first second)
  (setf (gethash (cons (ensure-word-index storage first)
                       (ensure-word-index storage second))
                 (chains storage))
        possibilities))

(defun ensure-chain (storage first second)
  (or (chain storage first second)
      (setf (chain storage first second)
            (make-array 0 :adjustable T :fill-pointer 0))))

(defun add-chain (storage first second &rest possibilities)
  (let ((chain (ensure-chain storage first second)))
    (dolist (pos possibilities storage)
      (vector-push-extend (ensure-word-index storage pos) chain))))

(defun next-word-index (storage first second)
  (let ((chain (chain storage first second)))
    (when chain
      (aref chain (random (length chain))))))

(defun random-token (storage)
  (let ((n (random (hash-table-count (chains storage)))))
    (loop for k being the hash-keys of (chains storage)
          repeat n finally (return (values (car k) (cdr k))))))

(defun make-sentence (storage)
  (with-output-to-string (out)
    (multiple-value-bind (f s) (random-token storage)
      (loop for first = f then second
            for second = s then third
            for third = (next-word-index storage first second)
            do (write-string (word first storage) out)
               (cond ((= second (end storage))
                      (write-char #\. out)
                      (return))
                     (T
                      (write-char #\Space out)))))))

(defun learn-sentence (sentence storage)
  (let ((tokens (cl-ppcre:split "[\\s,、\\-:：；]+" (string-downcase sentence))))
    (when (cddr tokens)
      (loop for first = (pop tokens) then second
            for second = (pop tokens) then third
            for third in tokens
            do (add-chain storage first second third)
            finally (add-chain storage first second (end storage)))))
  storage)

(defun learn (string storage)
  (dolist (sentence (cl-ppcre:split "[.!?¿¡̣;:(){}\\[\\]。！？：；]+"string))
    (learn-sentence sentence storage)))

(defun read-token (in)
  (let ((next))
    (loop while (eql (setf next (read-char in NIL NIL)) #\Space))
    (when next
      (with-output-to-string (out)
        (loop do (write-char next out)
              until (eql (setf next (read-char in NIL NIL)) #\Space))))))

(defun read-words (line storage)
  (with-input-from-string (in line)
    (setf (word-index storage) (make-array 0 :adjustable T :fill-pointer 0))
    (setf (word-refs storage) (make-hash-table :test 'equal))
    (loop for token = (read-token in)
          while token do (setf (word storage) token))))

(defun read-chain (line storage)
  (with-input-from-string (in line)
    (let ((first (read-token in))
          (second (read-token in)))
      (loop for token = (read-token in)
            while token do (add-chain storage first second token)))))

(defun read-storage (file storage)
  (with-open-file (stream file :direction :input)
    (read-words (read-line stream) storage)
    (loop for line = (read-line stream NIL NIL)
          while line do (read-chain line storage))))

(defun write-words (out storage)
  (loop for word across (word-index storage)
        do (format out "~a " word)))

(defun write-chains (out storage)
  (loop for (first . second) being the hash-keys of (chains storage)
        for possibilities being the hash-values of (chains storage)
        do (format out "~%~a ~a" first second)
           (loop for pos across possibilities
                 do (format out " ~a" pos))))

(defun write-storage (out storage)
  (etypecase out
    ((or symbol stream)
     (write-words out storage)
     (write-chains out storage))
    ((or string pathname)
     (with-open-file (stream out :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
       (write-from-storage stream storage)))))

(define-consumer markov ()
  ())

(define-handler (markov handle message-event) (c ev message)
  :class activatable-handler
  (learn message *storage*))

(define-command (markov ramble) (c ev)
  (reply ev "~a" (make-sentence *storage*)))
