#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.markov)

(define-consumer markov ()
  ((storage :initform NIL)
   (save-counter :initform 0 :accessor save-counter)
   (save-frequency :initform 20 :accessor save-frequency)
   (file :initform (asdf:system-relative-pathname :maiden-markov "markov.dat") :accessor file)))

(defmethod generator ((markov markov))
  (or (slot-value markov 'generator)
      (setf (slot-value markov 'generator)
            (read-generator (file markov)))))

(defun maybe-save (markov)
  (incf (save-counter markov))
  (when (<= (save-frequency markov) (save-counter markov))
    (setf (save-counter markov) 0)
    (write-generator (generator markov) (file markov))))

(define-handler (markov handle (or message-event passive-event)) (c ev message)
  :class activatable-handler
  (learn message (generator c))
  (maybe-save c))

(define-command (markov ramble) (c ev)
  (reply ev "~a" (make-sentence (generator c))))
