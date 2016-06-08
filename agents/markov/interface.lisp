#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.markov)

(define-consumer markov ()
  ((storage)))

(defmethod storage ((markov markov))
  (or (slot-value markov 'generator)
      (setf (slot-value markov 'generator)
            (read-generator (asdf:system-relative-pathname :maiden-markov "markov.dat")))))

(define-handler (markov handle message-event) (c ev message)
  :class activatable-handler
  (learn message (generator c)))

(define-command (markov ramble) (c ev)
  (reply ev "~a" (make-sentence (generator c))))
