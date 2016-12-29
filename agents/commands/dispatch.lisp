#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.commands)

(define-consumer commands (agent)
  ())

(define-handler (commands processor (and message-event passive-event)) (c ev message)
  (let ((command (extract-command ev)))
    (when command
      (multiple-value-bind (match alternatives) (find-matching-command command)
        (cond ((not (null match))
               (handler-case
                   (handler-bind ((error #'maybe-invoke-debugger))
                     (funcall (invoker match) ev (if (= (length command) (length (prefix match)))
                                                     "" (subseq command (1+ (length (prefix match)))))))
                 (command-condition (err)
                   (reply ev "Invalid command: ~a" err))
                 (error (err)
                   (reply ev "Unexpected error: ~a" err))))
              ((null alternatives)
               (reply ev "I don't know what you mean."))
              (T
               (setf alternatives (sort alternatives #'compare-alternatives))
               (reply ev "Unknown command. Possible matches: ~10{~a~^, ~}"
                      (mapcar #'prefix (mapcar #'cdr alternatives)))))))))

(defun compare-alternatives (a b)
  (let ((a-distance (car a))
        (a-length (length (prefix (cdr a))))
        (b-distance (car b))
        (b-length (length (prefix (cdr b)))))
    (or (< a-distance b-distance)
        (and (= a-distance b-distance)
             (< b-length a-length)))))

(defun levenshtein-distance (a b)
  (cond ((= 0 (length a)) (length b))
        ((= 0 (length b)) (length a))
        (T
         (let ((v0 (make-array (1+ (length b))))
               (v1 (make-array (1+ (length b)))))
           (dotimes (i (length v0)) (setf (aref v0 i) i))
           (dotimes (i (length a) (aref v1 (length b)))
             (incf (aref v1 0))
             (dotimes (j (length b))
               (let ((cost (if (char= (char a i) (char b j)) 0 1)))
                 (setf (aref v1 (1+ j)) (min (1+ (aref v1 j))
                                             (1+ (aref v0 (1+ j)))
                                             (+ cost (aref v0 j))))))
             (dotimes (j (length v0))
               (setf (aref v0 j) (aref v1 j))))))))
