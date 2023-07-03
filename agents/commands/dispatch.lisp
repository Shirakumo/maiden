(in-package #:org.shirakumo.maiden.agents.commands)

(define-consumer commands (agent)
  ())

(define-handler (commands processor (and message-event passive-event)) (c ev message)
  (unless (matches (username (client ev)) (user ev))
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
                        (mapcar #'prefix (mapcar #'cdr alternatives))))))))))

(defun compare-alternatives (a b)
  (let ((a-distance (car a))
        (a-length (length (prefix (cdr a))))
        (b-distance (car b))
        (b-length (length (prefix (cdr b)))))
    (or (< a-distance b-distance)
        (and (= a-distance b-distance)
             (< b-length a-length)))))
