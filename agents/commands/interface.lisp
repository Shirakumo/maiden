#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.commands)

(defvar *commands* ())

(define-consumer commands (agent)
  ())

;; FIXME: The command event needs to link-back to the initiating event
;;        in order to make the RESPOND/REPLY stuff actually work as they
;;        should grip on the initiating event.
(defmacro define-command ((consumer name) args &body body)
  (let ((fun-kargs (loop for arg in (cddr (lambda-fiddle:extract-lambda-vars args))
                         collect (kw arg) collect arg)))
    (form-fiddle:with-body-options (body options command) body
      `(progn (define-instruction (,consumer ,name) ,args
                :superclasses ()
                ,@options
                ,@body)
              (maiden::update-list
               (list ,(string-downcase (or command (string name)))
                     (lambda (event message)
                       (with-command-destructuring-bind ,(cddr args) message
                         (issue (make-instance ',name ,@fun-kargs)
                                (event-loop event)))))
               *commands* :key #'first :test #'string-equal)))))

(define-handler (commands processor message-event) (c ev message)
  (let ((match NIL)
        (alternatives ()))
    (loop for command in *commands*
          for prefix = (first command)
          for cut = (subseq message 0 (length prefix))
          for distance = (levenshtein-distance prefix cut)
          do (when (and (= 0 distance)
                        (or (null match) (< (length (first match))
                                            (length prefix))))
               (setf match command))
             (when (< distance 10)
               (push (cons distance command) alternatives)))
    (cond ((not (null match))
           (handler-case
               (handler-bind ((error #'invoke-debugger))
                 (funcall (second match) ev (subseq message (1+ (length (first match))))))
             (command-condition (err)
               (reply ev "Invalid command: ~a" err))
             (error (err)
               (reply ev "Unexpected error: ~a" err))))
          ((null alternatives)
           (reply ev "I don't know what you mean."))
          (T
           (setf alternatives (sort alternatives #'compare-alternatives))
           (reply ev "Unknown command. Possible matches:~{ ~a~}"
                  (mapcar #'second (subseq alternatives 0 10)))))))


(defun compare-alternatives (a b)
  (let ((a-distance (first a))
        (a-length (length (second a)))
        (b-distance (first b))
        (b-length (length (second b))))
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

;; test case
(define-command (commands echo) (c ev &rest args)
  (reply ev "ECHO: ~{~a~^ ~}" args))
(defvar *core* (start (make-instance 'core)))
(add-consumer (make-instance 'commands) *core*)
(define-event repl-msg (message-event) ()
  (:default-initargs :sender :repl :client :repl))
(defmethod reply ((msg repl-msg) f &rest a)
  (v:info :test "REPLY: ~?" f a))
(do-issue repl-msg :loop *core* :message "echo hi")
