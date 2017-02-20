#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.commands)

(define-condition command-condition (maiden-condition)
  ())

(define-condition lexing-error (command-condition error)
  ())

(define-condition expected-key-error (lexing-error)
  ((value :initarg :value))
  (:report (lambda (c s) (format s "Unexpected value ~s, expected a key."
                                 (slot-value c 'value)))))

(define-condition destructuring-error (command-condition error)
  ())

(define-condition not-enough-arguments-error (destructuring-error)
  ((lambda-list :initarg :lambda-list))
  (:report (lambda (c s) (format s "Not enough arguments to match ~a"
                                 (slot-value c 'lambda-list)))))

(defun peek (in)
  (peek-char T in NIL))

(defun consume (in)
  (read-char in NIL NIL))

(defun clear-whitespace (in)
  (loop for c = (consume in)
        while c
        when (char/= c #\Space)
        do (unread-char c in)
           (return)))

(defun read-delimited-token (in delim &key (escape #\\) (key #'identity) unread)
  (with-output-to-string (out)
    (flet ((emit (c) (write-char (funcall key c) out)))
      (loop with esc = NIL
            for c = (consume in)
            while c
            do (cond (esc (setf escape NIL) (emit c))
                     ((char= c escape) (setf esc T))
                     ((char= c delim) (when unread (unread-char c in)) (return))
                     (T (emit c)))))))

(defun read-token (in)
  (read-delimited-token in #\Space))

(defun read-string (in)
  (when (eql (peek in) #\")
    (consume in)
    (read-delimited-token in #\")))

(defun read-value (in)
  (clear-whitespace in)
  (or (read-string in)
      (read-token in)))

(defun normalize-opt-arg (o)
  (let ((norm (if (listp o)
                  (list (first o) (second o) (third o))
                  (list o NIL NIL))))
    (unless (symbolp (first norm))
      (error "Argument ~s is not a symbol." (first norm)))
    (unless (symbolp (third norm))
      (error "Provided-p argument ~s is not a symbol." (third norm)))
    norm))

(defun stream-end-p (stream)
  (not (peek-char T stream NIL NIL)))

(defun read-rest-arg (stream)
  (loop until (stream-end-p stream)
        collect (read-value stream)))

(defun read-string-arg (stream)
  (with-output-to-string (output)
    (loop for char = (read-char stream NIL)
          while char
          do (write-char char output))))

(defun getf* (plist item &key (test #'eql))
  (loop for (key val) on plist by #'cddr
        do (when (funcall test item key)
             (return val))))

(defun generate-lambda-list-body (lambda-list input)
  (when (and (find '&string lambda-list)
             (or (find '&rest lambda-list)
                 (find '&key lambda-list)))
    (error "Cannot use &string with &key or &rest in a lambda-list."))
  (let ((kargs)
        (karg (gensym "KARG"))
        (vars ()))
    (values (loop with mode = '&required
                  for item in lambda-list
                  for form = (cond ((find item '(&optional &key &rest &string))
                                    (setf mode item)
                                    (when (and (eql item '&key) (not kargs))
                                      (setf kargs (gensym "KARGS"))
                                      `(setf ,kargs (read-rest-arg ,input))))
                                   (T
                                    (case mode
                                      (&required
                                       (unless (symbolp item)
                                         (error "Required argument ~s is not a symbol." item))
                                       (push item vars)
                                       `(setf ,item (read-value ,input)))
                                      (&optional
                                       (destructuring-bind (symb val provided) (normalize-opt-arg item)
                                         (push symb vars)
                                         (when provided (push provided vars))
                                         `(unless (stream-end-p ,input)
                                            ,@(when provided `((setf ,provided T)))
                                            (setf ,symb (or (read-value ,input) ,val)))))
                                      (&rest
                                       (setf kargs item)
                                       (push item vars)
                                       `(setf ,item (read-rest-arg ,input)))
                                      (&string
                                       (push item vars)
                                       `(setf ,item (read-string-arg ,input)))
                                      (&key
                                       (destructuring-bind (symb val provided) (normalize-opt-arg item)
                                         (push symb vars)
                                         (when provided (push provided vars))
                                         `(let ((,karg (getf* ,kargs ,(format NIL ":~a" symb) :test #'string-equal)))
                                            (cond (,karg
                                                   ,@(when provided `((setf ,provided T)))
                                                   (setf ,symb ,karg))
                                                  (T
                                                   (setf ,symb ,val)))))))))
                  when form collect form)
            (append (unless (symbol-package kargs) (list kargs))
                    (nreverse vars)))))

(defmacro with-command-destructuring-bind (lambda-list input &body body)
  (let ((stream (gensym "STREAM")))
    (multiple-value-bind (forms vars) (generate-lambda-list-body lambda-list stream)
      `(let (,@vars)
         (with-input-from-string (,stream ,input)
           ,@forms)
         (locally ,@body)))))
