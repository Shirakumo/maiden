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

(defun read-keyword (in)
  (read-delimited-token in #\Space :key #'char-downcase))

(defun read-keyval (in)
  (when (eql (peek in) #\:)
    (consume in)
    (clear-whitespace in)
    (cons (read-keyword in)
          (read-value in))))

(defgeneric lex (in)
  (:method ((in string))
    (with-input-from-string (stream in)
      (lex stream)))
  (:method ((in stream))
    (let ((in-kargs NIL)
          (kargs ())
          (args ()))
      (loop while (peek in)
            for karg = (read-keyval in)
            do (cond ((and in-kargs (not karg))
                      (error 'expected-key-error :value (read-value in)))
                     (karg
                      (setf in-kargs T)
                      (push karg kargs))
                     (T
                      (push (read-value in) args))))
      (values (nreverse args)
              (nreverse kargs)))))

(defun generate-lambda-list-bindings (lambda-list args kargs)
  (lambda-fiddle:with-destructured-lambda-list
      (:required req :optional opt :key key :rest rest) lambda-list
    (append (loop for symb in req
                  do (unless (symbolp symb) (error "Required argument ~s is not a symbol." symb))
                  collect `(,symb (or (pop ,args)
                                      (error 'not-enough-arguments-error :lambda-list ',lambda-list))))
            (loop for o in opt
                  for (symb val) = (if (listp o) o (list o NIL))
                  do (unless (symbolp symb) (error "Optional argument ~s is not a symbol." symb))
                  collect `(,symb (or (pop ,args)
                                      ,val)))
            (when rest
              (unless (symbolp rest) (error "Rest argument ~s is not a symbol." rest))
              `((,rest ,args)))
            (loop for k in key
                  for (symb val) = (if (listp k) k (list k NIL))
                  do (unless (symbolp symb) (error "Keyword argument ~s is not a symbol." symb))
                  collect `(,symb (or (and ,args (pop ,args))
                                      (cdr (assoc ,(string symb) ,kargs :test #'string-equal))
                                      ,val))))))

(defmacro with-command-destructuring-bind (lambda-list input &body body)
  (let ((args (gensym "ARGS"))
        (kargs (gensym "KARGS")))
    `(multiple-value-bind (,args ,kargs) (lex ,input)
       (declare (ignorable ,args ,kargs))
       (let* ,(generate-lambda-list-bindings lambda-list args kargs)
         ,@body))))
