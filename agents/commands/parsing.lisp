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

(define-condition too-many-arguments-error (destructuring-error)
  ((lambda-list :initarg :lambda-list))
  (:report (lambda (c s) (format s "There were too many arguments to match ~a"
                                 (slot-value c 'lambda-list)))))

(define-condition unknown-keyword-argument-error (destructuring-error)
  ((lambda-list :initarg :lambda-list)
   (offending-kargs :initarg :offending-kargs))
  (:report (lambda (c s) (format s "The keyword argument~p ~{~a~^, ~} are not valid for ~a"
                                 (length (slot-value c 'offending-kargs))
                                 (slot-value c 'offending-kargs)
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
      (loop for c = (consume in)
            while c
            do (cond ((char= c escape) (emit (or (consume in) (error "Unexpected end of token."))))
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

(defun read-kargs-arg (stream)
  (loop until (stream-end-p stream)
        for key = (read-value stream)
        for val = (read-value stream)
        do (unless (char= #\: (char key 0))
             (error 'expected-key-error :value key))
        collect (string-upcase key) collect val))

(defun read-string-arg (stream)
  (with-output-to-string (output)
    (loop for char = (read-char stream NIL)
          while char
          do (write-char char output))))

(defun getf* (plist item &key (test #'eql))
  (loop for (key val) on plist by #'cddr
        do (when (funcall test item key)
             (return val))))

(defun %remf* (plist item &key (test #'eql))
  ;; Pop front
  (loop while (funcall test (car plist) item)
        do (setf plist (cddr plist)))
  ;; Remove inner cones
  (loop with cons = plist
        while cons
        do (if (funcall test (caddr cons) item)
               (setf (cddr cons) (cddddr cons))
               (setf cons (cddr cons))))
  plist)

(defmacro remf* (plist item &key (test '#'eql))
  `(setf ,plist (%remf* ,plist ,item :test ,test)))

(defun check-lambda-list (lambda-list)
  (when (and (find '&allow-other-keys lambda-list)
             (not (find '&key lambda-list)))
    (error "Cannot use &allow-other-keys without &key in a lambda-list."))
  (tagbody required
     (case (pop lambda-list)
       (&optional (go optional))
       (&string (go string))
       (&rest (go rest))
       (&key (go key))
       (&allow-other-keys (error "&allow-other-keys at invalid position in lambda-list."))
       ((NIL) (go end)))
     (go required)
   optional
     (case (pop lambda-list)
       (&optional (error "Already specified &optional once."))
       (&string (go string))
       (&rest (go rest))
       (&key (go key))
       (&allow-other-keys (error "&allow-other-keys at invalid position in lambda-list."))
       ((NIL) (go end)))
     (go optional)
   string
     (case (pop lambda-list)
       ((NIL) (error "&string requires a name.")))
     (go end)
   rest
     (case (pop lambda-list)
       ((NIL) (error "&rest requires a name.")))
     (case (pop lambda-list)
       (&optional (error "&optional cannot come after &rest."))
       (&string (error "&rest and &string cannot appear together."))
       (&rest (error "Already specified &rest once."))
       (&key (go key))
       (&allow-other-keys (error "&allow-other-keys at invalid position in lambda-list.")))
     (go end)
   key
     (case (pop lambda-list)
       (&optional (error "&optional cannot come after &key."))
       (&string (error "&string cannot come after &key."))
       (&rest (error "&rest cannot come after &key."))
       (&key (error "Already specified &key once."))
       (&allow-other-keys (go allow-other-keys))
       ((NIL) (go end)))
     (go key)
   allow-other-keys
     (go end)
   end
     (when lambda-list (error "No further arguments are allowed."))))

(defun check-for-unknown-kargs (rest lambda-list)
  (let* ((kargs (lambda-fiddle:key-lambda-vars lambda-list))
         (unknown (loop for (key val) on rest by #'cddr
                        unless (find key kargs :test (lambda (a b) (string-equal a b :start1 1)))
                        collect key)))
    (when unknown (error 'unknown-keyword-argument-error :lambda-list lambda-list :offending-kargs unknown))))

(defun generate-lambda-list-body (lambda-list input)
  (check-lambda-list lambda-list)
  (let ((kargs)
        (karg (gensym "KARG"))
        (vars ())
        (end-test `(unless (stream-end-p ,input)
                     (error 'too-many-arguments-error :lambda-list ',lambda-list))))
    (values (append (loop with mode = '&required
                          for item in lambda-list
                          for form = (cond ((find item '(&optional &key &rest &string &allow-other-keys))
                                            (setf mode item)
                                            (case item
                                              (&rest
                                               (setf end-test '()))
                                              (&key
                                               (prog1 (unless kargs
                                                        (setf kargs (gensym "KARGS"))
                                                        `(setf ,kargs (read-kargs-arg ,input)))
                                                 (setf end-test `(check-for-unknown-kargs ,kargs ',lambda-list))))
                                              (&allow-other-keys
                                               `(setf ,kargs ()))))
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
                                                 `(cond ((stream-end-p ,input)
                                                         (setf ,symb ,val))
                                                        (T
                                                         ,@(when provided `((setf ,provided T)))
                                                         (setf ,symb (read-value ,input))))))
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
                                                 `(let ((,karg (getf* ,kargs ,(format NIL ":~a" symb) :test #'string=)))
                                                    (cond (,karg
                                                           (remf* ,kargs ,(format NIL ":~a" symb) :test #'string=)
                                                           ,@(when provided `((setf ,provided T)))
                                                           (setf ,symb ,karg))
                                                          (T
                                                           (setf ,symb ,val)))))))))
                          when form collect form)
                    (list end-test))
            (append (unless (symbol-package kargs) (list kargs))
                    (nreverse vars)))))

(defmacro with-command-destructuring-bind (lambda-list input &body body)
  (let ((stream (gensym "STREAM")))
    (multiple-value-bind (forms vars) (generate-lambda-list-body lambda-list stream)
      `(let (,@vars)
         (with-input-from-string (,stream ,input)
           ,@forms)
         (locally ,@body)))))

