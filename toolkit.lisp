#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden)

(defvar *root*
  #+asdf (asdf:system-source-directory :maiden)
  #-asdf (make-pathname :name NIL
                        :type NIL
                        :version NIL
                        :defaults #.(or *compile-file-pathname* *load-pathname*)))
(defvar *debugger* (find-package :swank))

(defun maybe-invoke-debugger (condition &optional (restart 'abort) &rest values)
  (cond (*debugger*
         (with-simple-restart (continue "Don't handle ~a." condition)
           (invoke-debugger condition)))
        (restart
         (apply #'invoke-restart restart values))))

(defun update-root-for-image ()
  (let ((argv0 (uiop:argv0)))
    (setf *root* (if argv0 (pathname argv0) (user-homedir-pathname)))))

(uiop:register-image-restore-hook #'update-root-for-image)

(defun xor (a b)
  (or (and a (not b))
      (and (not a) b)))

(defun xnor (a b)
  (and (or (not a) b)
       (or a (not b))))

(defun kw (name)
  (intern (string name) :keyword))

(defun enlist (thing &rest extra-elements)
  (if (listp thing) thing (list* thing extra-elements)))

(defun unlist (thing &key (key #'first))
  (if (consp thing) (funcall key thing) thing))

(defun starts-with (start sequence &key (test #'eql))
  (and (<= (length start) (length sequence))
       (every test start sequence)))

(defmacro with-default-encoding ((&optional (encoding :UTF-8)) &body body)
  `(let (#+sbcl (sb-impl::*default-external-format* ,encoding)
         #+ccl (ccl:*default-external-format* ,encoding))
     ,@body))

(defun make-updated-list (thing list key test)
  (loop with found = NIL
        with thing-key = (funcall key thing)
        for item in list
        collect (cond ((funcall test (funcall key item) thing-key)
                       (setf found T)
                       thing)
                      (T item)) into new-list
        finally (return (if found new-list (cons thing new-list)))))

(defmacro update-list (thing list &key (key '#'identity) (test '#'eql))
  `(setf ,list (make-updated-list ,thing ,list ,key ,test)))

(defmacro with-retry-restart ((restart format-string &rest format-args) &body body)
  (let ((tag (gensym "TAG")) (stream (gensym "STREAM")))
    `(block ,tag
       (tagbody
          ,tag (restart-case (return-from ,tag ,@body)
                 (,restart ()
                   :report (lambda (,stream)
                             (format ,stream ,format-string ,@format-args))
                   (go ,tag)))))))

(defmacro do-issue (core event-type &rest initargs)
  `(deeds:do-issue ,event-type :loop ,core ,@initargs))

(defun broadcast (cores event-type &rest initargs)
  (apply #'deeds:broadcast event-type :loop cores initargs))

(defmacro named-lambda (name args &body body)
  `(flet ((,name ,args ,@body))
     #',name))

;; FIXME: Maybe put this all somewhere else?

(defvar *unix-epoch-difference* (encode-universal-time 0 0 0 1 1 1970 0))
(defun universal-to-unix (universal)
  (- universal *unix-epoch-difference*))

(defun unix-to-universal (unix)
  (+ unix *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix (get-universal-time)))

(defun format-relative-time (seconds)
  (if (= seconds 0)
      (format NIL "0 seconds")
      (let ((seconds   (mod (floor (/ seconds 1)) 60))
            (minutes   (mod (floor (/ seconds 60)) 60))
            (hours     (mod (floor (/ seconds 60 60)) 24))
            (days      (mod (floor (/ seconds 60 60 24)) 7))
            ;; We approximate by saying each month has four weeks
            (weeks     (mod (floor (/ seconds 60 60 24 7)) 4))
            (months    (mod (floor (/ seconds 60 60 24 7 4)) 12))
            ;; More accurate through seconds in a year
            (years     (mod (floor (/ seconds 31557600)) 10))
            (decades   (mod (floor (/ seconds 31557600 10)) 10))
            (centuries (mod (floor (/ seconds 31557600 10 10)) (expt 10 (- 9 2))))
            (aeons          (floor (/ seconds 31557600 10 10 (expt 10 (- 9 2)))))
            (non-NIL ()))
        (flet ((p (i format) (when (< 0 i) (push (format NIL format i) non-NIL))))
          (p seconds "~a second~:p")
          (p minutes "~a minute~:p")
          (p hours "~a hour~:p")
          (p days "~a day~:p")
          (p weeks "~a week~:p")
          (p months "~a month~:p")
          (p years "~a year~:p")
          (p decades "~a decade~:p")
          (p centuries "~a centur~:@p")
          (p aeons "~a æon~:p")
          (format NIL "~{~a~^, ~}" non-NIL)))))

(defun format-absolute-time (time)
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time time 0)
    (format NIL "~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d" yy mm dd h m s)))

(defun format-time (time &optional (relative-time-threshold (* 60 60 24)))
  (let ((now (get-universal-time)))
    (cond ((and (< (- now relative-time-threshold) time) (<= time now))
           (format NIL "~a ago" (format-relative-time (- now time))))
          ((and (< time (+ now relative-time-threshold)) (< now time))
           (format NIL "in ~a" (format-relative-time (- time now))))
          (T
           (format NIL "at ~a" (format-absolute-time time))))))

(defun find-consumer-in-package (package)
  (let ((package (find-package package)))
    (loop for symbol being the symbols of package
          for class = (find-class symbol NIL)
          do (when (and class (c2mop:subclassp class (find-class 'consumer)))
               (return symbol)))))
