#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.talk)

(define-consumer talk (agent)
  ((device :initarg :device :accessor device)
   (output :initform NIL :accessor output))
  (:default-initargs
   :device #+linux "pulse"
           #-linux NIL))

(defmethod start :after ((talk talk))
  (setf (output talk) (cl-out123:connect (cl-out123:make-output (device talk) :name "Maiden Talk")))
  #+linux (cl-out123:start (output talk) :rate 24000 :channels 1 :encoding :int16)
  #-linux (cl-out123:start (output talk)))

(defmethod stop :before ((talk talk))
  (when (output talk)
    (cl-out123:stop (output talk))
    (cl-out123:disconnect (output talk))
    (setf (output talk) NIL)))

(defun get-speech-stream (text language)
  (multiple-value-bind (stream code)
      (drakma:http-request "http://translate.google.com/translate_tts"
                           :parameters `(("ie" . "UTF-8")
                                         ("client" . "tw-ob")
                                         ("tl" . ,(language-code language))
                                         ("q" . ,text))
                           :external-format-out :utf-8
                           :external-format-in :utf-8
                           :want-stream T)
    (if (/= 200 code)
        (error "Failed to translate into speech. This failure is most likely due to an invalid language.")
        stream)))

(defun call-with-speech-file (function text language)
  (uiop:with-temporary-file (:stream out :pathname path :prefix "maiden-talk")
    (let ((in (get-speech-stream text language)))
      (uiop:copy-stream-to-stream in out :element-type '(unsigned-byte 8))
      (close in))
    (close out)
    (funcall function path)))

(defmacro with-speech-file ((path text &key (language "en-US")) &body body)
  `(call-with-speech-file (lambda (,path) ,@body) ,text ,language))

(defmacro with-output ((out device &rest args) &body body)
  `(let ((,out (cl-out123:connect (cl-out123:make-output ,device ,@args))))
     (unwind-protect
          (progn
            #+linux (cl-out123:start (output talk) :rate 24000 :channels 1 :encoding :int16)
            #-linux (cl-out123:start (output talk))
            ,@body)
       (cl-out123:stop ,out)
       (cl-out123:disconnect ,out))))

(defun play-file (file &key output)
  (if output
      (let ((file (cl-mpg123:connect (cl-mpg123:make-file file :accepted-format (list (cl-out123:rate output)
                                                                                      (cl-out123:channels output)
                                                                                      (cl-out123:encoding output))))))
        (unwind-protect
             (loop with buffer = (cl-mpg123:buffer file)
                   for read = (cl-mpg123:process file)
                   do (cl-out123:play output buffer read)
                   while (< 0 read))
          (cl-mpg123:disconnect file)))
      (with-output (out #+linux "pulse"
                        #-linux NIL)
        (play-file file :output out))))

(defun split-word-boundary (text max)
  (let ((boundary (loop with space = 0
                        for i downfrom (1- max) to 0
                        do (case (char text i)
                             ((#\. #\: #\? #\! #\！ #\？ #\。)
                              (return (1+ i)))
                             ((#\Space #\Tab #\　)
                              (setf space (max space i))))
                        finally (return space))))
    (if (< 0 boundary)
        (subseq text 0 boundary)
        (subseq text 0 (min max (length text))))))

(defun talk (text &key (language "en-US") output)
  (cond ((<= (length text) 200)
         (with-speech-file (path text :language language)
           (play-file path :output output)))
        (T
         (let ((sub (split-word-boundary text 200)))
           (talk sub :language language :output output)
           (talk (subseq text (length sub)) :language language :output output)))))

(define-command (talk talk-en) (c ev &rest text)
  :command "talk"
  (bt:with-lock-held ((lock c))
    (talk (format NIL "~{~a~^ ~}" text)
          :output (output c))))

(define-command (talk talk-lang) (c ev language &rest text)
  :command "talk in"
  (bt:with-lock-held ((lock c))
    (talk (format NIL "~{~a~^ ~}" text)
          :language language
          :output (output c))))
