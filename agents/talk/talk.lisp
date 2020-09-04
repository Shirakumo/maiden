#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.talk)

(define-consumer talk (agent)
  ((server :initform NIL :accessor server)
   (voice :initform NIL :accessor voice)))

(defmethod start :after ((talk talk))
  (setf (server talk) (harmony:start (harmony:make-simple-server :latency 0.1))))

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

(defun speech-file (text language)
  (let ((path (merge-pathnames (format NIL "maiden-talk-~d-~d.mp3" (get-universal-time) (random 1000))
                               (uiop:temporary-directory))))
    (with-open-file (out path :if-exists :supersede
                              :direction :output
                              :element-type '(unsigned-byte 8))
      (let ((in (get-speech-stream text language)))
        (uiop:copy-stream-to-stream in out :element-type '(unsigned-byte 8))
        (close in)))
    path))

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

(defmethod play ((talk talk) path)
  (when (voice talk)
    (loop until (mixed:done-p (voice talk))
          do (sleep 0.1)))
  (setf (voice talk) (harmony:play path :server (server talk) :mixer :speech)))

(defmethod talk ((talk talk) text &key (language "en-US") output)
  (cond ((<= (length text) 200)
         (play talk (speech-file text language)))
        (T
         (let ((sub (split-word-boundary text 200)))
           (talk talk sub :language language :output output)
           (talk talk (subseq text (length sub)) :language language :output output)))))

(define-command (talk talk-en) (c ev &string text)
  :command "talk"
  (v:info :test "test")
  (talk c text))

(define-command (talk talk-lang) (c ev language &string text)
  :command "talk in"
  (talk c text :language language))

(define-command (talk play) (c ev &string file)
  :command "play file"
  (play c (uiop:parse-native-namestring file)))

(define-command (talk shut-up) (c ev)
  :command "shut up"
  :add-to-consumer NIL
  (when (voice c)
    (setf (mixed:done-p (voice c)) T)))
