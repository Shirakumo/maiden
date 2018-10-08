#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.talk)

(define-consumer talk (agent)
  ((server :accessor server)))

(defclass queue (harmony:segment cl-mixed:queue) ())

(defmethod start :after ((talk talk))
  (unless (slot-boundp talk 'server)
    (let* ((pipeline (make-instance 'harmony:pipeline))
           (server (make-instance 'harmony:server))
           (output (make-instance #+linux 'harmony-pulse:pulse-drain
                                  #+windows 'harmony-wasapi:wasapi-drain
                                  #+darwin 'harmony-coreaudio:coreaudio-drain
                                  #-(or linux windows darwin) (error "Platform not supported.")
                                  :program-name "Maiden Talk"
                                  :context server))
           (queue (make-instance 'queue
                                 :name 'queue :inputs 0 :context server)))
      (harmony:connect pipeline queue 0 output 0)
      (harmony:connect pipeline queue 1 output 1)
      (harmony:compile-pipeline pipeline server)
      (setf (server talk) server)))
  (harmony:start (server talk)))

(defmethod stop :before ((talk talk))
  (when (slot-boundp talk 'server)
    (harmony:stop server)))

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

(defmethod stop-playing ((talk talk) &key all)
  (let ((queue (harmony:segment 'queue (server talk))))
    (if all
        (cl-mixed:clear queue)
        (let ((current (cl-mixed:current-segment queue)))
          (when current (cl-mixed:withdraw current queue))))))

(defmethod play ((talk talk) path)
  (harmony:add (make-instance (harmony:source-type (pathname-type path))
                              :file path :context (server talk))
               (harmony:segment 'queue (server talk))))

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
  (talk c (format NIL "~a" text)))

(define-command (talk talk-lang) (c ev language &string text)
  :command "talk in"
  (talk c (format NIL "~a" text) :language language))

(define-command (talk play) (c ev &string file)
  :command "play file"
  (play c (parse-namestring file)))

(define-command (talk shut-up) (c ev &optional what)
  :command "shut up"
  :add-to-consumer NIL
  (cond ((null what)
         (stop-playing c))
        ((string-equal what "everything")
         (stop-playing c :all T))
        (T
         (reply ev "I don't know how to shut that up."))))
