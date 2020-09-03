#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.talk)

(define-consumer talk (agent)
  ((playing :initform NIL :accessor playing)))

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
  (setf (playing talk) T)
  (mixed:with-objects ((source (mixed:make-unpacker))
                       (drain (mixed:make-packer))
                       (mp3 (make-instance 'org.shirakumo.fraf.mixed.mpg123:source :file path :pack source))
                       (out (make-instance #+linux 'org.shirakumo.fraf.mixed.pulse:drain
                                           #+windows 'org.shirakumo.fraf.mixed:wasapi:drain
                                           #+darwin 'org.shirakumo.fraf.mixed.coreaudio:drain
                                           :pack drain)))
    (mixed:with-buffers 500 (l r)
      (mixed:connect source :left drain :left l)
      (mixed:connect source :right drain :right r)
      (mixed:with-chain chain (mp3 source drain out)
        (loop while (playing talk)
              until (mixed:done-p mp3)
              do (mixed:mix chain))))))

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
  (setf (playing c) NIL))
