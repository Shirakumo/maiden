#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-talk
  (:nicknames #:org.shirakumo.maiden.agents.talk)
  (:use #:cl #:maiden #:maiden-commands)
  ;; talk.lisp
  (:export
   #:talk
   #:get-speech-stream
   #:with-speech-file
   #:with-output
   #:play-file
   #:talk
   #:talk-en
   #:talk-lang))
