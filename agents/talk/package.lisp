#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-talk
  (:nicknames #:org.shirakumo.maiden.agents.talk)
  (:use #:cl #:maiden #:maiden-commands)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed))
  ;; talk.lisp
  (:export
   #:talk
   #:speech-file
   #:stop-playing
   #:play
   #:talk
   #:talk-en
   #:talk-lang
   #:shut-up))
