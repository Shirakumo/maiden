(in-package #:maiden-user)
(defpackage #:maiden-talk
  (:nicknames #:org.shirakumo.maiden.agents.talk)
  (:use #:cl #:maiden #:maiden-commands)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:harmony #:org.shirakumo.fraf.harmony.user)
   (#:v #:org.shirakumo.verbose))
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
