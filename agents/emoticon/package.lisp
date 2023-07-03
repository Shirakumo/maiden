(in-package #:maiden-user)
(defpackage #:maiden-emoticon
  (:nicknames #:org.shirakumo.maiden.agents.emoticon)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities)
  ;; emoticon.lisp
  (:shadow #:remove #:list)
  (:export
   #:emoticon
   #:remove-emoticon
   #:list-emoticons
   #:add
   #:change
   #:remove
   #:list))
