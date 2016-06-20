#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-emoticon
  (:nicknames #:org.shirakumo.maiden.agents.emoticon)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities)
  ;; emoticon.lisp
  (:shadow #:remove #:list)
  (:export
   #:emoticon
   #:remove-emoticon
   #:list-emoticons))
