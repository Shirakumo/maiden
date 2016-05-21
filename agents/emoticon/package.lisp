#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-emoticon
  (:nicknames #:org.shirakumo.maiden.agents.emoticon)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands)
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
