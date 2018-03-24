#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-dictionary
  (:nicknames #:org.shirakumo.maiden.agents.dictionary)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities)
  ;; dictionary.lisp
  (:export
   #:dictionary
   #:synonyms
   #:antonyms
   #:definitions
   #:pronunciations
   #:etymologies
   #:description))
