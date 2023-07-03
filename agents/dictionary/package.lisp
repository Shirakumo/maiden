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
