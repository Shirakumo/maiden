(in-package #:maiden-user)
(defpackage #:maiden-markov
  (:nicknames #:org.shirakumo.maiden.agents.markov)
  (:use #:cl #:maiden #:maiden-activatable #:maiden-commands #:maiden-storage #:maiden-client-entities)
  ;; generator.lisp
  (:export
   #:generator
   #:word
   #:word-index
   #:chain
   #:ensure-chain
   #:add-chain
   #:next-word-index
   #:random-token
   #:make-sentence
   #:find-sentence
   #:learn-sentence
   #:learn
   #:learn-from-file)
  ;; storage.lisp
  (:export
   #:read-generator
   #:write-generator)
  ;; interface.lisp
  (:export
   #:markov
   #:ramble
   #:ramble-about
   #:ramble-chance
   #:set-ramble-chance
   #:stats))
