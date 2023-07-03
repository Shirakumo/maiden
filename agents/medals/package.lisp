(in-package #:maiden-user)
(defpackage #:maiden-medals
  (:nicknames #:org.shirakumo.maiden.agents.medals)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities)
  ;; medals.lisp
  (:export
   #:medals
   #:add-medals
   #:remove-medals
   #:medals
   #:show
   #:award
   #:take))
