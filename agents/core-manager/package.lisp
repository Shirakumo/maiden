(in-package #:maiden-user)
(defpackage #:maiden-core-manager
  (:nicknames #:org.shirakumo.maiden.agents.core-manager)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities)
  ;; core-manager.lisp
  (:export
   #:core-manager
   #:start-consumer
   #:stop-consuemr
   #:remove-consumer
   #:add-consumer
   #:list-consumers
   #:stop-core
   #:reload))
