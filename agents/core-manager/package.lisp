#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
