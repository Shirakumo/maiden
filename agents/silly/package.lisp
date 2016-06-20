#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-silly
  (:nicknames #:org.shirakumo.maiden.agents.silly)
  (:use #:cl #:maiden #:maiden-activatable #:maiden-commands #:maiden-api-access #:maiden-client-entities)
  ;; silly.lisp
  (:export
   #:silly))
