#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-medals
  (:nicknames #:org.shirakumo.maiden.agents.medals)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands)
  ;; medals.lisp
  (:export
   #:medals
   #:add-medals
   #:remove-medals
   #:medals))
