#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-storage
  (:nicknames #:org.shirakumo.maiden.modules.storage)
  (:use #:cl #:maiden)
  (:export
   #:config-pathname
   #:storage
   #:with-storage))

;; re-export
(let ((symbs '(ubiquitous:value
               ubiquitous:remvalue
               ubiquitous:defaulted-value)))
  (import symbs '#:maiden-storage)
  (export symbs '#:maiden-storage))
