#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem maiden-talk
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Performs text-to-speech"
  :homepage "https://github.com/Shinmera/maiden"
  :serial T
  :components ((:file "package")
               (:file "codes")
               (:file "talk")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :cl-mpg123
               :cl-out123
               :drakma))
