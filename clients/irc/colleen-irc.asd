#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem colleen-irc
  :defsystem-depends-on (:colleen)
  :class "colleen:module"
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "IRC client for Colleen"
  :homepage "https://github.com/Shinmera/colleen"
  :serial T
  :components ((:file "package")
               (:file "events")
               (:file "commands")
               (:file "client")
               (:file "documentation"))
  :depends-on (:usocket
               :babel
               :cl-ppcre))
