#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem colleen-accounts
  :defsystem-depends-on (:colleen)
  :class "colleen:module"
  :version "3.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Account management agent for Colleen"
  :homepage "https://github.com/Shinmera/colleen"
  :serial T
  :components ((:file "package")
               (:file "documentation"))
  :depends-on ())
