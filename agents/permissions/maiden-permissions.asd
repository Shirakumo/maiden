#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem maiden-permissions
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Permission management for maiden"
  :homepage "https://github.com/Shinmera/maiden"
  :serial T
  :components ((:file "package")
               (:file "permissions")
               (:file "documentation"))
  :depends-on (:maiden-storage
               :maiden-commands
               :maiden-client-entities
               :documentation-utils
               :cl-ppcre))
