#|
 This file is a part of Maiden
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem maiden-relay
  :defsystem-depends-on (:maiden)
  :class "maiden:module"
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Relay client for Maiden"
  :homepage "https://github.com/Shinmera/maiden"
  :serial T
  :components ((:file "package")
               (:file "conditions")
               (:file "containers")
               (:file "events")
               (:file "virtual-client")
               (:file "relay")
               (:file "client")
               (:file "documentation"))
  :depends-on (:maiden-serialize))
