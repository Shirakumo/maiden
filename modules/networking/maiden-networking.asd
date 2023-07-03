(asdf:defsystem maiden-networking
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Mixin components to help with common networking tasks in Maiden."
  :homepage "https://Shinmera.github.io/maiden/"
  :bug-tracker "https://github.com/Shinmera/maiden/issues"
  :source-control (:git "https://github.com/Shinmera/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "conditions")
               (:file "events")
               (:file "clients")
               (:file "documentation"))
  :depends-on (:maiden
               :cl+ssl
               :usocket))
