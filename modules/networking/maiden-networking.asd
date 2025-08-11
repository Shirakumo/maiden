(asdf:defsystem maiden-networking
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Mixin components to help with common networking tasks in Maiden."
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "conditions")
               (:file "events")
               (:file "clients")
               (:file "documentation"))
  :depends-on (:maiden
               :cl+ssl
               :usocket))
