(asdf:defsystem maiden-relay
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Relay client for Maiden"
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "conditions")
               (:file "containers")
               (:file "events")
               (:file "virtual-client")
               (:file "relay")
               (:file "client")
               (:file "documentation"))
  :depends-on (:maiden-serialize
               :maiden-networking))
