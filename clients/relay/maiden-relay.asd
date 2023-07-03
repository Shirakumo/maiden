(asdf:defsystem maiden-relay
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Relay client for Maiden"
  :homepage "https://Shinmera.github.io/maiden/"
  :bug-tracker "https://github.com/Shinmera/maiden/issues"
  :source-control (:git "https://github.com/Shinmera/maiden.git")
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
