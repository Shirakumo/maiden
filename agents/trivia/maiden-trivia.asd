(asdf:defsystem maiden-trivia
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Simple trivia game for Maiden."
  :homepage "https://Shinmera.github.io/maiden/"
  :bug-tracker "https://github.com/Shinmera/maiden/issues"
  :source-control (:git "https://github.com/Shinmera/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "trivia")
               (:file "game")
               (:file "interface")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :maiden-client-entities
               :maiden-storage
               :alexandria))
