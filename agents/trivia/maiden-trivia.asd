(asdf:defsystem maiden-trivia
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Simple trivia game for Maiden."
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
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
