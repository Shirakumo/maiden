(asdf:defsystem maiden-crimes
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Play Cards Against Humanity."
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "cards")
               (:file "cardcast")
               (:file "game")
               (:file "interface")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :maiden-client-entities
               :maiden-api-access
               :maiden-storage
               :alexandria
               :cl-ppcre))
