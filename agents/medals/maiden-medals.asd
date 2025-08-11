(asdf:defsystem maiden-medals
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Allows you to award \"medals\" to users in Maiden"
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "medals")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :maiden-storage
               :maiden-accounts
               :maiden-client-entities
               :cl-ppcre))
