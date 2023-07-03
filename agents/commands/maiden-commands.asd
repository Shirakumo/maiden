(asdf:defsystem maiden-commands
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Command parsing and issuing module for Maiden"
  :homepage "https://Shinmera.github.io/maiden/"
  :bug-tracker "https://github.com/Shinmera/maiden/issues"
  :source-control (:git "https://github.com/Shinmera/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "parsing")
               (:file "extraction")
               (:file "invocation")
               (:file "dispatch")
               (:file "documentation"))
  :depends-on (:lambda-fiddle
               :maiden
               :maiden-client-entities))
