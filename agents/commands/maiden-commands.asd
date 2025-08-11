(asdf:defsystem maiden-commands
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Command parsing and issuing module for Maiden"
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
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
