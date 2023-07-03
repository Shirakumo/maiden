(asdf:defsystem maiden-irc
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "IRC client for Maiden"
  :homepage "https://Shinmera.github.io/maiden/"
  :bug-tracker "https://github.com/Shinmera/maiden/issues"
  :source-control (:git "https://github.com/Shinmera/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "events")
               (:file "commands")
               (:file "client")
               (:file "users")
               (:file "documentation"))
  :depends-on (:maiden-networking
               :maiden-client-entities
               :babel
               :cl-ppcre
               :form-fiddle
               :lambda-fiddle
               :cl-base64))
