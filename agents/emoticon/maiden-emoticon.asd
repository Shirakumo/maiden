(asdf:defsystem maiden-emoticon
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Module to add \"emoticons\" to user messages in Maiden."
  :homepage "https://Shinmera.github.io/maiden/"
  :bug-tracker "https://github.com/Shinmera/maiden/issues"
  :source-control (:git "https://github.com/Shinmera/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "emoticon")
               (:file "documentation"))
  :depends-on (:maiden-activatable
               :maiden-commands
               :maiden-storage
               :maiden-client-entities
               :cl-ppcre))
