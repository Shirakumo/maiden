(asdf:defsystem maiden-counter
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Module to add counters to user messages in Maiden."
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "counter")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :maiden-storage
               :maiden-client-entities
               :maiden-activatable
               :cl-ppcre))
