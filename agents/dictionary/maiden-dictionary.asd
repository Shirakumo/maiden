(asdf:defsystem maiden-dictionary
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Performs dictionary lookups"
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "dictionary")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :maiden-storage
               :maiden-client-entities
               :oxenfurt))
