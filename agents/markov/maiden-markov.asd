(asdf:defsystem maiden-markov
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Markov chains for Maiden"
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "generator")
               (:file "storage")
               (:file "interface")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :maiden-activatable
               :maiden-client-entities
               :maiden-storage
               :alexandria
               :cl-ppcre
               :fast-io
               :babel
               :parse-number))
