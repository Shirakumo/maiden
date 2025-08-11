(asdf:defsystem maiden-lookup
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An agent providing spec lookup functionality"
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "lookup")
               (:file "parsers")
               (:file "archives")
               (:file "clhs")
               (:file "mop")
               (:file "shirakumo")
               (:file "interface")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :maiden-api-access
               :maiden-client-entities
               :cl-ppcre
               :lquery
               :drakma))
