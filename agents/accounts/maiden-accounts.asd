(asdf:defsystem maiden-accounts
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Account management agent for Maiden"
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "conditions")
               (:file "account")
               (:file "fields")
               (:file "interface")
               (:file "documentation"))
  :depends-on (:maiden-storage
               :maiden-commands
               :maiden-client-entities))
