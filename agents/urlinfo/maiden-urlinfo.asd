(asdf:defsystem maiden-urlinfo
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Access information about URLs in Maiden."
  :homepage "https://Shinmera.github.io/maiden/"
  :bug-tracker "https://github.com/Shinmera/maiden/issues"
  :source-control (:git "https://github.com/Shinmera/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "urlinfo")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :maiden-activatable
               :maiden-client-entities
               :drakma
               :cl-ppcre
               :plump))
