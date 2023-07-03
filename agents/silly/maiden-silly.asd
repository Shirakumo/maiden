(asdf:defsystem maiden-silly
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Makes the bot have silly responses for various messages."
  :homepage "https://Shinmera.github.io/maiden/"
  :bug-tracker "https://github.com/Shinmera/maiden/issues"
  :source-control (:git "https://github.com/Shinmera/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "silly")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :maiden-activatable
               :maiden-api-access
               :maiden-client-entities
               :lquery
               :cl-ppcre
               :alexandria))
