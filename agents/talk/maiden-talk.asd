(asdf:defsystem maiden-talk
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Performs text-to-speech"
  :homepage "https://Shinmera.github.io/maiden/"
  :bug-tracker "https://github.com/Shinmera/maiden/issues"
  :source-control (:git "https://github.com/Shinmera/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "codes")
               (:file "talk")
               (:file "documentation"))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:maiden-commands
               :array-utils
               :drakma
               :cl-mixed-mpg123
               :harmony))
