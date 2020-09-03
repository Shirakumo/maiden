#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem maiden-talk
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
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
               (:feature :linux :cl-mixed-pulse)
               (:feature :windows :cl-mixed-wasapi)
               (:feature :darwin :cl-mixed-coreaudio)))
