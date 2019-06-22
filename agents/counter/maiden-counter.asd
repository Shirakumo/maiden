#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem maiden-counter
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Module to add counters to user messages in Maiden."
  :homepage "https://Shinmera.github.io/maiden/"
  :bug-tracker "https://github.com/Shinmera/maiden/issues"
  :source-control (:git "https://github.com/Shinmera/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "counter")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :maiden-storage
               :maiden-client-entities
               :maiden-activatable
               :cl-ppcre))
