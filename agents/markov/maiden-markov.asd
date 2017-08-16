#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem maiden-markov
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Markov chains for Maiden"
  :homepage "https://github.com/Shinmera/maiden"
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
