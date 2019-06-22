#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#



(pushnew :deeds-no-startup *features*)

(asdf:defsystem maiden
  :version "3.1.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A modern and extensible chat bot framework."
  :homepage "https://Shinmera.github.io/maiden/"
  :bug-tracker "https://github.com/Shinmera/maiden/issues"
  :source-control (:git "https://github.com/Shinmera/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "conditions")
               (:file "event")
               (:file "standard-events")
               (:file "entity")
               (:file "consumer")
               (:file "core")
               (:file "agent")
               (:file "client")
               (:file "documentation"))
  :depends-on (:deeds
               :verbose
               :trivial-garbage
               :bordeaux-threads
               :closer-mop
               :uuid
               :form-fiddle
               :lambda-fiddle
               :documentation-utils
               :trivial-indent))
