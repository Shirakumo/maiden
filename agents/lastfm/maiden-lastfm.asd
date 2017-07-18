#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem maiden-lastfm
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A last.fm interface for Maiden."
  :homepage "https://github.com/Shinmera/maiden"
  :serial T
  :components ((:file "package")
               (:file "interface")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :maiden-api-access
               :maiden-client-entities
               :maiden-storage
               :bordeaux-threads))
