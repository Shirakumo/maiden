(in-package #:maiden-user)
(defpackage #:maiden-quicklisp
  (:nicknames #:org.shirakumo.maiden.agents.quicklisp)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities)
  (:export
   #:quicklisp
   #:update
   #:upgrade
   #:version
   #:quickload
   #:uninstall
   #:install-dist
   #:uninstall-dist))
