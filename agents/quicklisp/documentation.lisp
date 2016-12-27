#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.quicklisp)

(docs:define-docs
  (type quicklisp
    "This agent provides access to the Quicklisp package manager.")

  (command version
    "This displays version information about a particular system, or maiden itself.")

  (command update
    "This updates all the specified systems to the latest Quicklisp versions. This does not actually reload the code, however. See 'upgrade'.")

  (command upgrade
    "This upgrades all the specified systems to the latest Quicklisp versions. This causes the code to be reloaded as well.")

  (command quickload
    "Quickload a specific system.")

  (command uninstall
    "Uninstall a system from the disk. This will not remove the code from the lisp image if the system has already been loaded.")

  (command install-dist
    "Add a new dist to the quicklisp distribution.")

  (command uninstall-dist
    "Remove a dist from the quicklisp distribution."))
