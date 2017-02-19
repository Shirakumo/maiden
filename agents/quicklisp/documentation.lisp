#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.quicklisp)

(docs:define-docs
  (function dists-and-versions
    "Return a list of lists of dist names and their current versions.")

  (function dist-for-system
    "Return the first dist that contains the system specified.")

  (function check-dists-available
    "Check whether all of the dists in the list are ones Quicklisp knows about.

If one is not, an error is signalled.")

  (function check-systems-available
    "Check whether all of the systems in the list are ones Quicklisp knows about.

If one is not, an error is signalled.")

  (function check-systems-upgradable
    "Check whether all of the systems are upgradable.

System that are not managed by Git or Quicklisp
generate an error.")

  (function update
    "Update the Quicklisp dists definitions to the latest versions.

This will not update any actual systems until they are next loaded.")

  (function upgrade
    "Upgrade the given systems to the latest versions.

This will handle systems that are managed by Git
or by Quicklisp.")
  
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
