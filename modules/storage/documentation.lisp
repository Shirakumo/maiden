#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.storage)

;; storage.lisp
(docs:define-docs
  (variable *storages*
    "This is a hash table to store caches of the storages.")

  (function package-short-name
    "Returns the shortest nickname for the given package.")

  (function left-trim-string
    "If the STRING starts with TRIM, then trim it off.")

  (function split
    "Split the string by CHAR. Only stores nonempty substrings.")

  (function normalize-fqdn
    "Attempts to normalize the FQDN given.

1. Everything is downcased
2. Dots are replaced by slashes
3. If the result starts with org/shirakumo/, it is trimmed.
4. If the result starts with maiden, it is trimmed.
5. If the result starts with -/, it is trimmed.")

  (function package-path
    "Discover a path for where to put the storage file for the given package.")

  (function find-config-directory
    "Attempt to find a suitable configuration directory.

If there is a directory named \"config\" within MAIDEN:*ROOT*,
then that is used. Otherwise, a subdirectory named \"maiden\"
in the (UBIQUITOUS:CONFIG-DIRECTORY) is returned.")

  (functon config-pathname
    "Returns a suitable pathname to the configuration file for the given object.")

  (function storage
    "Accesses the storage object for the given thing. If the storage has not yet been loaded, NIL is returned.

See *STORAGES*")

  (function ensure-storage
    "Returns the proper storage object for the given thing. If the storage has not yet been loaded, it is restored from disk if possible.

The storage object is not always loaded from disk, and is
instead cached in memory.

See STORAGE
See RESTORE")

  (function with-storage
    "Ensure the storage is available within the body.

The designator determines for \"what\" the configuration is.
If ALWAYS-LOAD is non-NIL, then the storage is first loaded
from disk every time.

See CONFIG-PATHNAME
See ENSURE-STORAGE
See UBIQUITOUS:WITH-LOCAL-STORAGE")

  (function reload
    "Causes the configuration to be reloaded.

If DESIGNATOR is NIL, then the configuration is reloaded
for everything.

The configuration is not immediately reloaded. The reloading
is deferred until the storage in question is actually needed.

See STORAGE
See *STORAGES*")

  (function offload
    "Saves the storage for the given designator to disk.

The storage is always serialised under the MAIDEN-USER
package.

See UBIQUITOUS:OFFLOAD
See CONFIG-PATHNAME
See STORAGE")

  (function restore
    "Restores the storage for the given designator from disk.

See UBIQUITOUS:RESTORE
See CONFIG-PATHNAME")

  (function define-stored-accessor
    "Define an accessor that defers to the storage instead of the object's slot.

See WITH-STORAGE"))
