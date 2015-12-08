#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defun checkdocs (&optional (package *package*))
  "Check that all functions, classes, and variables have docstrings."
  (do-symbols (symb package)
    (when (eql (symbol-package symb) package)
      (when (and (fboundp symb) (not (documentation symb 'function)))
        (warn "No documentation for function ~s." symb))
      (when (and (boundp symb) (not (documentation symb 'variable)))
        (warn "No documentation for variable ~s." symb))
      (when (and (find-class symb NIL) (not (documentation symb 'type)))
        (warn "No documentation for class ~s." symb)))))

(defmacro setdocs (&body pairs)
  "Easily set the documentation."
  `(progn
     ,@(loop for (var doc) in pairs
             collect (destructuring-bind (var &optional (type 'function))
                         (if (listp var) var (list var))
                       `(setf (documentation ',var ',type) ,doc)))))
