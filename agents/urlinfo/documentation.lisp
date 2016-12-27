#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.urlinfo)

(docs:define-docs
  (type urlinfo
    "This agent provides an automatic URL inspection. When an URL is encountered, it is looked up, and information about it is displayed in the channel. This can be useful to preview what a link is about.")

  (function test
    "Retrieve the information about an URL."))
