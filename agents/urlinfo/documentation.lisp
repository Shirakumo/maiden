#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.urlinfo)

(docs:define-docs
  (function fetch
    "Fetches the given HTTP URL.

Returns three values:
- The URL that was fetched in the end (due to redirects)
- The content-type of the resource the URL points to
- The stream to read the content body from

See DRAKMA:HTTP-REQUEST")

  (function process-until
    "Call FUNCTION on each character read from STREAM until STRING is found.

The function is only called up until, but not including the
string that defines the end.")

  (function maybe-title
    "Extract a title for the document of the given type from the stream.

This is only done if the type is known and if the document
stream does indeed contain a title. Thus it does not always
return a title even if a document potentially defines it in
some manner.")

  (function nicer-title
    "Reformat the title in a way that is perceived to be nicer.

Gets rid of superfluous spaces and newlines, and reformats
the title as <empty> should it be an empty string.")

  (function short-url
    "Return a shorter version of the URL that is capped to a maximum of URL-CUTOFF.")

  (function nicer-content-type
    "Return a more human-readable version of the given content-type string.")

  (function urlinfo
    "Return a formatted string that presents some information about the given URL.

In particular, it will include a short form of the actual URL
as resolved by the request. It will also include the content-
type of the data at the end, and the title of the document, if
such a title is available.")

  (function find-urls-in-string
    "Extract all the URLs in the string and return them in a list.")
  
  (type urlinfo
    "This agent provides an automatic URL inspection. When an URL is encountered, it is looked up, and information about it is displayed in the channel. This can be useful to preview what a link is about.")

  (command test
    "Retrieve the information about an URL."))
