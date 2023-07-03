(in-package #:org.shirakumo.maiden.modules.api-access)

(docs:define-docs
  (function construct-url
    "Construct an URL with GET parameters properly encoded into it.

See DRAKMA:URL-ENCODE")

  (function request
    "Perform an HTTP request.

The URL is preserved as-is, and no encoding is performed on it.
You will have to make sure each URL character is already encoded
as necessary. GET and POST parameters are however encoded as
expected for a request.

See REQUEST-AS")

  (function parse-to
    "Attempt to parse the input string to a certain type of data.

TYPE can be one of
- :STRING     Just return the input again.
- :JSON       Parse the input into a JSON object.
- :HTML :XML  Parse the input into a DOM.
- :SEXP       Parse the input as a SEXP.

See JSOWN:PARSE
See PLUMP:PARSE
See CL:READ-FROM-STRING")

  (function request-as
    "Perform an HTTP request and parse the data into the requested format.

See REQUEST
See PARSE-TO")

  (function json-v
    "Easily access a value in a JSON object as parsed by JSOWN.

See PARSE-TO"))
