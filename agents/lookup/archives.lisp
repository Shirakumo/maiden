#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.lookup)

(define-webpage-lookup clhs (term)
  (format NIL "http://l1sp.org/cl/~a" (drakma:url-encode term :utf-8)))

(macrolet ((define-l1sp-lookups (&rest lookups)
             `(progn ,@(loop for lookup in lookups
                             collect `(define-webpage-lookup ,lookup (term)
                                        (format NIL "http://l1sp.org/~a/~a"
                                                ,(string-downcase lookup) (drakma:url-encode term :utf-8)))))))
  (define-l1sp-lookups asdf ccl cffi clim clisp clx mop pcl sbcl))

#+()
(defun generate-table-from-staple-page (url)
  (lquery:$ (initialize (drakma:http-request url))
    "#symbol-index article"
    (each (lambda (a)
            (cl-ppcre:register-groups-bind (b c d) ("([^ ]+ ([^:]+:(.+)))" (lquery:$1 a (attr :id)))
              (format T "~&((~{~s~^ ~}) ~s~%~s)"
                      (list b c d)
                      (format NIL "~a~a" url (lquery:$1 a ".name a" (attr :href)))
                      (format NIL "~@(~a~)~a" (char b 0) (subseq b 1))))
            T)))
  NIL)

(define-table-lookup lichat
  (("1" "wire format") "https://github.com/Shirakumo/lichat-protocol#1-wire-format")
  (("1.1" "symbols") "https://github.com/Shirakumo/lichat-protocol#11-symbols")
  (("1.2" "objects") "https://github.com/Shirakumo/lichat-protocol#12-objects")
  (("1.3" "null characters") "https://github.com/Shirakumo/lichat-protocol#13-null-characters")
  (("2" "server objects") "https://github.com/Shirakumo/lichat-protocol#2-server-objects")
  (("2.1" "connection") "https://github.com/Shirakumo/lichat-protocol#21-connection")
  (("2.2" "user") "https://github.com/Shirakumo/lichat-protocol#22-user")
  (("2.2.1" "user name constraints") "https://github.com/Shirakumo/lichat-protocol#221-user-name-constraints")
  (("2.3" "profile") "https://github.com/Shirakumo/lichat-protocol#23-profile")
  (("2.4" "channel") "https://github.com/Shirakumo/lichat-protocol#24-channel")
  (("2.4.1" "primary channels") "https://github.com/Shirakumo/lichat-protocol#241-primary-channels")
  (("2.4.2" "anonymous channels") "https://github.com/Shirakumo/lichat-protocol#242-anonymous-channels")
  (("2.4.3" "regular channels") "https://github.com/Shirakumo/lichat-protocol#243-regular-channels")
  (("2.4.4" "channel name constraints") "https://github.com/Shirakumo/lichat-protocol#244-channel-name-constraints")
  (("2.5" "permission rules") "https://github.com/Shirakumo/lichat-protocol#25-permission-rules")
  (("3" "general interaction") "https://github.com/Shirakumo/lichat-protocol#3-general-interaction")
  (("3.1" "null termination of updates") "https://github.com/Shirakumo/lichat-protocol#31-null-termination-of-updates")
  (("4" "connection") "https://github.com/Shirakumo/lichat-protocol#4-connection")
  (("4.1" "connection establishment") "https://github.com/Shirakumo/lichat-protocol#41-connection-establishment")
  (("4.2" "connection maintenance") "https://github.com/Shirakumo/lichat-protocol#42-connection-maintenance")
  (("4.3" "connection closure") "https://github.com/Shirakumo/lichat-protocol#43-connection-closure")
  (("5" "client interaction") "https://github.com/Shirakumo/lichat-protocol#5-client-interaction")
  (("5.1" "general update checks") "https://github.com/Shirakumo/lichat-protocol#51-general-update-checks")
  (("5.2" "profile registration") "https://github.com/Shirakumo/lichat-protocol#52-profile-registration")
  (("5.3" "channel creation & management") "https://github.com/Shirakumo/lichat-protocol#53-channel-creation--management")
  (("5.4" "channel interaction") "https://github.com/Shirakumo/lichat-protocol#54-channel-interaction")
  (("5.4.1" "joining a channel") "https://github.com/Shirakumo/lichat-protocol#541-joining-a-channel")
  (("5.4.2" "leaving a channel") "https://github.com/Shirakumo/lichat-protocol#542-leaving-a-channel")
  (("5.4.3" "pulling a user") "https://github.com/Shirakumo/lichat-protocol#543-pulling-a-user")
  (("5.4.4" "kicking a user") "https://github.com/Shirakumo/lichat-protocol#544-kicking-a-user")
  (("5.4.5" "sending a message") "https://github.com/Shirakumo/lichat-protocol#545-sending-a-message")
  (("5.5" "server information retrieval") "https://github.com/Shirakumo/lichat-protocol#55-server-information-retrieval")
  (("5.5.1" "listing public channels") "https://github.com/Shirakumo/lichat-protocol#551-listing-public-channels")
  (("5.5.2" "listing all users of a channel") "https://github.com/Shirakumo/lichat-protocol#552-listing-all-users-of-a-channel")
  (("5.5.3" "requesting information about a user") "https://github.com/Shirakumo/lichat-protocol#553-requesting-information-about-a-user")
  (("6" "protocol extension") "https://github.com/Shirakumo/lichat-protocol#6-protocol-extension")
  ("*DEFAULT-ANONYMOUS-CHANNEL-PERMISSIONS*" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:*DEFAULT-ANONYMOUS-CHANNEL-PERMISSIONS*")
  ("*DEFAULT-CHANNEL-LIFETIME*" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:*DEFAULT-CHANNEL-LIFETIME*")
  ("*DEFAULT-PRIMARY-CHANNEL-PERMISSIONS*" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:*DEFAULT-PRIMARY-CHANNEL-PERMISSIONS*")
  ("*DEFAULT-PROFILE-LIFETIME*" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:*DEFAULT-PROFILE-LIFETIME*")
  ("*DEFAULT-REGULAR-CHANNEL-PERMISSIONS*" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:*DEFAULT-REGULAR-CHANNEL-PERMISSIONS*")
  ("*ID-COUNTER*" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:*ID-COUNTER*")
  ("ALREADY-IN-CHANNEL" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:ALREADY-IN-CHANNEL")
  ("BAD-NAME" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:BAD-NAME")
  ("CHANNEL" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CHANNEL")
  ("CHANNEL-UPDATE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CHANNEL-UPDATE")
  ("CHANNELNAME-TAKEN" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CHANNELNAME-TAKEN")
  ("CHANNELS" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CHANNELS")
  ("CONNECT" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CONNECT")
  ("CONNECTION" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CONNECTION")
  ("CONNECTION-UNSTABLE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CONNECTION-UNSTABLE")
  ("CREATE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CREATE")
  ("DISCONNECT" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:DISCONNECT")
  ("FAILURE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:FAILURE")
  ("INCOMPATIBLE-VERSION" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:INCOMPATIBLE-VERSION")
  ("INSUFFICIENT-PERMISSIONS" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:INSUFFICIENT-PERMISSIONS")
  ("INVALID-PASSWORD" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:INVALID-PASSWORD")
  ("INVALID-PERMISSIONS" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:INVALID-PERMISSIONS")
  ("INVALID-UPDATE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:INVALID-UPDATE")
  ("JOIN" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:JOIN")
  ("KICK" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:KICK")
  ("LEAVE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:LEAVE")
  ("MALFORMED-UPDATE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:MALFORMED-UPDATE")
  ("MESSAGE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:MESSAGE")
  ("NAMED-OBJECT" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:NAMED-OBJECT")
  ("NO-SUCH-CHANNEL" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:NO-SUCH-CHANNEL")
  ("NO-SUCH-PROFILE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:NO-SUCH-PROFILE")
  ("NO-SUCH-USER" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:NO-SUCH-USER")
  ("NOT-IN-CHANNEL" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:NOT-IN-CHANNEL")
  ("PERMISSIONS" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:PERMISSIONS")
  ("PING" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:PING")
  ("PONG" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:PONG")
  ("PROFILE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:PROFILE")
  ("PULL" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:PULL")
  ("REGISTER" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:REGISTER")
  ("SERVER-OBJECT" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:SERVER-OBJECT")
  ("TARGET-UPDATE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:TARGET-UPDATE")
  ("TEXT-UPDATE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:TEXT-UPDATE")
  ("TOO-MANY-CONNECTIONS" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:TOO-MANY-CONNECTIONS")
  ("TOO-MANY-UPDATES" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:TOO-MANY-UPDATES")
  ("UPDATE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:UPDATE")
  ("UPDATE-FAILURE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:UPDATE-FAILURE")
  ("USER" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:USER")
  ("USER-INFO" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:USER-INFO")
  ("USERNAME-MISMATCH" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:USERNAME-MISMATCH")
  ("USERNAME-TAKEN" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:USERNAME-TAKEN")
  ("USERS" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:USERS")
  ("WIRE-OBJECT" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:WIRE-OBJECT")
  ("INCOMPATIBLE-VALUE-TYPE-FOR-SLOT" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:INCOMPATIBLE-VALUE-TYPE-FOR-SLOT")
  ("INCOMPLETE-TOKEN" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:INCOMPLETE-TOKEN")
  ("MISSING-CLOCK" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:MISSING-CLOCK")
  ("MISSING-ID" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:MISSING-ID")
  ("MISSING-UPDATE-ARGUMENT" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:MISSING-UPDATE-ARGUMENT")
  ("PRINTER-CONDITION" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:PRINTER-CONDITION")
  ("PROTOCOL-CONDITION" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:PROTOCOL-CONDITION")
  ("READER-CONDITION" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:READER-CONDITION")
  ("UNKNOWN-SYMBOL" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:UNKNOWN-SYMBOL")
  ("UNKNOWN-WIRE-OBJECT" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:UNKNOWN-WIRE-OBJECT")
  ("UNPRINTABLE-OBJECT" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:UNPRINTABLE-OBJECT")
  ("WIRE-CONDITION" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:WIRE-CONDITION")
  ("CHANNEL" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CHANNEL")
  ("CHANNELS" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CHANNELS")
  ("CLOCK" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CLOCK")
  ("CONNECTIONS" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CONNECTIONS")
  ("FROM" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:FROM")
  ("ID" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:ID")
  ("LIFETIME" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:LIFETIME")
  ("NAME" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:NAME")
  ("PASSWORD" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:PASSWORD")
  ("PERMISSIONS" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:PERMISSIONS")
  ("REGISTERED" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:REGISTERED")
  ("TARGET" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:TARGET")
  ("TEXT" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:TEXT")
  ("UPDATE-ID" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:UPDATE-ID")
  ("USER" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:USER")
  ("USERS" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:USERS")
  ("VERSION" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:VERSION")
  ("CHANNELNAME-P" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:CHANNELNAME-P")
  ("FROM-WIRE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:FROM-WIRE")
  ("ID-P" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:ID-P")
  ("NEXT-ID" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:NEXT-ID")
  ("PASSWORD-P" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:PASSWORD-P")
  ("PROTOCOL-VERSION" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:PROTOCOL-VERSION")
  ("TO-WIRE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:TO-WIRE")
  ("USERNAME-P" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:USERNAME-P")
  ("WHITESPACE-P" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:WHITESPACE-P")
  ("OBJECT" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:OBJECT")
  ("SYMBOL-DESIGNATOR" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:SYMBOL-DESIGNATOR")
  ("UPDATE" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:UPDATE")
  ("DEFINE-PROTOCOL-CLASS" "https://shirakumo.github.io/lichat-protocol/#LICHAT-PROTOCOL:DEFINE-PROTOCOL-CLASS"))

(define-table-lookup radiance
  (("1" "radiance concepts & parts") "https://github.com/shirakumo/radiance#1-radiance-concepts--parts")
  (("1.1" "uri") "https://github.com/shirakumo/radiance#11-uri")
  (("1.2" "request and response") "https://github.com/shirakumo/radiance#12-request-and-response")
  (("1.3" "route") "https://github.com/shirakumo/radiance#13-route")
  (("1.4" "uri dispatcher") "https://github.com/shirakumo/radiance#1.4-uri-dispatcher")
  (("1.5" "page") "https://github.com/shirakumo/radiance#15-page")
  (("1.6" "api endpoint") "https://github.com/shirakumo/radiance#16-api-endpoint")
  (("1.7" "options") "https://github.com/shirakumo/radiance#17-options")
  (("1.8" "module") "https://github.com/shirakumo/radiance#18-module")
  (("1.9" "hooks") "https://github.com/shirakumo/radiance#19-hooks")
  (("1.10" "interface") "https://github.com/shirakumo/radiance#110-interface")
  (("1.11" "environment") "https://github.com/shirakumo/radiance#111-environment")
  (("1.12" "instance management") "https://github.com/shirakumo/radiance#112-instance-management")
  (("2" "standard interfaces") "https://github.com/shirakumo/radiance#2-standard-interfaces")
  (("2.1" "admin") "https://github.com/shirakumo/radiance#21-admin")
  (("2.2" "auth") "https://github.com/shirakumo/radiance#22-auth")
  (("2.3" "ban") "https://github.com/shirakumo/radiance#23-ban")
  (("2.4" "cache") "https://github.com/shirakumo/radiance#24-cache")
  (("2.5" "database") "https://github.com/shirakumo/radiance#25-database")
  (("2.6" "logger") "https://github.com/shirakumo/radiance#26-logger")
  (("2.7" "mail") "https://github.com/shirakumo/radiance#27-mail")
  (("2.8" "profile") "https://github.com/shirakumo/radiance#28-profile")
  (("2.9" "rate") "https://github.com/shirakumo/radiance#29-rate")
  (("2.10" "server") "https://github.com/shirakumo/radiance#210-server")
  (("2.11" "session") "https://github.com/shirakumo/radiance#211-session")
  (("2.12" "user") "https://github.com/shirakumo/radiance#212-user")
  (("SPECIAL RADIANCE-CORE:*DEBUGGER*" "RADIANCE-CORE:*DEBUGGER*" "*DEBUGGER*") "https://shirakumo.github.io/radiance#SPECIAL%20RADIANCE-CORE%3A%2ADEBUGGER%2A"
   "Special radiance-core:*debugger*")
  (("SPECIAL RADIANCE-CORE:*DEFAULT-API-FORMAT*" "RADIANCE-CORE:*DEFAULT-API-FORMAT*" "*DEFAULT-API-FORMAT*") "https://shirakumo.github.io/radiance#SPECIAL%20RADIANCE-CORE%3A%2ADEFAULT-API-FORMAT%2A"
   "Special radiance-core:*default-api-format*")
  (("SPECIAL RADIANCE-CORE:*DEFAULT-CONTENT-TYPE*" "RADIANCE-CORE:*DEFAULT-CONTENT-TYPE*" "*DEFAULT-CONTENT-TYPE*") "https://shirakumo.github.io/radiance#SPECIAL%20RADIANCE-CORE%3A%2ADEFAULT-CONTENT-TYPE%2A"
   "Special radiance-core:*default-content-type*")
  (("SPECIAL RADIANCE-CORE:*DEFAULT-EXTERNAL-FORMAT*" "RADIANCE-CORE:*DEFAULT-EXTERNAL-FORMAT*" "*DEFAULT-EXTERNAL-FORMAT*") "https://shirakumo.github.io/radiance#SPECIAL%20RADIANCE-CORE%3A%2ADEFAULT-EXTERNAL-FORMAT%2A"
   "Special radiance-core:*default-external-format*")
  (("SPECIAL RADIANCE-CORE:*ENVIRONMENT-ROOT*" "RADIANCE-CORE:*ENVIRONMENT-ROOT*" "*ENVIRONMENT-ROOT*") "https://shirakumo.github.io/radiance#SPECIAL%20RADIANCE-CORE%3A%2AENVIRONMENT-ROOT%2A"
   "Special radiance-core:*environment-root*")
  (("SPECIAL RADIANCE-CORE:*MODULES-DIRECTORY*" "RADIANCE-CORE:*MODULES-DIRECTORY*" "*MODULES-DIRECTORY*") "https://shirakumo.github.io/radiance#SPECIAL%20RADIANCE-CORE%3A%2AMODULES-DIRECTORY%2A"
   "Special radiance-core:*modules-directory*")
  (("SPECIAL RADIANCE-CORE:*RANDOM-STRING-CHARACTERS*" "RADIANCE-CORE:*RANDOM-STRING-CHARACTERS*" "*RANDOM-STRING-CHARACTERS*") "https://shirakumo.github.io/radiance#SPECIAL%20RADIANCE-CORE%3A%2ARANDOM-STRING-CHARACTERS%2A"
   "Special radiance-core:*random-string-characters*")
  (("SPECIAL RADIANCE-CORE:*REQUEST*" "RADIANCE-CORE:*REQUEST*" "*REQUEST*") "https://shirakumo.github.io/radiance#SPECIAL%20RADIANCE-CORE%3A%2AREQUEST%2A"
   "Special radiance-core:*request*")
  (("SPECIAL RADIANCE-CORE:*RESPONSE*" "RADIANCE-CORE:*RESPONSE*" "*RESPONSE*") "https://shirakumo.github.io/radiance#SPECIAL%20RADIANCE-CORE%3A%2ARESPONSE%2A"
   "Special radiance-core:*response*")
  (("SPECIAL RADIANCE-CORE:*STARTUP-TIME*" "RADIANCE-CORE:*STARTUP-TIME*" "*STARTUP-TIME*") "https://shirakumo.github.io/radiance#SPECIAL%20RADIANCE-CORE%3A%2ASTARTUP-TIME%2A"
   "Special radiance-core:*startup-time*")
  (("CLASS RADIANCE-CORE:API-ENDPOINT" "RADIANCE-CORE:API-ENDPOINT" "API-ENDPOINT") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3AAPI-ENDPOINT"
   "Class radiance-core:api-endpoint")
  (("CLASS RADIANCE-CORE:COOKIE" "RADIANCE-CORE:COOKIE" "COOKIE") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3ACOOKIE"
   "Class radiance-core:cookie")
  (("CLASS RADIANCE-CORE:DOCUMENTABLE" "RADIANCE-CORE:DOCUMENTABLE" "DOCUMENTABLE") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3ADOCUMENTABLE"
   "Class radiance-core:documentable")
  (("CLASS RADIANCE-CORE:HOOK" "RADIANCE-CORE:HOOK" "HOOK") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3AHOOK"
   "Class radiance-core:hook")
  (("CLASS RADIANCE-CORE:MODULE" "RADIANCE-CORE:MODULE" "MODULE") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3AMODULE"
   "Class radiance-core:module")
  (("CLASS RADIANCE-CORE:OPTION" "RADIANCE-CORE:OPTION" "OPTION") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3AOPTION"
   "Class radiance-core:option")
  (("CLASS RADIANCE-CORE:REQUEST" "RADIANCE-CORE:REQUEST" "REQUEST") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3AREQUEST"
   "Class radiance-core:request")
  (("CLASS RADIANCE-CORE:RESOURCE-TYPE" "RADIANCE-CORE:RESOURCE-TYPE" "RESOURCE-TYPE") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3ARESOURCE-TYPE"
   "Class radiance-core:resource-type")
  (("CLASS RADIANCE-CORE:RESPONSE" "RADIANCE-CORE:RESPONSE" "RESPONSE") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3ARESPONSE"
   "Class radiance-core:response")
  (("CLASS RADIANCE-CORE:ROUTE" "RADIANCE-CORE:ROUTE" "ROUTE") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3AROUTE"
   "Class radiance-core:route")
  (("CLASS RADIANCE-CORE:URI" "RADIANCE-CORE:URI" "URI") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3AURI"
   "Class radiance-core:uri")
  (("CLASS RADIANCE-CORE:URI-DISPATCHER" "RADIANCE-CORE:URI-DISPATCHER" "URI-DISPATCHER") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3AURI-DISPATCHER"
   "Class radiance-core:uri-dispatcher")
  (("CLASS RADIANCE-CORE:VIRTUAL-MODULE" "RADIANCE-CORE:VIRTUAL-MODULE" "VIRTUAL-MODULE") "https://shirakumo.github.io/radiance#CLASS%20RADIANCE-CORE%3AVIRTUAL-MODULE"
   "Class radiance-core:virtual-module")
  (("RESOURCE-TYPE RADIANCE-CORE:API" "RADIANCE-CORE:API" "API") "https://shirakumo.github.io/radiance#RESOURCE-TYPE%20RADIANCE-CORE%3AAPI"
   "Resource-type radiance-core:api")
  (("RESOURCE-TYPE RADIANCE-CORE:DOMAIN" "RADIANCE-CORE:DOMAIN" "DOMAIN") "https://shirakumo.github.io/radiance#RESOURCE-TYPE%20RADIANCE-CORE%3ADOMAIN"
   "Resource-type radiance-core:domain")
  (("RESOURCE-TYPE RADIANCE-CORE:PAGE" "RADIANCE-CORE:PAGE" "PAGE") "https://shirakumo.github.io/radiance#RESOURCE-TYPE%20RADIANCE-CORE%3APAGE"
   "Resource-type radiance-core:page")
  (("RESOURCE-TYPE RADIANCE-CORE:STATIC" "RADIANCE-CORE:STATIC" "STATIC") "https://shirakumo.github.io/radiance#RESOURCE-TYPE%20RADIANCE-CORE%3ASTATIC"
   "Resource-type radiance-core:static")
  (("option PAGE RADIANCE-CORE:ACCESS" "PAGE RADIANCE-CORE:ACCESS" "ACCESS") "https://shirakumo.github.io/radiance#option%20PAGE%20RADIANCE-CORE%3AACCESS"
   "Option page radiance-core:access")
  (("option PAGE RADIANCE-CORE:HOOK" "PAGE RADIANCE-CORE:HOOK" "HOOK") "https://shirakumo.github.io/radiance#option%20PAGE%20RADIANCE-CORE%3AHOOK"
   "Option page radiance-core:hook")
  (("option API RADIANCE-CORE:ACCESS" "API RADIANCE-CORE:ACCESS" "ACCESS") "https://shirakumo.github.io/radiance#option%20API%20RADIANCE-CORE%3AACCESS"
   "Option api radiance-core:access")
  (("ROUTE RADIANCE-CORE:DOMAIN" "RADIANCE-CORE:DOMAIN" "DOMAIN") "https://shirakumo.github.io/radiance#ROUTE%20RADIANCE-CORE%3ADOMAIN"
   "Route radiance-core:domain")
  (("ROUTE RADIANCE-CORE:DOMAIN" "RADIANCE-CORE:DOMAIN" "DOMAIN") "https://shirakumo.github.io/radiance#ROUTE%20RADIANCE-CORE%3ADOMAIN"
   "Route radiance-core:domain")
  (("ROUTE RADIANCE-CORE:VIRTUAL-MODULE" "RADIANCE-CORE:VIRTUAL-MODULE" "VIRTUAL-MODULE") "https://shirakumo.github.io/radiance#ROUTE%20RADIANCE-CORE%3AVIRTUAL-MODULE"
   "Route radiance-core:virtual-module")
  (("ROUTE RADIANCE-CORE:VIRTUAL-MODULE" "RADIANCE-CORE:VIRTUAL-MODULE" "VIRTUAL-MODULE") "https://shirakumo.github.io/radiance#ROUTE%20RADIANCE-CORE%3AVIRTUAL-MODULE"
   "Route radiance-core:virtual-module")
  (("CONDITION RADIANCE-CORE:API-ARGUMENT-INVALID" "RADIANCE-CORE:API-ARGUMENT-INVALID" "API-ARGUMENT-INVALID") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AAPI-ARGUMENT-INVALID"
   "Condition radiance-core:api-argument-invalid")
  (("CONDITION RADIANCE-CORE:API-ARGUMENT-MISSING" "RADIANCE-CORE:API-ARGUMENT-MISSING" "API-ARGUMENT-MISSING") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AAPI-ARGUMENT-MISSING"
   "Condition radiance-core:api-argument-missing")
  (("CONDITION RADIANCE-CORE:API-AUTH-ERROR" "RADIANCE-CORE:API-AUTH-ERROR" "API-AUTH-ERROR") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AAPI-AUTH-ERROR"
   "Condition radiance-core:api-auth-error")
  (("CONDITION RADIANCE-CORE:API-CALL-NOT-FOUND" "RADIANCE-CORE:API-CALL-NOT-FOUND" "API-CALL-NOT-FOUND") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AAPI-CALL-NOT-FOUND"
   "Condition radiance-core:api-call-not-found")
  (("CONDITION RADIANCE-CORE:API-ERROR" "RADIANCE-CORE:API-ERROR" "API-ERROR") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AAPI-ERROR"
   "Condition radiance-core:api-error")
  (("CONDITION RADIANCE-CORE:API-RESPONSE-EMPTY" "RADIANCE-CORE:API-RESPONSE-EMPTY" "API-RESPONSE-EMPTY") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AAPI-RESPONSE-EMPTY"
   "Condition radiance-core:api-response-empty")
  (("CONDITION RADIANCE-CORE:API-UNKNOWN-FORMAT" "RADIANCE-CORE:API-UNKNOWN-FORMAT" "API-UNKNOWN-FORMAT") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AAPI-UNKNOWN-FORMAT"
   "Condition radiance-core:api-unknown-format")
  (("CONDITION RADIANCE-CORE:ENVIRONMENT-NOT-SET" "RADIANCE-CORE:ENVIRONMENT-NOT-SET" "ENVIRONMENT-NOT-SET") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AENVIRONMENT-NOT-SET"
   "Condition radiance-core:environment-not-set")
  (("CONDITION RADIANCE-CORE:FILE-TO-SERVE-DOES-NOT-EXIST" "RADIANCE-CORE:FILE-TO-SERVE-DOES-NOT-EXIST" "FILE-TO-SERVE-DOES-NOT-EXIST") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AFILE-TO-SERVE-DOES-NOT-EXIST"
   "Condition radiance-core:file-to-serve-does-not-exist")
  (("CONDITION RADIANCE-CORE:INTERFACE-IMPLEMENTATION-NOT-SET" "RADIANCE-CORE:INTERFACE-IMPLEMENTATION-NOT-SET" "INTERFACE-IMPLEMENTATION-NOT-SET") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AINTERFACE-IMPLEMENTATION-NOT-SET"
   "Condition radiance-core:interface-implementation-not-set")
  (("CONDITION RADIANCE-CORE:INTERNAL-ERROR" "RADIANCE-CORE:INTERNAL-ERROR" "INTERNAL-ERROR") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AINTERNAL-ERROR"
   "Condition radiance-core:internal-error")
  (("CONDITION RADIANCE-CORE:NO-SUCH-POST-PARAMETER" "RADIANCE-CORE:NO-SUCH-POST-PARAMETER" "NO-SUCH-POST-PARAMETER") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3ANO-SUCH-POST-PARAMETER"
   "Condition radiance-core:no-such-post-parameter")
  (("CONDITION RADIANCE-CORE:POST-PARAMETER-NOT-A-FILE" "RADIANCE-CORE:POST-PARAMETER-NOT-A-FILE" "POST-PARAMETER-NOT-A-FILE") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3APOST-PARAMETER-NOT-A-FILE"
   "Condition radiance-core:post-parameter-not-a-file")
  (("CONDITION RADIANCE-CORE:RADIANCE-CONDITION" "RADIANCE-CORE:RADIANCE-CONDITION" "RADIANCE-CONDITION") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3ARADIANCE-CONDITION"
   "Condition radiance-core:radiance-condition")
  (("CONDITION RADIANCE-CORE:REQUEST-DENIED" "RADIANCE-CORE:REQUEST-DENIED" "REQUEST-DENIED") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AREQUEST-DENIED"
   "Condition radiance-core:request-denied")
  (("CONDITION RADIANCE-CORE:REQUEST-EMPTY" "RADIANCE-CORE:REQUEST-EMPTY" "REQUEST-EMPTY") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AREQUEST-EMPTY"
   "Condition radiance-core:request-empty")
  (("CONDITION RADIANCE-CORE:REQUEST-ERROR" "RADIANCE-CORE:REQUEST-ERROR" "REQUEST-ERROR") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AREQUEST-ERROR"
   "Condition radiance-core:request-error")
  (("CONDITION RADIANCE-CORE:REQUEST-NOT-FOUND" "RADIANCE-CORE:REQUEST-NOT-FOUND" "REQUEST-NOT-FOUND") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AREQUEST-NOT-FOUND"
   "Condition radiance-core:request-not-found")
  (("CONDITION RADIANCE-CORE:UNPARSABLE-URI-STRING" "RADIANCE-CORE:UNPARSABLE-URI-STRING" "UNPARSABLE-URI-STRING") "https://shirakumo.github.io/radiance#CONDITION%20RADIANCE-CORE%3AUNPARSABLE-URI-STRING"
   "Condition radiance-core:unparsable-uri-string")
  (("HOOK RADIANCE-CORE:ENVIRONMENT-CHANGE" "RADIANCE-CORE:ENVIRONMENT-CHANGE" "ENVIRONMENT-CHANGE") "https://shirakumo.github.io/radiance#HOOK%20RADIANCE-CORE%3AENVIRONMENT-CHANGE"
   "Hook radiance-core:environment-change")
  (("HOOK RADIANCE-CORE:REQUEST" "RADIANCE-CORE:REQUEST" "REQUEST") "https://shirakumo.github.io/radiance#HOOK%20RADIANCE-CORE%3AREQUEST"
   "Hook radiance-core:request")
  (("HOOK RADIANCE-CORE:SERVER-READY" "RADIANCE-CORE:SERVER-READY" "SERVER-READY") "https://shirakumo.github.io/radiance#HOOK%20RADIANCE-CORE%3ASERVER-READY"
   "Hook radiance-core:server-ready")
  (("HOOK RADIANCE-CORE:SERVER-SHUTDOWN" "RADIANCE-CORE:SERVER-SHUTDOWN" "SERVER-SHUTDOWN") "https://shirakumo.github.io/radiance#HOOK%20RADIANCE-CORE%3ASERVER-SHUTDOWN"
   "Hook radiance-core:server-shutdown")
  (("HOOK RADIANCE-CORE:SERVER-START" "RADIANCE-CORE:SERVER-START" "SERVER-START") "https://shirakumo.github.io/radiance#HOOK%20RADIANCE-CORE%3ASERVER-START"
   "Hook radiance-core:server-start")
  (("HOOK RADIANCE-CORE:SERVER-STOP" "RADIANCE-CORE:SERVER-STOP" "SERVER-STOP") "https://shirakumo.github.io/radiance#HOOK%20RADIANCE-CORE%3ASERVER-STOP"
   "Hook radiance-core:server-stop")
  (("HOOK RADIANCE-CORE:SHUTDOWN" "RADIANCE-CORE:SHUTDOWN" "SHUTDOWN") "https://shirakumo.github.io/radiance#HOOK%20RADIANCE-CORE%3ASHUTDOWN"
   "Hook radiance-core:shutdown")
  (("HOOK RADIANCE-CORE:SHUTDOWN-DONE" "RADIANCE-CORE:SHUTDOWN-DONE" "SHUTDOWN-DONE") "https://shirakumo.github.io/radiance#HOOK%20RADIANCE-CORE%3ASHUTDOWN-DONE"
   "Hook radiance-core:shutdown-done")
  (("HOOK RADIANCE-CORE:STARTUP" "RADIANCE-CORE:STARTUP" "STARTUP") "https://shirakumo.github.io/radiance#HOOK%20RADIANCE-CORE%3ASTARTUP"
   "Hook radiance-core:startup")
  (("HOOK RADIANCE-CORE:STARTUP-DONE" "RADIANCE-CORE:STARTUP-DONE" "STARTUP-DONE") "https://shirakumo.github.io/radiance#HOOK%20RADIANCE-CORE%3ASTARTUP-DONE"
   "Hook radiance-core:startup-done")
  (("ACCESSOR RADIANCE-CORE:API-ENDPOINT" "RADIANCE-CORE:API-ENDPOINT" "API-ENDPOINT") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AAPI-ENDPOINT"
   "Accessor radiance-core:api-endpoint")
  (("ACCESSOR RADIANCE-CORE:API-FORMAT" "RADIANCE-CORE:API-FORMAT" "API-FORMAT") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AAPI-FORMAT"
   "Accessor radiance-core:api-format")
  (("ACCESSOR RADIANCE-CORE:ARGSLIST" "RADIANCE-CORE:ARGSLIST" "ARGSLIST") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AARGSLIST"
   "Accessor radiance-core:argslist")
  (("ACCESSOR RADIANCE-CORE:CONTENT-TYPE" "RADIANCE-CORE:CONTENT-TYPE" "CONTENT-TYPE") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ACONTENT-TYPE"
   "Accessor radiance-core:content-type")
  (("ACCESSOR RADIANCE-CORE:COOKIE" "RADIANCE-CORE:COOKIE" "COOKIE") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ACOOKIE"
   "Accessor radiance-core:cookie")
  (("ACCESSOR RADIANCE-CORE:COOKIES" "RADIANCE-CORE:COOKIES" "COOKIES") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ACOOKIES"
   "Accessor radiance-core:cookies")
  (("ACCESSOR RADIANCE-CORE:DATA" "RADIANCE-CORE:DATA" "DATA") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ADATA"
   "Accessor radiance-core:data")
  (("ACCESSOR RADIANCE-CORE:DIRECTION" "RADIANCE-CORE:DIRECTION" "DIRECTION") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ADIRECTION"
   "Accessor radiance-core:direction")
  (("ACCESSOR RADIANCE-CORE:DISPATCH-FUNCTION" "RADIANCE-CORE:DISPATCH-FUNCTION" "DISPATCH-FUNCTION") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ADISPATCH-FUNCTION"
   "Accessor radiance-core:dispatch-function")
  (("ACCESSOR RADIANCE-CORE:DOMAIN" "RADIANCE-CORE:DOMAIN" "DOMAIN") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ADOMAIN"
   "Accessor radiance-core:domain")
  (("ACCESSOR RADIANCE-CORE:DOMAINS" "RADIANCE-CORE:DOMAINS" "DOMAINS") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ADOMAINS"
   "Accessor radiance-core:domains")
  (("ACCESSOR RADIANCE-CORE:ENVIRONMENT" "RADIANCE-CORE:ENVIRONMENT" "ENVIRONMENT") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AENVIRONMENT"
   "Accessor radiance-core:environment")
  (("ACCESSOR RADIANCE-CORE:EXPANDER" "RADIANCE-CORE:EXPANDER" "EXPANDER") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AEXPANDER"
   "Accessor radiance-core:expander")
  (("ACCESSOR RADIANCE-CORE:EXPIRES" "RADIANCE-CORE:EXPIRES" "EXPIRES") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AEXPIRES"
   "Accessor radiance-core:expires")
  (("ACCESSOR RADIANCE-CORE:EXTERNAL-FORMAT" "RADIANCE-CORE:EXTERNAL-FORMAT" "EXTERNAL-FORMAT") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AEXTERNAL-FORMAT"
   "Accessor radiance-core:external-format")
  (("ACCESSOR RADIANCE-CORE:GET-DATA" "RADIANCE-CORE:GET-DATA" "GET-DATA") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AGET-DATA"
   "Accessor radiance-core:get-data")
  (("ACCESSOR RADIANCE-CORE:HANDLER" "RADIANCE-CORE:HANDLER" "HANDLER") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AHANDLER"
   "Accessor radiance-core:handler")
  (("ACCESSOR RADIANCE-CORE:HEADER" "RADIANCE-CORE:HEADER" "HEADER") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AHEADER"
   "Accessor radiance-core:header")
  (("ACCESSOR RADIANCE-CORE:HEADERS" "RADIANCE-CORE:HEADERS" "HEADERS") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AHEADERS"
   "Accessor radiance-core:headers")
  (("ACCESSOR RADIANCE-CORE:HOOK" "RADIANCE-CORE:HOOK" "HOOK") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AHOOK"
   "Accessor radiance-core:hook")
  (("ACCESSOR RADIANCE-CORE:HTTP-METHOD" "RADIANCE-CORE:HTTP-METHOD" "HTTP-METHOD") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AHTTP-METHOD"
   "Accessor radiance-core:http-method")
  (("ACCESSOR RADIANCE-CORE:HTTP-ONLY" "RADIANCE-CORE:HTTP-ONLY" "HTTP-ONLY") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AHTTP-ONLY"
   "Accessor radiance-core:http-only")
  (("ACCESSOR RADIANCE-CORE:IMPLEMENTATION" "RADIANCE-CORE:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AIMPLEMENTATION"
   "Accessor radiance-core:implementation")
  (("ACCESSOR RADIANCE-CORE:ISSUE-TIME" "RADIANCE-CORE:ISSUE-TIME" "ISSUE-TIME") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AISSUE-TIME"
   "Accessor radiance-core:issue-time")
  (("ACCESSOR RADIANCE-CORE:LOCATORS" "RADIANCE-CORE:LOCATORS" "LOCATORS") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ALOCATORS"
   "Accessor radiance-core:locators")
  (("ACCESSOR RADIANCE-CORE:MATCHER" "RADIANCE-CORE:MATCHER" "MATCHER") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AMATCHER"
   "Accessor radiance-core:matcher")
  (("ACCESSOR RADIANCE-CORE:MCONFIG" "RADIANCE-CORE:MCONFIG" "MCONFIG") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AMCONFIG"
   "Accessor radiance-core:mconfig")
  (("ACCESSOR RADIANCE-CORE:MESSAGE" "RADIANCE-CORE:MESSAGE" "MESSAGE") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AMESSAGE"
   "Accessor radiance-core:message")
  (("ACCESSOR RADIANCE-CORE:MODULE-PERMISSIONS" "RADIANCE-CORE:MODULE-PERMISSIONS" "MODULE-PERMISSIONS") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AMODULE-PERMISSIONS"
   "Accessor radiance-core:module-permissions")
  (("ACCESSOR RADIANCE-CORE:MODULE-STORAGE" "RADIANCE-CORE:MODULE-STORAGE" "MODULE-STORAGE") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AMODULE-STORAGE"
   "Accessor radiance-core:module-storage")
  (("ACCESSOR RADIANCE-CORE:NAME" "RADIANCE-CORE:NAME" "NAME") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ANAME"
   "Accessor radiance-core:name")
  (("ACCESSOR RADIANCE-CORE:OPTION" "RADIANCE-CORE:OPTION" "OPTION") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AOPTION"
   "Accessor radiance-core:option")
  (("ACCESSOR RADIANCE-CORE:OPTION-TYPE" "RADIANCE-CORE:OPTION-TYPE" "OPTION-TYPE") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AOPTION-TYPE"
   "Accessor radiance-core:option-type")
  (("ACCESSOR RADIANCE-CORE:PATH" "RADIANCE-CORE:PATH" "PATH") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3APATH"
   "Accessor radiance-core:path")
  (("ACCESSOR RADIANCE-CORE:PORT" "RADIANCE-CORE:PORT" "PORT") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3APORT"
   "Accessor radiance-core:port")
  (("ACCESSOR RADIANCE-CORE:POST-DATA" "RADIANCE-CORE:POST-DATA" "POST-DATA") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3APOST-DATA"
   "Accessor radiance-core:post-data")
  (("ACCESSOR RADIANCE-CORE:PRIORITY" "RADIANCE-CORE:PRIORITY" "PRIORITY") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3APRIORITY"
   "Accessor radiance-core:priority")
  (("ACCESSOR RADIANCE-CORE:REFERER" "RADIANCE-CORE:REFERER" "REFERER") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AREFERER"
   "Accessor radiance-core:referer")
  (("ACCESSOR RADIANCE-CORE:REMOTE" "RADIANCE-CORE:REMOTE" "REMOTE") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AREMOTE"
   "Accessor radiance-core:remote")
  (("ACCESSOR RADIANCE-CORE:REQUEST-HANDLER" "RADIANCE-CORE:REQUEST-HANDLER" "REQUEST-HANDLER") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AREQUEST-HANDLER"
   "Accessor radiance-core:request-handler")
  (("ACCESSOR RADIANCE-CORE:RESOURCE-LOCATOR" "RADIANCE-CORE:RESOURCE-LOCATOR" "RESOURCE-LOCATOR") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ARESOURCE-LOCATOR"
   "Accessor radiance-core:resource-locator")
  (("ACCESSOR RADIANCE-CORE:RESOURCE-TYPE" "RADIANCE-CORE:RESOURCE-TYPE" "RESOURCE-TYPE") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ARESOURCE-TYPE"
   "Accessor radiance-core:resource-type")
  (("ACCESSOR RADIANCE-CORE:RETURN-CODE" "RADIANCE-CORE:RETURN-CODE" "RETURN-CODE") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ARETURN-CODE"
   "Accessor radiance-core:return-code")
  (("ACCESSOR RADIANCE-CORE:ROUTE" "RADIANCE-CORE:ROUTE" "ROUTE") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AROUTE"
   "Accessor radiance-core:route")
  (("ACCESSOR RADIANCE-CORE:SECURE" "RADIANCE-CORE:SECURE" "SECURE") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ASECURE"
   "Accessor radiance-core:secure")
  (("ACCESSOR RADIANCE-CORE:TRANSLATOR" "RADIANCE-CORE:TRANSLATOR" "TRANSLATOR") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3ATRANSLATOR"
   "Accessor radiance-core:translator")
  (("ACCESSOR RADIANCE-CORE:URI" "RADIANCE-CORE:URI" "URI") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AURI"
   "Accessor radiance-core:uri")
  (("ACCESSOR RADIANCE-CORE:URI-DISPATCHER" "RADIANCE-CORE:URI-DISPATCHER" "URI-DISPATCHER") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AURI-DISPATCHER"
   "Accessor radiance-core:uri-dispatcher")
  (("ACCESSOR RADIANCE-CORE:USER-AGENT" "RADIANCE-CORE:USER-AGENT" "USER-AGENT") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AUSER-AGENT"
   "Accessor radiance-core:user-agent")
  (("ACCESSOR RADIANCE-CORE:VALUE" "RADIANCE-CORE:VALUE" "VALUE") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AVALUE"
   "Accessor radiance-core:value")
  (("ACCESSOR RADIANCE-CORE:VIRTUAL-MODULE-NAME" "RADIANCE-CORE:VIRTUAL-MODULE-NAME" "VIRTUAL-MODULE-NAME") "https://shirakumo.github.io/radiance#ACCESSOR%20RADIANCE-CORE%3AVIRTUAL-MODULE-NAME"
   "Accessor radiance-core:virtual-module-name")
  (("FUNCTION RADIANCE-CORE:*REQUEST*" "RADIANCE-CORE:*REQUEST*" "*REQUEST*") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3A%2AREQUEST%2A"
   "Function radiance-core:*request*")
  (("FUNCTION RADIANCE-CORE:*RESPONSE*" "RADIANCE-CORE:*RESPONSE*" "*RESPONSE*") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3A%2ARESPONSE%2A"
   "Function radiance-core:*response*")
  (("FUNCTION RADIANCE-CORE:ABORT-HANDLING" "RADIANCE-CORE:ABORT-HANDLING" "ABORT-HANDLING") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AABORT-HANDLING"
   "Function radiance-core:abort-handling")
  (("FUNCTION RADIANCE-CORE:ADD-DOMAIN" "RADIANCE-CORE:ADD-DOMAIN" "ADD-DOMAIN") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AADD-DOMAIN"
   "Function radiance-core:add-domain")
  (("FUNCTION RADIANCE-CORE:API-OUTPUT" "RADIANCE-CORE:API-OUTPUT" "API-OUTPUT") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AAPI-OUTPUT"
   "Function radiance-core:api-output")
  (("FUNCTION RADIANCE-CORE:CALL-API" "RADIANCE-CORE:CALL-API" "CALL-API") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ACALL-API"
   "Function radiance-core:call-api")
  (("FUNCTION RADIANCE-CORE:CALL-API-REQUEST" "RADIANCE-CORE:CALL-API-REQUEST" "CALL-API-REQUEST") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ACALL-API-REQUEST"
   "Function radiance-core:call-api-request")
  (("FUNCTION RADIANCE-CORE:CHECK-ENVIRONMENT" "RADIANCE-CORE:CHECK-ENVIRONMENT" "CHECK-ENVIRONMENT") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ACHECK-ENVIRONMENT"
   "Function radiance-core:check-environment")
  (("FUNCTION RADIANCE-CORE:COOKIE-HEADER" "RADIANCE-CORE:COOKIE-HEADER" "COOKIE-HEADER") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ACOOKIE-HEADER"
   "Function radiance-core:cookie-header")
  (("FUNCTION RADIANCE-CORE:COPY-URI" "RADIANCE-CORE:COPY-URI" "COPY-URI") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ACOPY-URI"
   "Function radiance-core:copy-uri")
  (("FUNCTION RADIANCE-CORE:CREATE-MODULE" "RADIANCE-CORE:CREATE-MODULE" "CREATE-MODULE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ACREATE-MODULE"
   "Function radiance-core:create-module")
  (("FUNCTION RADIANCE-CORE:DEFAULTED-MCONFIG" "RADIANCE-CORE:DEFAULTED-MCONFIG" "DEFAULTED-MCONFIG") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ADEFAULTED-MCONFIG"
   "Function radiance-core:defaulted-mconfig")
  (("FUNCTION RADIANCE-CORE:DELETE-MODULE" "RADIANCE-CORE:DELETE-MODULE" "DELETE-MODULE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ADELETE-MODULE"
   "Function radiance-core:delete-module")
  (("FUNCTION RADIANCE-CORE:DESCRIBE-MODULE" "RADIANCE-CORE:DESCRIBE-MODULE" "DESCRIBE-MODULE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ADESCRIBE-MODULE"
   "Function radiance-core:describe-module")
  (("FUNCTION RADIANCE-CORE:DISPATCH" "RADIANCE-CORE:DISPATCH" "DISPATCH") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ADISPATCH"
   "Function radiance-core:dispatch")
  (("FUNCTION RADIANCE-CORE:ENSURE-URI" "RADIANCE-CORE:ENSURE-URI" "ENSURE-URI") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AENSURE-URI"
   "Function radiance-core:ensure-uri")
  (("FUNCTION RADIANCE-CORE:EXECUTE-REQUEST" "RADIANCE-CORE:EXECUTE-REQUEST" "EXECUTE-REQUEST") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AEXECUTE-REQUEST"
   "Function radiance-core:execute-request")
  (("FUNCTION RADIANCE-CORE:EXPAND-OPTIONS" "RADIANCE-CORE:EXPAND-OPTIONS" "EXPAND-OPTIONS") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AEXPAND-OPTIONS"
   "Function radiance-core:expand-options")
  (("FUNCTION RADIANCE-CORE:EXTERNAL-URI" "RADIANCE-CORE:EXTERNAL-URI" "EXTERNAL-URI") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AEXTERNAL-URI"
   "Function radiance-core:external-uri")
  (("FUNCTION RADIANCE-CORE:FILE" "RADIANCE-CORE:FILE" "FILE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AFILE"
   "Function radiance-core:file")
  (("FUNCTION RADIANCE-CORE:FILE-SIZE" "RADIANCE-CORE:FILE-SIZE" "FILE-SIZE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AFILE-SIZE"
   "Function radiance-core:file-size")
  (("FUNCTION RADIANCE-CORE:FIND-ALL-MODULES" "RADIANCE-CORE:FIND-ALL-MODULES" "FIND-ALL-MODULES") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AFIND-ALL-MODULES"
   "Function radiance-core:find-all-modules")
  (("FUNCTION RADIANCE-CORE:FIND-IMPLEMENTATION" "RADIANCE-CORE:FIND-IMPLEMENTATION" "FIND-IMPLEMENTATION") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AFIND-IMPLEMENTATION"
   "Function radiance-core:find-implementation")
  (("FUNCTION RADIANCE-CORE:FORMAT-CLOCK-TIME" "RADIANCE-CORE:FORMAT-CLOCK-TIME" "FORMAT-CLOCK-TIME") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AFORMAT-CLOCK-TIME"
   "Function radiance-core:format-clock-time")
  (("FUNCTION RADIANCE-CORE:FORMAT-FANCY-DATE" "RADIANCE-CORE:FORMAT-FANCY-DATE" "FORMAT-FANCY-DATE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AFORMAT-FANCY-DATE"
   "Function radiance-core:format-fancy-date")
  (("FUNCTION RADIANCE-CORE:FORMAT-HUMAN-DATE" "RADIANCE-CORE:FORMAT-HUMAN-DATE" "FORMAT-HUMAN-DATE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AFORMAT-HUMAN-DATE"
   "Function radiance-core:format-human-date")
  (("FUNCTION RADIANCE-CORE:FORMAT-MACHINE-DATE" "RADIANCE-CORE:FORMAT-MACHINE-DATE" "FORMAT-MACHINE-DATE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AFORMAT-MACHINE-DATE"
   "Function radiance-core:format-machine-date")
  (("FUNCTION RADIANCE-CORE:FORMAT-RELATIVE-TIME" "RADIANCE-CORE:FORMAT-RELATIVE-TIME" "FORMAT-RELATIVE-TIME") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AFORMAT-RELATIVE-TIME"
   "Function radiance-core:format-relative-time")
  (("FUNCTION RADIANCE-CORE:FORMAT-TIME" "RADIANCE-CORE:FORMAT-TIME" "FORMAT-TIME") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AFORMAT-TIME"
   "Function radiance-core:format-time")
  (("FUNCTION RADIANCE-CORE:FORMAT-URI" "RADIANCE-CORE:FORMAT-URI" "FORMAT-URI") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AFORMAT-URI"
   "Function radiance-core:format-uri")
  (("FUNCTION RADIANCE-CORE:GET-UNIX-TIME" "RADIANCE-CORE:GET-UNIX-TIME" "GET-UNIX-TIME") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AGET-UNIX-TIME"
   "Function radiance-core:get-unix-time")
  (("FUNCTION RADIANCE-CORE:GET-VAR" "RADIANCE-CORE:GET-VAR" "GET-VAR") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AGET-VAR"
   "Function radiance-core:get-var")
  (("FUNCTION RADIANCE-CORE:HANDLE-CONDITION" "RADIANCE-CORE:HANDLE-CONDITION" "HANDLE-CONDITION") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AHANDLE-CONDITION"
   "Function radiance-core:handle-condition")
  (("FUNCTION RADIANCE-CORE:IMPLEMENTS" "RADIANCE-CORE:IMPLEMENTS" "IMPLEMENTS") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AIMPLEMENTS"
   "Function radiance-core:implements")
  (("FUNCTION RADIANCE-CORE:INTERFACE" "RADIANCE-CORE:INTERFACE" "INTERFACE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AINTERFACE"
   "Function radiance-core:interface")
  (("FUNCTION RADIANCE-CORE:INTERFACE-P" "RADIANCE-CORE:INTERFACE-P" "INTERFACE-P") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AINTERFACE-P"
   "Function radiance-core:interface-p")
  (("FUNCTION RADIANCE-CORE:INTERNAL-URI" "RADIANCE-CORE:INTERNAL-URI" "INTERNAL-URI") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AINTERNAL-URI"
   "Function radiance-core:internal-uri")
  (("FUNCTION RADIANCE-CORE:LIST-API-ENDPOINTS" "RADIANCE-CORE:LIST-API-ENDPOINTS" "LIST-API-ENDPOINTS") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ALIST-API-ENDPOINTS"
   "Function radiance-core:list-api-endpoints")
  (("FUNCTION RADIANCE-CORE:LIST-API-FORMATS" "RADIANCE-CORE:LIST-API-FORMATS" "LIST-API-FORMATS") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ALIST-API-FORMATS"
   "Function radiance-core:list-api-formats")
  (("FUNCTION RADIANCE-CORE:LIST-HOOKS" "RADIANCE-CORE:LIST-HOOKS" "LIST-HOOKS") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ALIST-HOOKS"
   "Function radiance-core:list-hooks")
  (("FUNCTION RADIANCE-CORE:LIST-MODULES" "RADIANCE-CORE:LIST-MODULES" "LIST-MODULES") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ALIST-MODULES"
   "Function radiance-core:list-modules")
  (("FUNCTION RADIANCE-CORE:LIST-OPTIONS" "RADIANCE-CORE:LIST-OPTIONS" "LIST-OPTIONS") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ALIST-OPTIONS"
   "Function radiance-core:list-options")
  (("FUNCTION RADIANCE-CORE:LIST-RESOURCE-TYPES" "RADIANCE-CORE:LIST-RESOURCE-TYPES" "LIST-RESOURCE-TYPES") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ALIST-RESOURCE-TYPES"
   "Function radiance-core:list-resource-types")
  (("FUNCTION RADIANCE-CORE:LIST-ROUTES" "RADIANCE-CORE:LIST-ROUTES" "LIST-ROUTES") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ALIST-ROUTES"
   "Function radiance-core:list-routes")
  (("FUNCTION RADIANCE-CORE:LIST-URI-DISPATCHERS" "RADIANCE-CORE:LIST-URI-DISPATCHERS" "LIST-URI-DISPATCHERS") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ALIST-URI-DISPATCHERS"
   "Function radiance-core:list-uri-dispatchers")
  (("FUNCTION RADIANCE-CORE:LOAD-IMPLEMENTATION" "RADIANCE-CORE:LOAD-IMPLEMENTATION" "LOAD-IMPLEMENTATION") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ALOAD-IMPLEMENTATION"
   "Function radiance-core:load-implementation")
  (("FUNCTION RADIANCE-CORE:MAKE-RANDOM-STRING" "RADIANCE-CORE:MAKE-RANDOM-STRING" "MAKE-RANDOM-STRING") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMAKE-RANDOM-STRING"
   "Function radiance-core:make-random-string")
  (("FUNCTION RADIANCE-CORE:MAKE-URI" "RADIANCE-CORE:MAKE-URI" "MAKE-URI") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMAKE-URI"
   "Function radiance-core:make-uri")
  (("FUNCTION RADIANCE-CORE:MAKE-URL" "RADIANCE-CORE:MAKE-URL" "MAKE-URL") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMAKE-URL"
   "Function radiance-core:make-url")
  (("FUNCTION RADIANCE-CORE:MCONFIG-PATHNAME" "RADIANCE-CORE:MCONFIG-PATHNAME" "MCONFIG-PATHNAME") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMCONFIG-PATHNAME"
   "Function radiance-core:mconfig-pathname")
  (("FUNCTION RADIANCE-CORE:MCONFIG-STORAGE" "RADIANCE-CORE:MCONFIG-STORAGE" "MCONFIG-STORAGE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMCONFIG-STORAGE"
   "Function radiance-core:mconfig-storage")
  (("FUNCTION RADIANCE-CORE:MERGE-URIS" "RADIANCE-CORE:MERGE-URIS" "MERGE-URIS") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMERGE-URIS"
   "Function radiance-core:merge-uris")
  (("FUNCTION RADIANCE-CORE:MODULE" "RADIANCE-CORE:MODULE" "MODULE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMODULE"
   "Function radiance-core:module")
  (("FUNCTION RADIANCE-CORE:MODULE-API-ENDPOINTS" "RADIANCE-CORE:MODULE-API-ENDPOINTS" "MODULE-API-ENDPOINTS") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMODULE-API-ENDPOINTS"
   "Function radiance-core:module-api-endpoints")
  (("FUNCTION RADIANCE-CORE:MODULE-DEPENDENCIES" "RADIANCE-CORE:MODULE-DEPENDENCIES" "MODULE-DEPENDENCIES") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMODULE-DEPENDENCIES"
   "Function radiance-core:module-dependencies")
  (("FUNCTION RADIANCE-CORE:MODULE-DOMAIN" "RADIANCE-CORE:MODULE-DOMAIN" "MODULE-DOMAIN") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMODULE-DOMAIN"
   "Function radiance-core:module-domain")
  (("FUNCTION RADIANCE-CORE:MODULE-IDENTIFIER" "RADIANCE-CORE:MODULE-IDENTIFIER" "MODULE-IDENTIFIER") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMODULE-IDENTIFIER"
   "Function radiance-core:module-identifier")
  (("FUNCTION RADIANCE-CORE:MODULE-NAME" "RADIANCE-CORE:MODULE-NAME" "MODULE-NAME") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMODULE-NAME"
   "Function radiance-core:module-name")
  (("FUNCTION RADIANCE-CORE:MODULE-P" "RADIANCE-CORE:MODULE-P" "MODULE-P") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMODULE-P"
   "Function radiance-core:module-p")
  (("FUNCTION RADIANCE-CORE:MODULE-PAGES" "RADIANCE-CORE:MODULE-PAGES" "MODULE-PAGES") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMODULE-PAGES"
   "Function radiance-core:module-pages")
  (("FUNCTION RADIANCE-CORE:MODULE-REQUIRED-INTERFACES" "RADIANCE-CORE:MODULE-REQUIRED-INTERFACES" "MODULE-REQUIRED-INTERFACES") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMODULE-REQUIRED-INTERFACES"
   "Function radiance-core:module-required-interfaces")
  (("FUNCTION RADIANCE-CORE:MODULE-REQUIRED-SYSTEMS" "RADIANCE-CORE:MODULE-REQUIRED-SYSTEMS" "MODULE-REQUIRED-SYSTEMS") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMODULE-REQUIRED-SYSTEMS"
   "Function radiance-core:module-required-systems")
  (("FUNCTION RADIANCE-CORE:MODULE-STORAGE-REMOVE" "RADIANCE-CORE:MODULE-STORAGE-REMOVE" "MODULE-STORAGE-REMOVE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AMODULE-STORAGE-REMOVE"
   "Function radiance-core:module-storage-remove")
  (("FUNCTION RADIANCE-CORE:PARSE-PATH-SAFELY" "RADIANCE-CORE:PARSE-PATH-SAFELY" "PARSE-PATH-SAFELY") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3APARSE-PATH-SAFELY"
   "Function radiance-core:parse-path-safely")
  (("FUNCTION RADIANCE-CORE:PARSE-URI" "RADIANCE-CORE:PARSE-URI" "PARSE-URI") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3APARSE-URI"
   "Function radiance-core:parse-uri")
  (("FUNCTION RADIANCE-CORE:POST-VAR" "RADIANCE-CORE:POST-VAR" "POST-VAR") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3APOST-VAR"
   "Function radiance-core:post-var")
  (("FUNCTION RADIANCE-CORE:POST/GET" "RADIANCE-CORE:POST/GET" "POST/GET") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3APOST%2FGET"
   "Function radiance-core:post/get")
  (("FUNCTION RADIANCE-CORE:REDIRECT" "RADIANCE-CORE:REDIRECT" "REDIRECT") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREDIRECT"
   "Function radiance-core:redirect")
  (("FUNCTION RADIANCE-CORE:RELOAD-ENVIRONMENT" "RADIANCE-CORE:RELOAD-ENVIRONMENT" "RELOAD-ENVIRONMENT") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ARELOAD-ENVIRONMENT"
   "Function radiance-core:reload-environment")
  (("FUNCTION RADIANCE-CORE:REMMCONFIG" "RADIANCE-CORE:REMMCONFIG" "REMMCONFIG") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREMMCONFIG"
   "Function radiance-core:remmconfig")
  (("FUNCTION RADIANCE-CORE:REMOVE-API-ENDPOINT" "RADIANCE-CORE:REMOVE-API-ENDPOINT" "REMOVE-API-ENDPOINT") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREMOVE-API-ENDPOINT"
   "Function radiance-core:remove-api-endpoint")
  (("FUNCTION RADIANCE-CORE:REMOVE-API-FORMAT" "RADIANCE-CORE:REMOVE-API-FORMAT" "REMOVE-API-FORMAT") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREMOVE-API-FORMAT"
   "Function radiance-core:remove-api-format")
  (("FUNCTION RADIANCE-CORE:REMOVE-DOMAIN" "RADIANCE-CORE:REMOVE-DOMAIN" "REMOVE-DOMAIN") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREMOVE-DOMAIN"
   "Function radiance-core:remove-domain")
  (("FUNCTION RADIANCE-CORE:REMOVE-HOOK" "RADIANCE-CORE:REMOVE-HOOK" "REMOVE-HOOK") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREMOVE-HOOK"
   "Function radiance-core:remove-hook")
  (("FUNCTION RADIANCE-CORE:REMOVE-OPTION" "RADIANCE-CORE:REMOVE-OPTION" "REMOVE-OPTION") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREMOVE-OPTION"
   "Function radiance-core:remove-option")
  (("FUNCTION RADIANCE-CORE:REMOVE-PAGE" "RADIANCE-CORE:REMOVE-PAGE" "REMOVE-PAGE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREMOVE-PAGE"
   "Function radiance-core:remove-page")
  (("FUNCTION RADIANCE-CORE:REMOVE-RESOURCE-TYPE" "RADIANCE-CORE:REMOVE-RESOURCE-TYPE" "REMOVE-RESOURCE-TYPE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREMOVE-RESOURCE-TYPE"
   "Function radiance-core:remove-resource-type")
  (("FUNCTION RADIANCE-CORE:REMOVE-ROUTE" "RADIANCE-CORE:REMOVE-ROUTE" "REMOVE-ROUTE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREMOVE-ROUTE"
   "Function radiance-core:remove-route")
  (("FUNCTION RADIANCE-CORE:REMOVE-TRIGGER" "RADIANCE-CORE:REMOVE-TRIGGER" "REMOVE-TRIGGER") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREMOVE-TRIGGER"
   "Function radiance-core:remove-trigger")
  (("FUNCTION RADIANCE-CORE:REMOVE-URI-DISPATCHER" "RADIANCE-CORE:REMOVE-URI-DISPATCHER" "REMOVE-URI-DISPATCHER") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREMOVE-URI-DISPATCHER"
   "Function radiance-core:remove-uri-dispatcher")
  (("FUNCTION RADIANCE-CORE:RENDER-ERROR-PAGE" "RADIANCE-CORE:RENDER-ERROR-PAGE" "RENDER-ERROR-PAGE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ARENDER-ERROR-PAGE"
   "Function radiance-core:render-error-page")
  (("FUNCTION RADIANCE-CORE:REPRESENT-URI" "RADIANCE-CORE:REPRESENT-URI" "REPRESENT-URI") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREPRESENT-URI"
   "Function radiance-core:represent-uri")
  (("FUNCTION RADIANCE-CORE:REQUEST" "RADIANCE-CORE:REQUEST" "REQUEST") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREQUEST"
   "Function radiance-core:request")
  (("FUNCTION RADIANCE-CORE:REQUEST-RUN-TIME" "RADIANCE-CORE:REQUEST-RUN-TIME" "REQUEST-RUN-TIME") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AREQUEST-RUN-TIME"
   "Function radiance-core:request-run-time")
  (("FUNCTION RADIANCE-CORE:RESET-INTERFACE" "RADIANCE-CORE:RESET-INTERFACE" "RESET-INTERFACE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ARESET-INTERFACE"
   "Function radiance-core:reset-interface")
  (("FUNCTION RADIANCE-CORE:RESOLVE-BASE" "RADIANCE-CORE:RESOLVE-BASE" "RESOLVE-BASE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ARESOLVE-BASE"
   "Function radiance-core:resolve-base")
  (("FUNCTION RADIANCE-CORE:RESOURCE" "RADIANCE-CORE:RESOURCE" "RESOURCE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ARESOURCE"
   "Function radiance-core:resource")
  (("FUNCTION RADIANCE-CORE:SERVE-FILE" "RADIANCE-CORE:SERVE-FILE" "SERVE-FILE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ASERVE-FILE"
   "Function radiance-core:serve-file")
  (("FUNCTION RADIANCE-CORE:SHUTDOWN" "RADIANCE-CORE:SHUTDOWN" "SHUTDOWN") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ASHUTDOWN"
   "Function radiance-core:shutdown")
  (("FUNCTION RADIANCE-CORE:STARTED-P" "RADIANCE-CORE:STARTED-P" "STARTED-P") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ASTARTED-P"
   "Function radiance-core:started-p")
  (("FUNCTION RADIANCE-CORE:STARTUP" "RADIANCE-CORE:STARTUP" "STARTUP") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ASTARTUP"
   "Function radiance-core:startup")
  (("FUNCTION RADIANCE-CORE:STATIC-FILE" "RADIANCE-CORE:STATIC-FILE" "STATIC-FILE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ASTATIC-FILE"
   "Function radiance-core:static-file")
  (("FUNCTION RADIANCE-CORE:TEMPLATE-FILE" "RADIANCE-CORE:TEMPLATE-FILE" "TEMPLATE-FILE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ATEMPLATE-FILE"
   "Function radiance-core:template-file")
  (("FUNCTION RADIANCE-CORE:TRIGGER" "RADIANCE-CORE:TRIGGER" "TRIGGER") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3ATRIGGER"
   "Function radiance-core:trigger")
  (("FUNCTION RADIANCE-CORE:UNIVERSAL-TO-UNIX-TIME" "RADIANCE-CORE:UNIVERSAL-TO-UNIX-TIME" "UNIVERSAL-TO-UNIX-TIME") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AUNIVERSAL-TO-UNIX-TIME"
   "Function radiance-core:universal-to-unix-time")
  (("FUNCTION RADIANCE-CORE:UNIX-TO-UNIVERSAL-TIME" "RADIANCE-CORE:UNIX-TO-UNIVERSAL-TIME" "UNIX-TO-UNIVERSAL-TIME") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AUNIX-TO-UNIVERSAL-TIME"
   "Function radiance-core:unix-to-universal-time")
  (("FUNCTION RADIANCE-CORE:UPTIME" "RADIANCE-CORE:UPTIME" "UPTIME") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AUPTIME"
   "Function radiance-core:uptime")
  (("FUNCTION RADIANCE-CORE:URI-DISPATCHER>" "RADIANCE-CORE:URI-DISPATCHER>" "URI-DISPATCHER>") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AURI-DISPATCHER%3E"
   "Function radiance-core:uri-dispatcher>")
  (("FUNCTION RADIANCE-CORE:URI-MATCHES" "RADIANCE-CORE:URI-MATCHES" "URI-MATCHES") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AURI-MATCHES"
   "Function radiance-core:uri-matches")
  (("FUNCTION RADIANCE-CORE:URI-STRING" "RADIANCE-CORE:URI-STRING" "URI-STRING") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AURI-STRING"
   "Function radiance-core:uri-string")
  (("FUNCTION RADIANCE-CORE:URI-TO-URL" "RADIANCE-CORE:URI-TO-URL" "URI-TO-URL") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AURI-TO-URL"
   "Function radiance-core:uri-to-url")
  (("FUNCTION RADIANCE-CORE:URI<" "RADIANCE-CORE:URI<" "URI<") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AURI%3C"
   "Function radiance-core:uri<")
  (("FUNCTION RADIANCE-CORE:URI=" "RADIANCE-CORE:URI=" "URI=") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AURI%3D"
   "Function radiance-core:uri=")
  (("FUNCTION RADIANCE-CORE:URI>" "RADIANCE-CORE:URI>" "URI>") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AURI%3E"
   "Function radiance-core:uri>")
  (("FUNCTION RADIANCE-CORE:URL-ENCODE" "RADIANCE-CORE:URL-ENCODE" "URL-ENCODE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AURL-ENCODE"
   "Function radiance-core:url-encode")
  (("FUNCTION RADIANCE-CORE:VIRTUAL-MODULE" "RADIANCE-CORE:VIRTUAL-MODULE" "VIRTUAL-MODULE") "https://shirakumo.github.io/radiance#FUNCTION%20RADIANCE-CORE%3AVIRTUAL-MODULE"
   "Function radiance-core:virtual-module")
  (("URI-DISPATCHER RADIANCE-CORE:API" "RADIANCE-CORE:API" "API") "https://shirakumo.github.io/radiance#URI-DISPATCHER%20RADIANCE-CORE%3AAPI"
   "Uri-dispatcher radiance-core:api")
  (("URI-DISPATCHER RADIANCE-CORE:FAVICON" "RADIANCE-CORE:FAVICON" "FAVICON") "https://shirakumo.github.io/radiance#URI-DISPATCHER%20RADIANCE-CORE%3AFAVICON"
   "Uri-dispatcher radiance-core:favicon")
  (("URI-DISPATCHER RADIANCE-CORE:ROBOTS" "RADIANCE-CORE:ROBOTS" "ROBOTS") "https://shirakumo.github.io/radiance#URI-DISPATCHER%20RADIANCE-CORE%3AROBOTS"
   "Uri-dispatcher radiance-core:robots")
  (("URI-DISPATCHER RADIANCE-CORE:STATIC" "RADIANCE-CORE:STATIC" "STATIC") "https://shirakumo.github.io/radiance#URI-DISPATCHER%20RADIANCE-CORE%3ASTATIC"
   "Uri-dispatcher radiance-core:static")
  (("GENERIC RADIANCE-CORE:API-SERIALIZE" "RADIANCE-CORE:API-SERIALIZE" "API-SERIALIZE") "https://shirakumo.github.io/radiance#GENERIC%20RADIANCE-CORE%3AAPI-SERIALIZE"
   "Generic radiance-core:api-serialize")
  (("MACRO RADIANCE-CORE:@STATIC" "RADIANCE-CORE:@STATIC" "@STATIC") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3A%40STATIC"
   "Macro radiance-core:@static")
  (("MACRO RADIANCE-CORE:@TEMPLATE" "RADIANCE-CORE:@TEMPLATE" "@TEMPLATE") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3A%40TEMPLATE"
   "Macro radiance-core:@template")
  (("MACRO RADIANCE-CORE:API-ERROR" "RADIANCE-CORE:API-ERROR" "API-ERROR") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3AAPI-ERROR"
   "Macro radiance-core:api-error")
  (("MACRO RADIANCE-CORE:CONFIG" "RADIANCE-CORE:CONFIG" "CONFIG") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ACONFIG"
   "Macro radiance-core:config")
  (("MACRO RADIANCE-CORE:CURRENT-MODULE" "RADIANCE-CORE:CURRENT-MODULE" "CURRENT-MODULE") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ACURRENT-MODULE"
   "Macro radiance-core:current-module")
  (("MACRO RADIANCE-CORE:DEFAULTED-CONFIG" "RADIANCE-CORE:DEFAULTED-CONFIG" "DEFAULTED-CONFIG") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFAULTED-CONFIG"
   "Macro radiance-core:defaulted-config")
  (("MACRO RADIANCE-CORE:DEFINE-API" "RADIANCE-CORE:DEFINE-API" "DEFINE-API") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-API"
   "Macro radiance-core:define-api")
  (("MACRO RADIANCE-CORE:DEFINE-API-FORMAT" "RADIANCE-CORE:DEFINE-API-FORMAT" "DEFINE-API-FORMAT") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-API-FORMAT"
   "Macro radiance-core:define-api-format")
  (("MACRO RADIANCE-CORE:DEFINE-DOCUMENTABLE" "RADIANCE-CORE:DEFINE-DOCUMENTABLE" "DEFINE-DOCUMENTABLE") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-DOCUMENTABLE"
   "Macro radiance-core:define-documentable")
  (("MACRO RADIANCE-CORE:DEFINE-HOOK" "RADIANCE-CORE:DEFINE-HOOK" "DEFINE-HOOK") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-HOOK"
   "Macro radiance-core:define-hook")
  (("MACRO RADIANCE-CORE:DEFINE-HOOK-SWITCH" "RADIANCE-CORE:DEFINE-HOOK-SWITCH" "DEFINE-HOOK-SWITCH") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-HOOK-SWITCH"
   "Macro radiance-core:define-hook-switch")
  (("MACRO RADIANCE-CORE:DEFINE-IMPLEMENT-TRIGGER" "RADIANCE-CORE:DEFINE-IMPLEMENT-TRIGGER" "DEFINE-IMPLEMENT-TRIGGER") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-IMPLEMENT-TRIGGER"
   "Macro radiance-core:define-implement-trigger")
  (("MACRO RADIANCE-CORE:DEFINE-INTERFACE" "RADIANCE-CORE:DEFINE-INTERFACE" "DEFINE-INTERFACE") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-INTERFACE"
   "Macro radiance-core:define-interface")
  (("MACRO RADIANCE-CORE:DEFINE-INTERFACE-EXTENSION" "RADIANCE-CORE:DEFINE-INTERFACE-EXTENSION" "DEFINE-INTERFACE-EXTENSION") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-INTERFACE-EXTENSION"
   "Macro radiance-core:define-interface-extension")
  (("MACRO RADIANCE-CORE:DEFINE-MATCHING-ROUTE" "RADIANCE-CORE:DEFINE-MATCHING-ROUTE" "DEFINE-MATCHING-ROUTE") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-MATCHING-ROUTE"
   "Macro radiance-core:define-matching-route")
  (("MACRO RADIANCE-CORE:DEFINE-MODULE" "RADIANCE-CORE:DEFINE-MODULE" "DEFINE-MODULE") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-MODULE"
   "Macro radiance-core:define-module")
  (("MACRO RADIANCE-CORE:DEFINE-MODULE-EXTENSION" "RADIANCE-CORE:DEFINE-MODULE-EXTENSION" "DEFINE-MODULE-EXTENSION") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-MODULE-EXTENSION"
   "Macro radiance-core:define-module-extension")
  (("MACRO RADIANCE-CORE:DEFINE-OPTION" "RADIANCE-CORE:DEFINE-OPTION" "DEFINE-OPTION") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-OPTION"
   "Macro radiance-core:define-option")
  (("MACRO RADIANCE-CORE:DEFINE-PAGE" "RADIANCE-CORE:DEFINE-PAGE" "DEFINE-PAGE") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-PAGE"
   "Macro radiance-core:define-page")
  (("MACRO RADIANCE-CORE:DEFINE-RESOURCE-LOCATOR" "RADIANCE-CORE:DEFINE-RESOURCE-LOCATOR" "DEFINE-RESOURCE-LOCATOR") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-RESOURCE-LOCATOR"
   "Macro radiance-core:define-resource-locator")
  (("MACRO RADIANCE-CORE:DEFINE-RESOURCE-TYPE" "RADIANCE-CORE:DEFINE-RESOURCE-TYPE" "DEFINE-RESOURCE-TYPE") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-RESOURCE-TYPE"
   "Macro radiance-core:define-resource-type")
  (("MACRO RADIANCE-CORE:DEFINE-ROUTE" "RADIANCE-CORE:DEFINE-ROUTE" "DEFINE-ROUTE") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-ROUTE"
   "Macro radiance-core:define-route")
  (("MACRO RADIANCE-CORE:DEFINE-STRING-ROUTE" "RADIANCE-CORE:DEFINE-STRING-ROUTE" "DEFINE-STRING-ROUTE") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-STRING-ROUTE"
   "Macro radiance-core:define-string-route")
  (("MACRO RADIANCE-CORE:DEFINE-TARGET-ROUTE" "RADIANCE-CORE:DEFINE-TARGET-ROUTE" "DEFINE-TARGET-ROUTE") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-TARGET-ROUTE"
   "Macro radiance-core:define-target-route")
  (("MACRO RADIANCE-CORE:DEFINE-TRIGGER" "RADIANCE-CORE:DEFINE-TRIGGER" "DEFINE-TRIGGER") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-TRIGGER"
   "Macro radiance-core:define-trigger")
  (("MACRO RADIANCE-CORE:DEFINE-URI-DISPATCHER" "RADIANCE-CORE:DEFINE-URI-DISPATCHER" "DEFINE-URI-DISPATCHER") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3ADEFINE-URI-DISPATCHER"
   "Macro radiance-core:define-uri-dispatcher")
  (("MACRO RADIANCE-CORE:OR*" "RADIANCE-CORE:OR*" "OR*") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3AOR%2A"
   "Macro radiance-core:or*")
  (("MACRO RADIANCE-CORE:PERM" "RADIANCE-CORE:PERM" "PERM") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3APERM"
   "Macro radiance-core:perm")
  (("MACRO RADIANCE-CORE:REMCONFIG" "RADIANCE-CORE:REMCONFIG" "REMCONFIG") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3AREMCONFIG"
   "Macro radiance-core:remconfig")
  (("MACRO RADIANCE-CORE:WITH-ACTIONS" "RADIANCE-CORE:WITH-ACTIONS" "WITH-ACTIONS") "https://shirakumo.github.io/radiance#MACRO%20RADIANCE-CORE%3AWITH-ACTIONS"
   "Macro radiance-core:with-actions")
  (("SPECIAL ADMIN:*IMPLEMENTATION*" "ADMIN:*IMPLEMENTATION*" "*IMPLEMENTATION*") "https://shirakumo.github.io/radiance#SPECIAL%20ADMIN%3A%2AIMPLEMENTATION%2A"
   "Special admin:*implementation*")
  (("RESOURCE-TYPE ADMIN:PAGE" "ADMIN:PAGE" "PAGE") "https://shirakumo.github.io/radiance#RESOURCE-TYPE%20ADMIN%3APAGE"
   "Resource-type admin:page")
  (("option PANEL ADMIN:ACCESS" "PANEL ADMIN:ACCESS" "ACCESS") "https://shirakumo.github.io/radiance#option%20PANEL%20ADMIN%3AACCESS"
   "Option panel admin:access")
  (("HOOK ADMIN:IMPLEMENTED" "ADMIN:IMPLEMENTED" "IMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20ADMIN%3AIMPLEMENTED"
   "Hook admin:implemented")
  (("HOOK ADMIN:UNIMPLEMENTED" "ADMIN:UNIMPLEMENTED" "UNIMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20ADMIN%3AUNIMPLEMENTED"
   "Hook admin:unimplemented")
  (("ACCESSOR ADMIN:IMPLEMENTATION" "ADMIN:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20ADMIN%3AIMPLEMENTATION"
   "Accessor admin:implementation")
  (("FUNCTION ADMIN:LIST-PANELS" "ADMIN:LIST-PANELS" "LIST-PANELS") "https://shirakumo.github.io/radiance#FUNCTION%20ADMIN%3ALIST-PANELS"
   "Function admin:list-panels")
  (("FUNCTION ADMIN:REMOVE-PANEL" "ADMIN:REMOVE-PANEL" "REMOVE-PANEL") "https://shirakumo.github.io/radiance#FUNCTION%20ADMIN%3AREMOVE-PANEL"
   "Function admin:remove-panel")
  (("MACRO ADMIN:DEFINE-PANEL" "ADMIN:DEFINE-PANEL" "DEFINE-PANEL") "https://shirakumo.github.io/radiance#MACRO%20ADMIN%3ADEFINE-PANEL"
   "Macro admin:define-panel")
  (("SPECIAL AUTH:*IMPLEMENTATION*" "AUTH:*IMPLEMENTATION*" "*IMPLEMENTATION*") "https://shirakumo.github.io/radiance#SPECIAL%20AUTH%3A%2AIMPLEMENTATION%2A"
   "Special auth:*implementation*")
  (("SPECIAL AUTH:*LOGIN-TIMEOUT*" "AUTH:*LOGIN-TIMEOUT*" "*LOGIN-TIMEOUT*") "https://shirakumo.github.io/radiance#SPECIAL%20AUTH%3A%2ALOGIN-TIMEOUT%2A"
   "Special auth:*login-timeout*")
  (("RESOURCE-TYPE AUTH:PAGE" "AUTH:PAGE" "PAGE") "https://shirakumo.github.io/radiance#RESOURCE-TYPE%20AUTH%3APAGE"
   "Resource-type auth:page")
  (("HOOK AUTH:ASSOCIATE" "AUTH:ASSOCIATE" "ASSOCIATE") "https://shirakumo.github.io/radiance#HOOK%20AUTH%3AASSOCIATE"
   "Hook auth:associate")
  (("HOOK AUTH:IMPLEMENTED" "AUTH:IMPLEMENTED" "IMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20AUTH%3AIMPLEMENTED"
   "Hook auth:implemented")
  (("HOOK AUTH:UNIMPLEMENTED" "AUTH:UNIMPLEMENTED" "UNIMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20AUTH%3AUNIMPLEMENTED"
   "Hook auth:unimplemented")
  (("ACCESSOR AUTH:IMPLEMENTATION" "AUTH:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20AUTH%3AIMPLEMENTATION"
   "Accessor auth:implementation")
  (("FUNCTION AUTH:ASSOCIATE" "AUTH:ASSOCIATE" "ASSOCIATE") "https://shirakumo.github.io/radiance#FUNCTION%20AUTH%3AASSOCIATE"
   "Function auth:associate")
  (("FUNCTION AUTH:CURRENT" "AUTH:CURRENT" "CURRENT") "https://shirakumo.github.io/radiance#FUNCTION%20AUTH%3ACURRENT"
   "Function auth:current")
  (("SPECIAL BAN:*IMPLEMENTATION*" "BAN:*IMPLEMENTATION*" "*IMPLEMENTATION*") "https://shirakumo.github.io/radiance#SPECIAL%20BAN%3A%2AIMPLEMENTATION%2A"
   "Special ban:*implementation*")
  (("HOOK BAN:IMPLEMENTED" "BAN:IMPLEMENTED" "IMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20BAN%3AIMPLEMENTED"
   "Hook ban:implemented")
  (("HOOK BAN:UNIMPLEMENTED" "BAN:UNIMPLEMENTED" "UNIMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20BAN%3AUNIMPLEMENTED"
   "Hook ban:unimplemented")
  (("ACCESSOR BAN:IMPLEMENTATION" "BAN:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20BAN%3AIMPLEMENTATION"
   "Accessor ban:implementation")
  (("FUNCTION BAN:JAIL" "BAN:JAIL" "JAIL") "https://shirakumo.github.io/radiance#FUNCTION%20BAN%3AJAIL"
   "Function ban:jail")
  (("FUNCTION BAN:JAIL-TIME" "BAN:JAIL-TIME" "JAIL-TIME") "https://shirakumo.github.io/radiance#FUNCTION%20BAN%3AJAIL-TIME"
   "Function ban:jail-time")
  (("FUNCTION BAN:LIST" "BAN:LIST" "LIST") "https://shirakumo.github.io/radiance#FUNCTION%20BAN%3ALIST"
   "Function ban:list")
  (("FUNCTION BAN:RELEASE" "BAN:RELEASE" "RELEASE") "https://shirakumo.github.io/radiance#FUNCTION%20BAN%3ARELEASE"
   "Function ban:release")
  (("SPECIAL CACHE:*IMPLEMENTATION*" "CACHE:*IMPLEMENTATION*" "*IMPLEMENTATION*") "https://shirakumo.github.io/radiance#SPECIAL%20CACHE%3A%2AIMPLEMENTATION%2A"
   "Special cache:*implementation*")
  (("HOOK CACHE:IMPLEMENTED" "CACHE:IMPLEMENTED" "IMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20CACHE%3AIMPLEMENTED"
   "Hook cache:implemented")
  (("HOOK CACHE:UNIMPLEMENTED" "CACHE:UNIMPLEMENTED" "UNIMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20CACHE%3AUNIMPLEMENTED"
   "Hook cache:unimplemented")
  (("ACCESSOR CACHE:IMPLEMENTATION" "CACHE:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20CACHE%3AIMPLEMENTATION"
   "Accessor cache:implementation")
  (("FUNCTION CACHE:GET" "CACHE:GET" "GET") "https://shirakumo.github.io/radiance#FUNCTION%20CACHE%3AGET"
   "Function cache:get")
  (("FUNCTION CACHE:RENEW" "CACHE:RENEW" "RENEW") "https://shirakumo.github.io/radiance#FUNCTION%20CACHE%3ARENEW"
   "Function cache:renew")
  (("MACRO CACHE:WITH-CACHE" "CACHE:WITH-CACHE" "WITH-CACHE") "https://shirakumo.github.io/radiance#MACRO%20CACHE%3AWITH-CACHE"
   "Macro cache:with-cache")
  (("SPECIAL DATABASE:*IMPLEMENTATION*" "DATABASE:*IMPLEMENTATION*" "*IMPLEMENTATION*") "https://shirakumo.github.io/radiance#SPECIAL%20DATABASE%3A%2AIMPLEMENTATION%2A"
   "Special database:*implementation*")
  (("CONDITION DATABASE:COLLECTION-ALREADY-EXISTS" "DATABASE:COLLECTION-ALREADY-EXISTS" "COLLECTION-ALREADY-EXISTS") "https://shirakumo.github.io/radiance#CONDITION%20DATABASE%3ACOLLECTION-ALREADY-EXISTS"
   "Condition database:collection-already-exists")
  (("CONDITION DATABASE:COLLECTION-CONDITION" "DATABASE:COLLECTION-CONDITION" "COLLECTION-CONDITION") "https://shirakumo.github.io/radiance#CONDITION%20DATABASE%3ACOLLECTION-CONDITION"
   "Condition database:collection-condition")
  (("CONDITION DATABASE:CONDITION" "DATABASE:CONDITION" "CONDITION") "https://shirakumo.github.io/radiance#CONDITION%20DATABASE%3ACONDITION"
   "Condition database:condition")
  (("CONDITION DATABASE:CONNECTION-ALREADY-OPEN" "DATABASE:CONNECTION-ALREADY-OPEN" "CONNECTION-ALREADY-OPEN") "https://shirakumo.github.io/radiance#CONDITION%20DATABASE%3ACONNECTION-ALREADY-OPEN"
   "Condition database:connection-already-open")
  (("CONDITION DATABASE:CONNECTION-FAILED" "DATABASE:CONNECTION-FAILED" "CONNECTION-FAILED") "https://shirakumo.github.io/radiance#CONDITION%20DATABASE%3ACONNECTION-FAILED"
   "Condition database:connection-failed")
  (("CONDITION DATABASE:ID" "DATABASE:ID" "ID") "https://shirakumo.github.io/radiance#CONDITION%20DATABASE%3AID"
   "Condition database:id")
  (("CONDITION DATABASE:INVALID-COLLECTION" "DATABASE:INVALID-COLLECTION" "INVALID-COLLECTION") "https://shirakumo.github.io/radiance#CONDITION%20DATABASE%3AINVALID-COLLECTION"
   "Condition database:invalid-collection")
  (("CONDITION DATABASE:INVALID-FIELD" "DATABASE:INVALID-FIELD" "INVALID-FIELD") "https://shirakumo.github.io/radiance#CONDITION%20DATABASE%3AINVALID-FIELD"
   "Condition database:invalid-field")
  (("STRUCTURE DATABASE:ID" "DATABASE:ID" "ID") "https://shirakumo.github.io/radiance#STRUCTURE%20DATABASE%3AID"
   "Structure database:id")
  (("HOOK DATABASE:CONNECTED" "DATABASE:CONNECTED" "CONNECTED") "https://shirakumo.github.io/radiance#HOOK%20DATABASE%3ACONNECTED"
   "Hook database:connected")
  (("HOOK DATABASE:DISCONNECTED" "DATABASE:DISCONNECTED" "DISCONNECTED") "https://shirakumo.github.io/radiance#HOOK%20DATABASE%3ADISCONNECTED"
   "Hook database:disconnected")
  (("HOOK DATABASE:IMPLEMENTED" "DATABASE:IMPLEMENTED" "IMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20DATABASE%3AIMPLEMENTED"
   "Hook database:implemented")
  (("HOOK DATABASE:UNIMPLEMENTED" "DATABASE:UNIMPLEMENTED" "UNIMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20DATABASE%3AUNIMPLEMENTED"
   "Hook database:unimplemented")
  (("ACCESSOR DATABASE:IMPLEMENTATION" "DATABASE:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20DATABASE%3AIMPLEMENTATION"
   "Accessor database:implementation")
  (("FUNCTION DATABASE:COLLECTION-EXISTS-P" "DATABASE:COLLECTION-EXISTS-P" "COLLECTION-EXISTS-P") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3ACOLLECTION-EXISTS-P"
   "Function database:collection-exists-p")
  (("FUNCTION DATABASE:COLLECTIONS" "DATABASE:COLLECTIONS" "COLLECTIONS") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3ACOLLECTIONS"
   "Function database:collections")
  (("FUNCTION DATABASE:CONNECT" "DATABASE:CONNECT" "CONNECT") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3ACONNECT"
   "Function database:connect")
  (("FUNCTION DATABASE:CONNECTED-P" "DATABASE:CONNECTED-P" "CONNECTED-P") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3ACONNECTED-P"
   "Function database:connected-p")
  (("FUNCTION DATABASE:COUNT" "DATABASE:COUNT" "COUNT") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3ACOUNT"
   "Function database:count")
  (("FUNCTION DATABASE:CREATE" "DATABASE:CREATE" "CREATE") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3ACREATE"
   "Function database:create")
  (("FUNCTION DATABASE:DISCONNECT" "DATABASE:DISCONNECT" "DISCONNECT") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3ADISCONNECT"
   "Function database:disconnect")
  (("FUNCTION DATABASE:DROP" "DATABASE:DROP" "DROP") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3ADROP"
   "Function database:drop")
  (("FUNCTION DATABASE:EMPTY" "DATABASE:EMPTY" "EMPTY") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3AEMPTY"
   "Function database:empty")
  (("FUNCTION DATABASE:ENSURE-ID" "DATABASE:ENSURE-ID" "ENSURE-ID") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3AENSURE-ID"
   "Function database:ensure-id")
  (("FUNCTION DATABASE:INSERT" "DATABASE:INSERT" "INSERT") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3AINSERT"
   "Function database:insert")
  (("FUNCTION DATABASE:ITERATE" "DATABASE:ITERATE" "ITERATE") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3AITERATE"
   "Function database:iterate")
  (("FUNCTION DATABASE:REMOVE" "DATABASE:REMOVE" "REMOVE") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3AREMOVE"
   "Function database:remove")
  (("FUNCTION DATABASE:SELECT" "DATABASE:SELECT" "SELECT") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3ASELECT"
   "Function database:select")
  (("FUNCTION DATABASE:STRUCTURE" "DATABASE:STRUCTURE" "STRUCTURE") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3ASTRUCTURE"
   "Function database:structure")
  (("FUNCTION DATABASE:UPDATE" "DATABASE:UPDATE" "UPDATE") "https://shirakumo.github.io/radiance#FUNCTION%20DATABASE%3AUPDATE"
   "Function database:update")
  (("MACRO DATABASE:QUERY" "DATABASE:QUERY" "QUERY") "https://shirakumo.github.io/radiance#MACRO%20DATABASE%3AQUERY"
   "Macro database:query")
  (("MACRO DATABASE:WITH-TRANSACTION" "DATABASE:WITH-TRANSACTION" "WITH-TRANSACTION") "https://shirakumo.github.io/radiance#MACRO%20DATABASE%3AWITH-TRANSACTION"
   "Macro database:with-transaction")
  (("SPECIAL LOGGER:*IMPLEMENTATION*" "LOGGER:*IMPLEMENTATION*" "*IMPLEMENTATION*") "https://shirakumo.github.io/radiance#SPECIAL%20LOGGER%3A%2AIMPLEMENTATION%2A"
   "Special logger:*implementation*")
  (("HOOK LOGGER:IMPLEMENTED" "LOGGER:IMPLEMENTED" "IMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20LOGGER%3AIMPLEMENTED"
   "Hook logger:implemented")
  (("HOOK LOGGER:UNIMPLEMENTED" "LOGGER:UNIMPLEMENTED" "UNIMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20LOGGER%3AUNIMPLEMENTED"
   "Hook logger:unimplemented")
  (("ACCESSOR LOGGER:IMPLEMENTATION" "LOGGER:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20LOGGER%3AIMPLEMENTATION"
   "Accessor logger:implementation")
  (("FUNCTION LOGGER:DEBUG" "LOGGER:DEBUG" "DEBUG") "https://shirakumo.github.io/radiance#FUNCTION%20LOGGER%3ADEBUG"
   "Function logger:debug")
  (("FUNCTION LOGGER:ERROR" "LOGGER:ERROR" "ERROR") "https://shirakumo.github.io/radiance#FUNCTION%20LOGGER%3AERROR"
   "Function logger:error")
  (("FUNCTION LOGGER:FATAL" "LOGGER:FATAL" "FATAL") "https://shirakumo.github.io/radiance#FUNCTION%20LOGGER%3AFATAL"
   "Function logger:fatal")
  (("FUNCTION LOGGER:INFO" "LOGGER:INFO" "INFO") "https://shirakumo.github.io/radiance#FUNCTION%20LOGGER%3AINFO"
   "Function logger:info")
  (("FUNCTION LOGGER:LOG" "LOGGER:LOG" "LOG") "https://shirakumo.github.io/radiance#FUNCTION%20LOGGER%3ALOG"
   "Function logger:log")
  (("FUNCTION LOGGER:SEVERE" "LOGGER:SEVERE" "SEVERE") "https://shirakumo.github.io/radiance#FUNCTION%20LOGGER%3ASEVERE"
   "Function logger:severe")
  (("FUNCTION LOGGER:TRACE" "LOGGER:TRACE" "TRACE") "https://shirakumo.github.io/radiance#FUNCTION%20LOGGER%3ATRACE"
   "Function logger:trace")
  (("FUNCTION LOGGER:WARN" "LOGGER:WARN" "WARN") "https://shirakumo.github.io/radiance#FUNCTION%20LOGGER%3AWARN"
   "Function logger:warn")
  (("SPECIAL MAIL:*IMPLEMENTATION*" "MAIL:*IMPLEMENTATION*" "*IMPLEMENTATION*") "https://shirakumo.github.io/radiance#SPECIAL%20MAIL%3A%2AIMPLEMENTATION%2A"
   "Special mail:*implementation*")
  (("HOOK MAIL:IMPLEMENTED" "MAIL:IMPLEMENTED" "IMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20MAIL%3AIMPLEMENTED"
   "Hook mail:implemented")
  (("HOOK MAIL:SEND" "MAIL:SEND" "SEND") "https://shirakumo.github.io/radiance#HOOK%20MAIL%3ASEND"
   "Hook mail:send")
  (("HOOK MAIL:UNIMPLEMENTED" "MAIL:UNIMPLEMENTED" "UNIMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20MAIL%3AUNIMPLEMENTED"
   "Hook mail:unimplemented")
  (("ACCESSOR MAIL:IMPLEMENTATION" "MAIL:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20MAIL%3AIMPLEMENTATION"
   "Accessor mail:implementation")
  (("FUNCTION MAIL:SEND" "MAIL:SEND" "SEND") "https://shirakumo.github.io/radiance#FUNCTION%20MAIL%3ASEND"
   "Function mail:send")
  (("SPECIAL PROFILE:*IMPLEMENTATION*" "PROFILE:*IMPLEMENTATION*" "*IMPLEMENTATION*") "https://shirakumo.github.io/radiance#SPECIAL%20PROFILE%3A%2AIMPLEMENTATION%2A"
   "Special profile:*implementation*")
  (("RESOURCE-TYPE PROFILE:PAGE" "PROFILE:PAGE" "PAGE") "https://shirakumo.github.io/radiance#RESOURCE-TYPE%20PROFILE%3APAGE"
   "Resource-type profile:page")
  (("option PANEL PROFILE:ACCESS" "PANEL PROFILE:ACCESS" "ACCESS") "https://shirakumo.github.io/radiance#option%20PANEL%20PROFILE%3AACCESS"
   "Option panel profile:access")
  (("HOOK PROFILE:IMPLEMENTED" "PROFILE:IMPLEMENTED" "IMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20PROFILE%3AIMPLEMENTED"
   "Hook profile:implemented")
  (("HOOK PROFILE:UNIMPLEMENTED" "PROFILE:UNIMPLEMENTED" "UNIMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20PROFILE%3AUNIMPLEMENTED"
   "Hook profile:unimplemented")
  (("ACCESSOR PROFILE:IMPLEMENTATION" "PROFILE:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20PROFILE%3AIMPLEMENTATION"
   "Accessor profile:implementation")
  (("FUNCTION PROFILE:ADD-FIELD" "PROFILE:ADD-FIELD" "ADD-FIELD") "https://shirakumo.github.io/radiance#FUNCTION%20PROFILE%3AADD-FIELD"
   "Function profile:add-field")
  (("FUNCTION PROFILE:AVATAR" "PROFILE:AVATAR" "AVATAR") "https://shirakumo.github.io/radiance#FUNCTION%20PROFILE%3AAVATAR"
   "Function profile:avatar")
  (("FUNCTION PROFILE:FIELDS" "PROFILE:FIELDS" "FIELDS") "https://shirakumo.github.io/radiance#FUNCTION%20PROFILE%3AFIELDS"
   "Function profile:fields")
  (("FUNCTION PROFILE:LIST-PANELS" "PROFILE:LIST-PANELS" "LIST-PANELS") "https://shirakumo.github.io/radiance#FUNCTION%20PROFILE%3ALIST-PANELS"
   "Function profile:list-panels")
  (("FUNCTION PROFILE:NAME" "PROFILE:NAME" "NAME") "https://shirakumo.github.io/radiance#FUNCTION%20PROFILE%3ANAME"
   "Function profile:name")
  (("FUNCTION PROFILE:REMOVE-FIELD" "PROFILE:REMOVE-FIELD" "REMOVE-FIELD") "https://shirakumo.github.io/radiance#FUNCTION%20PROFILE%3AREMOVE-FIELD"
   "Function profile:remove-field")
  (("FUNCTION PROFILE:REMOVE-PANEL" "PROFILE:REMOVE-PANEL" "REMOVE-PANEL") "https://shirakumo.github.io/radiance#FUNCTION%20PROFILE%3AREMOVE-PANEL"
   "Function profile:remove-panel")
  (("MACRO PROFILE:DEFINE-PANEL" "PROFILE:DEFINE-PANEL" "DEFINE-PANEL") "https://shirakumo.github.io/radiance#MACRO%20PROFILE%3ADEFINE-PANEL"
   "Macro profile:define-panel")
  (("SPECIAL RATE:*IMPLEMENTATION*" "RATE:*IMPLEMENTATION*" "*IMPLEMENTATION*") "https://shirakumo.github.io/radiance#SPECIAL%20RATE%3A%2AIMPLEMENTATION%2A"
   "Special rate:*implementation*")
  (("HOOK RATE:IMPLEMENTED" "RATE:IMPLEMENTED" "IMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20RATE%3AIMPLEMENTED"
   "Hook rate:implemented")
  (("HOOK RATE:UNIMPLEMENTED" "RATE:UNIMPLEMENTED" "UNIMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20RATE%3AUNIMPLEMENTED"
   "Hook rate:unimplemented")
  (("ACCESSOR RATE:IMPLEMENTATION" "RATE:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20RATE%3AIMPLEMENTATION"
   "Accessor rate:implementation")
  (("FUNCTION RATE:LEFT" "RATE:LEFT" "LEFT") "https://shirakumo.github.io/radiance#FUNCTION%20RATE%3ALEFT"
   "Function rate:left")
  (("MACRO RATE:DEFINE-LIMIT" "RATE:DEFINE-LIMIT" "DEFINE-LIMIT") "https://shirakumo.github.io/radiance#MACRO%20RATE%3ADEFINE-LIMIT"
   "Macro rate:define-limit")
  (("MACRO RATE:WITH-LIMITATION" "RATE:WITH-LIMITATION" "WITH-LIMITATION") "https://shirakumo.github.io/radiance#MACRO%20RATE%3AWITH-LIMITATION"
   "Macro rate:with-limitation")
  (("SPECIAL SERVER:*IMPLEMENTATION*" "SERVER:*IMPLEMENTATION*" "*IMPLEMENTATION*") "https://shirakumo.github.io/radiance#SPECIAL%20SERVER%3A%2AIMPLEMENTATION%2A"
   "Special server:*implementation*")
  (("HOOK SERVER:IMPLEMENTED" "SERVER:IMPLEMENTED" "IMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20SERVER%3AIMPLEMENTED"
   "Hook server:implemented")
  (("HOOK SERVER:STARTED" "SERVER:STARTED" "STARTED") "https://shirakumo.github.io/radiance#HOOK%20SERVER%3ASTARTED"
   "Hook server:started")
  (("HOOK SERVER:STOPPED" "SERVER:STOPPED" "STOPPED") "https://shirakumo.github.io/radiance#HOOK%20SERVER%3ASTOPPED"
   "Hook server:stopped")
  (("HOOK SERVER:UNIMPLEMENTED" "SERVER:UNIMPLEMENTED" "UNIMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20SERVER%3AUNIMPLEMENTED"
   "Hook server:unimplemented")
  (("ACCESSOR SERVER:IMPLEMENTATION" "SERVER:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20SERVER%3AIMPLEMENTATION"
   "Accessor server:implementation")
  (("FUNCTION SERVER:LISTENERS" "SERVER:LISTENERS" "LISTENERS") "https://shirakumo.github.io/radiance#FUNCTION%20SERVER%3ALISTENERS"
   "Function server:listeners")
  (("FUNCTION SERVER:START" "SERVER:START" "START") "https://shirakumo.github.io/radiance#FUNCTION%20SERVER%3ASTART"
   "Function server:start")
  (("FUNCTION SERVER:STOP" "SERVER:STOP" "STOP") "https://shirakumo.github.io/radiance#FUNCTION%20SERVER%3ASTOP"
   "Function server:stop")
  (("SPECIAL SESSION:*DEFAULT-TIMEOUT*" "SESSION:*DEFAULT-TIMEOUT*" "*DEFAULT-TIMEOUT*") "https://shirakumo.github.io/radiance#SPECIAL%20SESSION%3A%2ADEFAULT-TIMEOUT%2A"
   "Special session:*default-timeout*")
  (("SPECIAL SESSION:*IMPLEMENTATION*" "SESSION:*IMPLEMENTATION*" "*IMPLEMENTATION*") "https://shirakumo.github.io/radiance#SPECIAL%20SESSION%3A%2AIMPLEMENTATION%2A"
   "Special session:*implementation*")
  (("CLASS SESSION:SESSION" "SESSION:SESSION" "SESSION") "https://shirakumo.github.io/radiance#CLASS%20SESSION%3ASESSION"
   "Class session:session")
  (("HOOK SESSION:CREATE" "SESSION:CREATE" "CREATE") "https://shirakumo.github.io/radiance#HOOK%20SESSION%3ACREATE"
   "Hook session:create")
  (("HOOK SESSION:IMPLEMENTED" "SESSION:IMPLEMENTED" "IMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20SESSION%3AIMPLEMENTED"
   "Hook session:implemented")
  (("HOOK SESSION:UNIMPLEMENTED" "SESSION:UNIMPLEMENTED" "UNIMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20SESSION%3AUNIMPLEMENTED"
   "Hook session:unimplemented")
  (("ACCESSOR SESSION:FIELD" "SESSION:FIELD" "FIELD") "https://shirakumo.github.io/radiance#ACCESSOR%20SESSION%3AFIELD"
   "Accessor session:field")
  (("ACCESSOR SESSION:IMPLEMENTATION" "SESSION:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20SESSION%3AIMPLEMENTATION"
   "Accessor session:implementation")
  (("ACCESSOR SESSION:TIMEOUT" "SESSION:TIMEOUT" "TIMEOUT") "https://shirakumo.github.io/radiance#ACCESSOR%20SESSION%3ATIMEOUT"
   "Accessor session:timeout")
  (("FUNCTION SESSION:=" "SESSION:=" "=") "https://shirakumo.github.io/radiance#FUNCTION%20SESSION%3A%3D"
   "Function session:=")
  (("FUNCTION SESSION:ACTIVE-P" "SESSION:ACTIVE-P" "ACTIVE-P") "https://shirakumo.github.io/radiance#FUNCTION%20SESSION%3AACTIVE-P"
   "Function session:active-p")
  (("FUNCTION SESSION:END" "SESSION:END" "END") "https://shirakumo.github.io/radiance#FUNCTION%20SESSION%3AEND"
   "Function session:end")
  (("FUNCTION SESSION:GET" "SESSION:GET" "GET") "https://shirakumo.github.io/radiance#FUNCTION%20SESSION%3AGET"
   "Function session:get")
  (("FUNCTION SESSION:ID" "SESSION:ID" "ID") "https://shirakumo.github.io/radiance#FUNCTION%20SESSION%3AID"
   "Function session:id")
  (("FUNCTION SESSION:LIST" "SESSION:LIST" "LIST") "https://shirakumo.github.io/radiance#FUNCTION%20SESSION%3ALIST"
   "Function session:list")
  (("FUNCTION SESSION:START" "SESSION:START" "START") "https://shirakumo.github.io/radiance#FUNCTION%20SESSION%3ASTART"
   "Function session:start")
  (("SPECIAL USER:*IMPLEMENTATION*" "USER:*IMPLEMENTATION*" "*IMPLEMENTATION*") "https://shirakumo.github.io/radiance#SPECIAL%20USER%3A%2AIMPLEMENTATION%2A"
   "Special user:*implementation*")
  (("CLASS USER:USER" "USER:USER" "USER") "https://shirakumo.github.io/radiance#CLASS%20USER%3AUSER"
   "Class user:user")
  (("CONDITION USER:CONDITION" "USER:CONDITION" "CONDITION") "https://shirakumo.github.io/radiance#CONDITION%20USER%3ACONDITION"
   "Condition user:condition")
  (("CONDITION USER:NOT-FOUND" "USER:NOT-FOUND" "NOT-FOUND") "https://shirakumo.github.io/radiance#CONDITION%20USER%3ANOT-FOUND"
   "Condition user:not-found")
  (("HOOK USER:ACTION" "USER:ACTION" "ACTION") "https://shirakumo.github.io/radiance#HOOK%20USER%3AACTION"
   "Hook user:action")
  (("HOOK USER:CREATE" "USER:CREATE" "CREATE") "https://shirakumo.github.io/radiance#HOOK%20USER%3ACREATE"
   "Hook user:create")
  (("HOOK USER:IMPLEMENTED" "USER:IMPLEMENTED" "IMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20USER%3AIMPLEMENTED"
   "Hook user:implemented")
  (("HOOK USER:READY" "USER:READY" "READY") "https://shirakumo.github.io/radiance#HOOK%20USER%3AREADY"
   "Hook user:ready")
  (("HOOK USER:REMOVE" "USER:REMOVE" "REMOVE") "https://shirakumo.github.io/radiance#HOOK%20USER%3AREMOVE"
   "Hook user:remove")
  (("HOOK USER:UNIMPLEMENTED" "USER:UNIMPLEMENTED" "UNIMPLEMENTED") "https://shirakumo.github.io/radiance#HOOK%20USER%3AUNIMPLEMENTED"
   "Hook user:unimplemented")
  (("HOOK USER:UNREADY" "USER:UNREADY" "UNREADY") "https://shirakumo.github.io/radiance#HOOK%20USER%3AUNREADY"
   "Hook user:unready")
  (("ACCESSOR USER:FIELD" "USER:FIELD" "FIELD") "https://shirakumo.github.io/radiance#ACCESSOR%20USER%3AFIELD"
   "Accessor user:field")
  (("ACCESSOR USER:IMPLEMENTATION" "USER:IMPLEMENTATION" "IMPLEMENTATION") "https://shirakumo.github.io/radiance#ACCESSOR%20USER%3AIMPLEMENTATION"
   "Accessor user:implementation")
  (("FUNCTION USER:=" "USER:=" "=") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3A%3D"
   "Function user:=")
  (("FUNCTION USER:ACTION" "USER:ACTION" "ACTION") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3AACTION"
   "Function user:action")
  (("FUNCTION USER:ACTIONS" "USER:ACTIONS" "ACTIONS") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3AACTIONS"
   "Function user:actions")
  (("FUNCTION USER:ADD-DEFAULT-PERMISSIONS" "USER:ADD-DEFAULT-PERMISSIONS" "ADD-DEFAULT-PERMISSIONS") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3AADD-DEFAULT-PERMISSIONS"
   "Function user:add-default-permissions")
  (("FUNCTION USER:CHECK" "USER:CHECK" "CHECK") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3ACHECK"
   "Function user:check")
  (("FUNCTION USER:FIELDS" "USER:FIELDS" "FIELDS") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3AFIELDS"
   "Function user:fields")
  (("FUNCTION USER:GET" "USER:GET" "GET") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3AGET"
   "Function user:get")
  (("FUNCTION USER:GRANT" "USER:GRANT" "GRANT") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3AGRANT"
   "Function user:grant")
  (("FUNCTION USER:LIST" "USER:LIST" "LIST") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3ALIST"
   "Function user:list")
  (("FUNCTION USER:REMOVE" "USER:REMOVE" "REMOVE") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3AREMOVE"
   "Function user:remove")
  (("FUNCTION USER:REMOVE-FIELD" "USER:REMOVE-FIELD" "REMOVE-FIELD") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3AREMOVE-FIELD"
   "Function user:remove-field")
  (("FUNCTION USER:REVOKE" "USER:REVOKE" "REVOKE") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3AREVOKE"
   "Function user:revoke")
  (("FUNCTION USER:USERNAME" "USER:USERNAME" "USERNAME") "https://shirakumo.github.io/radiance#FUNCTION%20USER%3AUSERNAME"
   "Function user:username"))
