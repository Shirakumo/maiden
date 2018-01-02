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

(defun split-section-title (text)
  (let ((section (with-output-to-string (out)
                   (loop for c across text
                         do (if (find c "0123456789.")
                                (write-char c out)
                                (return))))))
    (when (string/= "" section)
      (list section
            (string-right-trim "." section)
            (string-left-trim " " (subseq text (length section)))))))

#+(or)
(defun generate-table-from-staple-page (url)
  (let ((doc (etypecase url
               (string (drakma:http-request url))
               (pathname url))))
    (lquery:$ (initialize doc)
      "#documentation" "h1,h2,h3,h4,h5,h6"
      (each (lambda (a)
              (let ((id (lquery:$1 a (attr :id)))
                    (text (string-trim '(#\Space #\Tab #\Return #\Linefeed) (lquery:$1 a (text)))))
                (format T "~&((~{~s~^ ~})~%~s~%~s)"
                        (list* text (split-section-title text))
                        (format NIL "~a~@[#~a~]" url id)
                        text))
              T)))
    (lquery:$ (initialize doc)
      "#symbol-index article"
      (each (lambda (a)
              (cl-ppcre:register-groups-bind (b c d) ("([^ ]+ ([^:]+:(.+)))" (lquery:$1 a (attr :id)))
                (format T "~&((~{~s~^ ~})~%~s~%~s)"
                        (list b c d)
                        (format NIL "~a~a" url (lquery:$1 a ".name a" (attr :href)))
                        (format NIL "~@(~a~)~a" (char b 0) (subseq b 1))))
              T))))
  NIL)

(define-table-lookup lichat
  (("About Lichat-Protocol")
   "https://shirakumo.github.io/lichat-protocol/#about_lichat-protocol"
   "About Lichat-Protocol")
  (("Protocol Specification")
   "https://shirakumo.github.io/lichat-protocol/#protocol_specification"
   "Protocol Specification")
  (("See Also")
   "https://shirakumo.github.io/lichat-protocol/#see_also"
   "See Also")
  (("1. Wire Format" "1." "1" "Wire Format")
   "https://shirakumo.github.io/lichat-protocol/#1._wire_format"
   "1. Wire Format")
  (("2. Server Objects" "2." "2" "Server Objects")
   "https://shirakumo.github.io/lichat-protocol/#2._server_objects"
   "2. Server Objects")
  (("3. General Interaction" "3." "3" "General Interaction")
   "https://shirakumo.github.io/lichat-protocol/#3._general_interaction"
   "3. General Interaction")
  (("4. Connection" "4." "4" "Connection")
   "https://shirakumo.github.io/lichat-protocol/#4._connection"
   "4. Connection")
  (("5. Client Interaction" "5." "5" "Client Interaction")
   "https://shirakumo.github.io/lichat-protocol/#5._client_interaction"
   "5. Client Interaction")
  (("6. Protocol Extension" "6." "6" "Protocol Extension")
   "https://shirakumo.github.io/lichat-protocol/#6._protocol_extension"
   "6. Protocol Extension")
  (("7. Protocol Extensions" "7." "7" "Protocol Extensions")
   "https://shirakumo.github.io/lichat-protocol/#7._protocol_extensions"
   "7. Protocol Extensions")
  (("1.1 Symbols" "1.1" "1.1" "Symbols")
   "https://shirakumo.github.io/lichat-protocol/#1.1_symbols"
   "1.1 Symbols")
  (("1.2 Objects" "1.2" "1.2" "Objects")
   "https://shirakumo.github.io/lichat-protocol/#1.2_objects"
   "1.2 Objects")
  (("1.3 Null Characters" "1.3" "1.3" "Null Characters")
   "https://shirakumo.github.io/lichat-protocol/#1.3_null_characters"
   "1.3 Null Characters")
  (("2.1 Connection" "2.1" "2.1" "Connection")
   "https://shirakumo.github.io/lichat-protocol/#2.1_connection"
   "2.1 Connection")
  (("2.2 User" "2.2" "2.2" "User")
   "https://shirakumo.github.io/lichat-protocol/#2.2_user"
   "2.2 User")
  (("2.3 Profile" "2.3" "2.3" "Profile")
   "https://shirakumo.github.io/lichat-protocol/#2.3_profile"
   "2.3 Profile")
  (("2.4 Channel" "2.4" "2.4" "Channel")
   "https://shirakumo.github.io/lichat-protocol/#2.4_channel"
   "2.4 Channel")
  (("2.5 Permission Rules" "2.5" "2.5" "Permission Rules")
   "https://shirakumo.github.io/lichat-protocol/#2.5_permission_rules"
   "2.5 Permission Rules")
  (("3.1 Null Termination of Updates" "3.1" "3.1" "Null Termination of Updates")
   "https://shirakumo.github.io/lichat-protocol/#3.1_null_termination_of_updates"
   "3.1 Null Termination of Updates")
  (("4.1 Connection Establishment" "4.1" "4.1" "Connection Establishment")
   "https://shirakumo.github.io/lichat-protocol/#4.1_connection_establishment"
   "4.1 Connection Establishment")
  (("4.2 Connection Maintenance" "4.2" "4.2" "Connection Maintenance")
   "https://shirakumo.github.io/lichat-protocol/#4.2_connection_maintenance"
   "4.2 Connection Maintenance")
  (("4.3 Connection Closure" "4.3" "4.3" "Connection Closure")
   "https://shirakumo.github.io/lichat-protocol/#4.3_connection_closure"
   "4.3 Connection Closure")
  (("5.1 General Update Checks" "5.1" "5.1" "General Update Checks")
   "https://shirakumo.github.io/lichat-protocol/#5.1_general_update_checks"
   "5.1 General Update Checks")
  (("5.2 Profile Registration" "5.2" "5.2" "Profile Registration")
   "https://shirakumo.github.io/lichat-protocol/#5.2_profile_registration"
   "5.2 Profile Registration")
  (("5.3 Channel Creation & Management" "5.3" "5.3" "Channel Creation & Management")
   "https://shirakumo.github.io/lichat-protocol/#5.3_channel_creation_&_management"
   "5.3 Channel Creation & Management")
  (("5.4 Channel Interaction" "5.4" "5.4" "Channel Interaction")
   "https://shirakumo.github.io/lichat-protocol/#5.4_channel_interaction"
   "5.4 Channel Interaction")
  (("5.5 Server Information Retrieval" "5.5" "5.5" "Server Information Retrieval")
   "https://shirakumo.github.io/lichat-protocol/#5.5_server_information_retrieval"
   "5.5 Server Information Retrieval")
  (("7.1 Backfill (shirakumo-backfill)" "7.1" "7.1" "Backfill (shirakumo-backfill)")
   "https://shirakumo.github.io/lichat-protocol/#7.1_backfill_(shirakumo-backfill)"
   "7.1 Backfill (shirakumo-backfill)")
  (("7.2 Data (shirakumo-data)" "7.2" "7.2" "Data (shirakumo-data)")
   "https://shirakumo.github.io/lichat-protocol/#7.2_data_(shirakumo-data)"
   "7.2 Data (shirakumo-data)")
  (("7.3 Emotes (shirakumo-emotes)" "7.3" "7.3" "Emotes (shirakumo-emotes)")
   "https://shirakumo.github.io/lichat-protocol/#7.3_emotes_(shirakumo-emotes)"
   "7.3 Emotes (shirakumo-emotes)")
  (("2.2.1 User Name Constraints" "2.2.1" "2.2.1" "User Name Constraints")
   "https://shirakumo.github.io/lichat-protocol/#2.2.1_user_name_constraints"
   "2.2.1 User Name Constraints")
  (("2.3.1 Password Constraints" "2.3.1" "2.3.1" "Password Constraints")
   "https://shirakumo.github.io/lichat-protocol/#2.3.1_password_constraints"
   "2.3.1 Password Constraints")
  (("2.4.1 Primary Channels" "2.4.1" "2.4.1" "Primary Channels")
   "https://shirakumo.github.io/lichat-protocol/#2.4.1_primary_channels"
   "2.4.1 Primary Channels")
  (("2.4.2 Anonymous Channels" "2.4.2" "2.4.2" "Anonymous Channels")
   "https://shirakumo.github.io/lichat-protocol/#2.4.2_anonymous_channels"
   "2.4.2 Anonymous Channels")
  (("2.4.3 Regular Channels" "2.4.3" "2.4.3" "Regular Channels")
   "https://shirakumo.github.io/lichat-protocol/#2.4.3_regular_channels"
   "2.4.3 Regular Channels")
  (("2.4.4 Channel Name Constraints" "2.4.4" "2.4.4" "Channel Name Constraints")
   "https://shirakumo.github.io/lichat-protocol/#2.4.4_channel_name_constraints"
   "2.4.4 Channel Name Constraints")
  (("5.4.1 Joining a Channel" "5.4.1" "5.4.1" "Joining a Channel")
   "https://shirakumo.github.io/lichat-protocol/#5.4.1_joining_a_channel"
   "5.4.1 Joining a Channel")
  (("5.4.2 Leaving a Channel" "5.4.2" "5.4.2" "Leaving a Channel")
   "https://shirakumo.github.io/lichat-protocol/#5.4.2_leaving_a_channel"
   "5.4.2 Leaving a Channel")
  (("5.4.3 Pulling a User" "5.4.3" "5.4.3" "Pulling a User")
   "https://shirakumo.github.io/lichat-protocol/#5.4.3_pulling_a_user"
   "5.4.3 Pulling a User")
  (("5.4.4 Kicking a User" "5.4.4" "5.4.4" "Kicking a User")
   "https://shirakumo.github.io/lichat-protocol/#5.4.4_kicking_a_user"
   "5.4.4 Kicking a User")
  (("5.4.5 Sending a Message" "5.4.5" "5.4.5" "Sending a Message")
   "https://shirakumo.github.io/lichat-protocol/#5.4.5_sending_a_message"
   "5.4.5 Sending a Message")
  (("5.5.1 Listing Public Channels" "5.5.1" "5.5.1" "Listing Public Channels")
   "https://shirakumo.github.io/lichat-protocol/#5.5.1_listing_public_channels"
   "5.5.1 Listing Public Channels")
  (("5.5.2 Listing All Users of a Channel" "5.5.2" "5.5.2" "Listing All Users of a Channel")
   "https://shirakumo.github.io/lichat-protocol/#5.5.2_listing_all_users_of_a_channel"
   "5.5.2 Listing All Users of a Channel")
  (("5.5.3 Requesting Information About a User" "5.5.3" "5.5.3" "Requesting Information About a User")
   "https://shirakumo.github.io/lichat-protocol/#5.5.3_requesting_information_about_a_user"
   "5.5.3 Requesting Information About a User")
  (("SPECIAL LICHAT-PROTOCOL:*DEFAULT-ANONYMOUS-CHANNEL-PERMISSIONS*" "LICHAT-PROTOCOL:*DEFAULT-ANONYMOUS-CHANNEL-PERMISSIONS*" "*DEFAULT-ANONYMOUS-CHANNEL-PERMISSIONS*")
   "https://shirakumo.github.io/lichat-protocol/#SPECIAL%20LICHAT-PROTOCOL%3A%2ADEFAULT-ANONYMOUS-CHANNEL-PERMISSIONS%2A"
   "SPECIAL LICHAT-PROTOCOL:*DEFAULT-ANONYMOUS-CHANNEL-PERMISSIONS*")
  (("SPECIAL LICHAT-PROTOCOL:*DEFAULT-CHANNEL-LIFETIME*" "LICHAT-PROTOCOL:*DEFAULT-CHANNEL-LIFETIME*" "*DEFAULT-CHANNEL-LIFETIME*")
   "https://shirakumo.github.io/lichat-protocol/#SPECIAL%20LICHAT-PROTOCOL%3A%2ADEFAULT-CHANNEL-LIFETIME%2A"
   "SPECIAL LICHAT-PROTOCOL:*DEFAULT-CHANNEL-LIFETIME*")
  (("SPECIAL LICHAT-PROTOCOL:*DEFAULT-PRIMARY-CHANNEL-PERMISSIONS*" "LICHAT-PROTOCOL:*DEFAULT-PRIMARY-CHANNEL-PERMISSIONS*" "*DEFAULT-PRIMARY-CHANNEL-PERMISSIONS*")
   "https://shirakumo.github.io/lichat-protocol/#SPECIAL%20LICHAT-PROTOCOL%3A%2ADEFAULT-PRIMARY-CHANNEL-PERMISSIONS%2A"
   "SPECIAL LICHAT-PROTOCOL:*DEFAULT-PRIMARY-CHANNEL-PERMISSIONS*")
  (("SPECIAL LICHAT-PROTOCOL:*DEFAULT-PROFILE-LIFETIME*" "LICHAT-PROTOCOL:*DEFAULT-PROFILE-LIFETIME*" "*DEFAULT-PROFILE-LIFETIME*")
   "https://shirakumo.github.io/lichat-protocol/#SPECIAL%20LICHAT-PROTOCOL%3A%2ADEFAULT-PROFILE-LIFETIME%2A"
   "SPECIAL LICHAT-PROTOCOL:*DEFAULT-PROFILE-LIFETIME*")
  (("SPECIAL LICHAT-PROTOCOL:*DEFAULT-REGULAR-CHANNEL-PERMISSIONS*" "LICHAT-PROTOCOL:*DEFAULT-REGULAR-CHANNEL-PERMISSIONS*" "*DEFAULT-REGULAR-CHANNEL-PERMISSIONS*")
   "https://shirakumo.github.io/lichat-protocol/#SPECIAL%20LICHAT-PROTOCOL%3A%2ADEFAULT-REGULAR-CHANNEL-PERMISSIONS%2A"
   "SPECIAL LICHAT-PROTOCOL:*DEFAULT-REGULAR-CHANNEL-PERMISSIONS*")
  (("SPECIAL LICHAT-PROTOCOL:*ID-COUNTER*" "LICHAT-PROTOCOL:*ID-COUNTER*" "*ID-COUNTER*")
   "https://shirakumo.github.io/lichat-protocol/#SPECIAL%20LICHAT-PROTOCOL%3A%2AID-COUNTER%2A"
   "SPECIAL LICHAT-PROTOCOL:*ID-COUNTER*")
  (("CLASS LICHAT-PROTOCOL:ALREADY-IN-CHANNEL" "LICHAT-PROTOCOL:ALREADY-IN-CHANNEL" "ALREADY-IN-CHANNEL")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AALREADY-IN-CHANNEL"
   "CLASS LICHAT-PROTOCOL:ALREADY-IN-CHANNEL")
  (("CLASS LICHAT-PROTOCOL:BACKFILL" "LICHAT-PROTOCOL:BACKFILL" "BACKFILL")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ABACKFILL"
   "CLASS LICHAT-PROTOCOL:BACKFILL")
  (("CLASS LICHAT-PROTOCOL:BAD-CONTENT-TYPE" "LICHAT-PROTOCOL:BAD-CONTENT-TYPE" "BAD-CONTENT-TYPE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ABAD-CONTENT-TYPE"
   "CLASS LICHAT-PROTOCOL:BAD-CONTENT-TYPE")
  (("CLASS LICHAT-PROTOCOL:BAD-NAME" "LICHAT-PROTOCOL:BAD-NAME" "BAD-NAME")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ABAD-NAME"
   "CLASS LICHAT-PROTOCOL:BAD-NAME")
  (("CLASS LICHAT-PROTOCOL:CHANNEL" "LICHAT-PROTOCOL:CHANNEL" "CHANNEL")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ACHANNEL"
   "CLASS LICHAT-PROTOCOL:CHANNEL")
  (("CLASS LICHAT-PROTOCOL:CHANNEL-UPDATE" "LICHAT-PROTOCOL:CHANNEL-UPDATE" "CHANNEL-UPDATE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ACHANNEL-UPDATE"
   "CLASS LICHAT-PROTOCOL:CHANNEL-UPDATE")
  (("CLASS LICHAT-PROTOCOL:CHANNELNAME-TAKEN" "LICHAT-PROTOCOL:CHANNELNAME-TAKEN" "CHANNELNAME-TAKEN")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ACHANNELNAME-TAKEN"
   "CLASS LICHAT-PROTOCOL:CHANNELNAME-TAKEN")
  (("CLASS LICHAT-PROTOCOL:CHANNELS" "LICHAT-PROTOCOL:CHANNELS" "CHANNELS")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ACHANNELS"
   "CLASS LICHAT-PROTOCOL:CHANNELS")
  (("CLASS LICHAT-PROTOCOL:CONNECT" "LICHAT-PROTOCOL:CONNECT" "CONNECT")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ACONNECT"
   "CLASS LICHAT-PROTOCOL:CONNECT")
  (("CLASS LICHAT-PROTOCOL:CONNECTION" "LICHAT-PROTOCOL:CONNECTION" "CONNECTION")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ACONNECTION"
   "CLASS LICHAT-PROTOCOL:CONNECTION")
  (("CLASS LICHAT-PROTOCOL:CONNECTION-UNSTABLE" "LICHAT-PROTOCOL:CONNECTION-UNSTABLE" "CONNECTION-UNSTABLE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ACONNECTION-UNSTABLE"
   "CLASS LICHAT-PROTOCOL:CONNECTION-UNSTABLE")
  (("CLASS LICHAT-PROTOCOL:CREATE" "LICHAT-PROTOCOL:CREATE" "CREATE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ACREATE"
   "CLASS LICHAT-PROTOCOL:CREATE")
  (("CLASS LICHAT-PROTOCOL:DATA" "LICHAT-PROTOCOL:DATA" "DATA")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ADATA"
   "CLASS LICHAT-PROTOCOL:DATA")
  (("CLASS LICHAT-PROTOCOL:DISCONNECT" "LICHAT-PROTOCOL:DISCONNECT" "DISCONNECT")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ADISCONNECT"
   "CLASS LICHAT-PROTOCOL:DISCONNECT")
  (("CLASS LICHAT-PROTOCOL:EMOTE" "LICHAT-PROTOCOL:EMOTE" "EMOTE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AEMOTE"
   "CLASS LICHAT-PROTOCOL:EMOTE")
  (("CLASS LICHAT-PROTOCOL:EMOTES" "LICHAT-PROTOCOL:EMOTES" "EMOTES")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AEMOTES"
   "CLASS LICHAT-PROTOCOL:EMOTES")
  (("CLASS LICHAT-PROTOCOL:FAILURE" "LICHAT-PROTOCOL:FAILURE" "FAILURE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AFAILURE"
   "CLASS LICHAT-PROTOCOL:FAILURE")
  (("CLASS LICHAT-PROTOCOL:INCOMPATIBLE-VERSION" "LICHAT-PROTOCOL:INCOMPATIBLE-VERSION" "INCOMPATIBLE-VERSION")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AINCOMPATIBLE-VERSION"
   "CLASS LICHAT-PROTOCOL:INCOMPATIBLE-VERSION")
  (("CLASS LICHAT-PROTOCOL:INSUFFICIENT-PERMISSIONS" "LICHAT-PROTOCOL:INSUFFICIENT-PERMISSIONS" "INSUFFICIENT-PERMISSIONS")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AINSUFFICIENT-PERMISSIONS"
   "CLASS LICHAT-PROTOCOL:INSUFFICIENT-PERMISSIONS")
  (("CLASS LICHAT-PROTOCOL:INVALID-PASSWORD" "LICHAT-PROTOCOL:INVALID-PASSWORD" "INVALID-PASSWORD")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AINVALID-PASSWORD"
   "CLASS LICHAT-PROTOCOL:INVALID-PASSWORD")
  (("CLASS LICHAT-PROTOCOL:INVALID-PERMISSIONS" "LICHAT-PROTOCOL:INVALID-PERMISSIONS" "INVALID-PERMISSIONS")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AINVALID-PERMISSIONS"
   "CLASS LICHAT-PROTOCOL:INVALID-PERMISSIONS")
  (("CLASS LICHAT-PROTOCOL:INVALID-UPDATE" "LICHAT-PROTOCOL:INVALID-UPDATE" "INVALID-UPDATE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AINVALID-UPDATE"
   "CLASS LICHAT-PROTOCOL:INVALID-UPDATE")
  (("CLASS LICHAT-PROTOCOL:JOIN" "LICHAT-PROTOCOL:JOIN" "JOIN")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AJOIN"
   "CLASS LICHAT-PROTOCOL:JOIN")
  (("CLASS LICHAT-PROTOCOL:KICK" "LICHAT-PROTOCOL:KICK" "KICK")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AKICK"
   "CLASS LICHAT-PROTOCOL:KICK")
  (("CLASS LICHAT-PROTOCOL:LEAVE" "LICHAT-PROTOCOL:LEAVE" "LEAVE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ALEAVE"
   "CLASS LICHAT-PROTOCOL:LEAVE")
  (("CLASS LICHAT-PROTOCOL:MALFORMED-UPDATE" "LICHAT-PROTOCOL:MALFORMED-UPDATE" "MALFORMED-UPDATE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AMALFORMED-UPDATE"
   "CLASS LICHAT-PROTOCOL:MALFORMED-UPDATE")
  (("CLASS LICHAT-PROTOCOL:MESSAGE" "LICHAT-PROTOCOL:MESSAGE" "MESSAGE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AMESSAGE"
   "CLASS LICHAT-PROTOCOL:MESSAGE")
  (("CLASS LICHAT-PROTOCOL:NAMED-OBJECT" "LICHAT-PROTOCOL:NAMED-OBJECT" "NAMED-OBJECT")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ANAMED-OBJECT"
   "CLASS LICHAT-PROTOCOL:NAMED-OBJECT")
  (("CLASS LICHAT-PROTOCOL:NO-SUCH-CHANNEL" "LICHAT-PROTOCOL:NO-SUCH-CHANNEL" "NO-SUCH-CHANNEL")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ANO-SUCH-CHANNEL"
   "CLASS LICHAT-PROTOCOL:NO-SUCH-CHANNEL")
  (("CLASS LICHAT-PROTOCOL:NO-SUCH-PROFILE" "LICHAT-PROTOCOL:NO-SUCH-PROFILE" "NO-SUCH-PROFILE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ANO-SUCH-PROFILE"
   "CLASS LICHAT-PROTOCOL:NO-SUCH-PROFILE")
  (("CLASS LICHAT-PROTOCOL:NO-SUCH-USER" "LICHAT-PROTOCOL:NO-SUCH-USER" "NO-SUCH-USER")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ANO-SUCH-USER"
   "CLASS LICHAT-PROTOCOL:NO-SUCH-USER")
  (("CLASS LICHAT-PROTOCOL:NOT-IN-CHANNEL" "LICHAT-PROTOCOL:NOT-IN-CHANNEL" "NOT-IN-CHANNEL")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ANOT-IN-CHANNEL"
   "CLASS LICHAT-PROTOCOL:NOT-IN-CHANNEL")
  (("CLASS LICHAT-PROTOCOL:PERMISSIONS" "LICHAT-PROTOCOL:PERMISSIONS" "PERMISSIONS")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3APERMISSIONS"
   "CLASS LICHAT-PROTOCOL:PERMISSIONS")
  (("CLASS LICHAT-PROTOCOL:PING" "LICHAT-PROTOCOL:PING" "PING")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3APING"
   "CLASS LICHAT-PROTOCOL:PING")
  (("CLASS LICHAT-PROTOCOL:PONG" "LICHAT-PROTOCOL:PONG" "PONG")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3APONG"
   "CLASS LICHAT-PROTOCOL:PONG")
  (("CLASS LICHAT-PROTOCOL:PROFILE" "LICHAT-PROTOCOL:PROFILE" "PROFILE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3APROFILE"
   "CLASS LICHAT-PROTOCOL:PROFILE")
  (("CLASS LICHAT-PROTOCOL:PULL" "LICHAT-PROTOCOL:PULL" "PULL")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3APULL"
   "CLASS LICHAT-PROTOCOL:PULL")
  (("CLASS LICHAT-PROTOCOL:REGISTER" "LICHAT-PROTOCOL:REGISTER" "REGISTER")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AREGISTER"
   "CLASS LICHAT-PROTOCOL:REGISTER")
  (("CLASS LICHAT-PROTOCOL:SERVER-OBJECT" "LICHAT-PROTOCOL:SERVER-OBJECT" "SERVER-OBJECT")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ASERVER-OBJECT"
   "CLASS LICHAT-PROTOCOL:SERVER-OBJECT")
  (("CLASS LICHAT-PROTOCOL:TARGET-UPDATE" "LICHAT-PROTOCOL:TARGET-UPDATE" "TARGET-UPDATE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ATARGET-UPDATE"
   "CLASS LICHAT-PROTOCOL:TARGET-UPDATE")
  (("CLASS LICHAT-PROTOCOL:TEXT-UPDATE" "LICHAT-PROTOCOL:TEXT-UPDATE" "TEXT-UPDATE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ATEXT-UPDATE"
   "CLASS LICHAT-PROTOCOL:TEXT-UPDATE")
  (("CLASS LICHAT-PROTOCOL:TOO-MANY-CONNECTIONS" "LICHAT-PROTOCOL:TOO-MANY-CONNECTIONS" "TOO-MANY-CONNECTIONS")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ATOO-MANY-CONNECTIONS"
   "CLASS LICHAT-PROTOCOL:TOO-MANY-CONNECTIONS")
  (("CLASS LICHAT-PROTOCOL:TOO-MANY-UPDATES" "LICHAT-PROTOCOL:TOO-MANY-UPDATES" "TOO-MANY-UPDATES")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3ATOO-MANY-UPDATES"
   "CLASS LICHAT-PROTOCOL:TOO-MANY-UPDATES")
  (("CLASS LICHAT-PROTOCOL:UPDATE" "LICHAT-PROTOCOL:UPDATE" "UPDATE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AUPDATE"
   "CLASS LICHAT-PROTOCOL:UPDATE")
  (("CLASS LICHAT-PROTOCOL:UPDATE-FAILURE" "LICHAT-PROTOCOL:UPDATE-FAILURE" "UPDATE-FAILURE")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AUPDATE-FAILURE"
   "CLASS LICHAT-PROTOCOL:UPDATE-FAILURE")
  (("CLASS LICHAT-PROTOCOL:UPDATE-TOO-LONG" "LICHAT-PROTOCOL:UPDATE-TOO-LONG" "UPDATE-TOO-LONG")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AUPDATE-TOO-LONG"
   "CLASS LICHAT-PROTOCOL:UPDATE-TOO-LONG")
  (("CLASS LICHAT-PROTOCOL:USER" "LICHAT-PROTOCOL:USER" "USER")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AUSER"
   "CLASS LICHAT-PROTOCOL:USER")
  (("CLASS LICHAT-PROTOCOL:USER-INFO" "LICHAT-PROTOCOL:USER-INFO" "USER-INFO")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AUSER-INFO"
   "CLASS LICHAT-PROTOCOL:USER-INFO")
  (("CLASS LICHAT-PROTOCOL:USERNAME-MISMATCH" "LICHAT-PROTOCOL:USERNAME-MISMATCH" "USERNAME-MISMATCH")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AUSERNAME-MISMATCH"
   "CLASS LICHAT-PROTOCOL:USERNAME-MISMATCH")
  (("CLASS LICHAT-PROTOCOL:USERNAME-TAKEN" "LICHAT-PROTOCOL:USERNAME-TAKEN" "USERNAME-TAKEN")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AUSERNAME-TAKEN"
   "CLASS LICHAT-PROTOCOL:USERNAME-TAKEN")
  (("CLASS LICHAT-PROTOCOL:USERS" "LICHAT-PROTOCOL:USERS" "USERS")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AUSERS"
   "CLASS LICHAT-PROTOCOL:USERS")
  (("CLASS LICHAT-PROTOCOL:WIRE-OBJECT" "LICHAT-PROTOCOL:WIRE-OBJECT" "WIRE-OBJECT")
   "https://shirakumo.github.io/lichat-protocol/#CLASS%20LICHAT-PROTOCOL%3AWIRE-OBJECT"
   "CLASS LICHAT-PROTOCOL:WIRE-OBJECT")
  (("CONDITION LICHAT-PROTOCOL:INCOMPATIBLE-VALUE-TYPE-FOR-SLOT" "LICHAT-PROTOCOL:INCOMPATIBLE-VALUE-TYPE-FOR-SLOT" "INCOMPATIBLE-VALUE-TYPE-FOR-SLOT")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3AINCOMPATIBLE-VALUE-TYPE-FOR-SLOT"
   "CONDITION LICHAT-PROTOCOL:INCOMPATIBLE-VALUE-TYPE-FOR-SLOT")
  (("CONDITION LICHAT-PROTOCOL:INCOMPLETE-TOKEN" "LICHAT-PROTOCOL:INCOMPLETE-TOKEN" "INCOMPLETE-TOKEN")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3AINCOMPLETE-TOKEN"
   "CONDITION LICHAT-PROTOCOL:INCOMPLETE-TOKEN")
  (("CONDITION LICHAT-PROTOCOL:MALFORMED-WIRE-OBJECT" "LICHAT-PROTOCOL:MALFORMED-WIRE-OBJECT" "MALFORMED-WIRE-OBJECT")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3AMALFORMED-WIRE-OBJECT"
   "CONDITION LICHAT-PROTOCOL:MALFORMED-WIRE-OBJECT")
  (("CONDITION LICHAT-PROTOCOL:MISSING-CLOCK" "LICHAT-PROTOCOL:MISSING-CLOCK" "MISSING-CLOCK")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3AMISSING-CLOCK"
   "CONDITION LICHAT-PROTOCOL:MISSING-CLOCK")
  (("CONDITION LICHAT-PROTOCOL:MISSING-ID" "LICHAT-PROTOCOL:MISSING-ID" "MISSING-ID")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3AMISSING-ID"
   "CONDITION LICHAT-PROTOCOL:MISSING-ID")
  (("CONDITION LICHAT-PROTOCOL:MISSING-UPDATE-ARGUMENT" "LICHAT-PROTOCOL:MISSING-UPDATE-ARGUMENT" "MISSING-UPDATE-ARGUMENT")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3AMISSING-UPDATE-ARGUMENT"
   "CONDITION LICHAT-PROTOCOL:MISSING-UPDATE-ARGUMENT")
  (("CONDITION LICHAT-PROTOCOL:NULL-IN-SYMBOL-DESIGNATOR" "LICHAT-PROTOCOL:NULL-IN-SYMBOL-DESIGNATOR" "NULL-IN-SYMBOL-DESIGNATOR")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3ANULL-IN-SYMBOL-DESIGNATOR"
   "CONDITION LICHAT-PROTOCOL:NULL-IN-SYMBOL-DESIGNATOR")
  (("CONDITION LICHAT-PROTOCOL:PRINTER-CONDITION" "LICHAT-PROTOCOL:PRINTER-CONDITION" "PRINTER-CONDITION")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3APRINTER-CONDITION"
   "CONDITION LICHAT-PROTOCOL:PRINTER-CONDITION")
  (("CONDITION LICHAT-PROTOCOL:PROTOCOL-CONDITION" "LICHAT-PROTOCOL:PROTOCOL-CONDITION" "PROTOCOL-CONDITION")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3APROTOCOL-CONDITION"
   "CONDITION LICHAT-PROTOCOL:PROTOCOL-CONDITION")
  (("CONDITION LICHAT-PROTOCOL:READ-LIMIT-HIT" "LICHAT-PROTOCOL:READ-LIMIT-HIT" "READ-LIMIT-HIT")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3AREAD-LIMIT-HIT"
   "CONDITION LICHAT-PROTOCOL:READ-LIMIT-HIT")
  (("CONDITION LICHAT-PROTOCOL:READER-CONDITION" "LICHAT-PROTOCOL:READER-CONDITION" "READER-CONDITION")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3AREADER-CONDITION"
   "CONDITION LICHAT-PROTOCOL:READER-CONDITION")
  (("CONDITION LICHAT-PROTOCOL:STRAY-NULL-FOUND" "LICHAT-PROTOCOL:STRAY-NULL-FOUND" "STRAY-NULL-FOUND")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3ASTRAY-NULL-FOUND"
   "CONDITION LICHAT-PROTOCOL:STRAY-NULL-FOUND")
  (("CONDITION LICHAT-PROTOCOL:UNKNOWN-SYMBOL" "LICHAT-PROTOCOL:UNKNOWN-SYMBOL" "UNKNOWN-SYMBOL")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3AUNKNOWN-SYMBOL"
   "CONDITION LICHAT-PROTOCOL:UNKNOWN-SYMBOL")
  (("CONDITION LICHAT-PROTOCOL:UNKNOWN-WIRE-OBJECT" "LICHAT-PROTOCOL:UNKNOWN-WIRE-OBJECT" "UNKNOWN-WIRE-OBJECT")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3AUNKNOWN-WIRE-OBJECT"
   "CONDITION LICHAT-PROTOCOL:UNKNOWN-WIRE-OBJECT")
  (("CONDITION LICHAT-PROTOCOL:UNPRINTABLE-OBJECT" "LICHAT-PROTOCOL:UNPRINTABLE-OBJECT" "UNPRINTABLE-OBJECT")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3AUNPRINTABLE-OBJECT"
   "CONDITION LICHAT-PROTOCOL:UNPRINTABLE-OBJECT")
  (("CONDITION LICHAT-PROTOCOL:WIRE-CONDITION" "LICHAT-PROTOCOL:WIRE-CONDITION" "WIRE-CONDITION")
   "https://shirakumo.github.io/lichat-protocol/#CONDITION%20LICHAT-PROTOCOL%3AWIRE-CONDITION"
   "CONDITION LICHAT-PROTOCOL:WIRE-CONDITION")
  (("ACCESSOR LICHAT-PROTOCOL:ALLOWED-CONTENT-TYPES" "LICHAT-PROTOCOL:ALLOWED-CONTENT-TYPES" "ALLOWED-CONTENT-TYPES")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3AALLOWED-CONTENT-TYPES"
   "ACCESSOR LICHAT-PROTOCOL:ALLOWED-CONTENT-TYPES")
  (("ACCESSOR LICHAT-PROTOCOL:CHANNEL" "LICHAT-PROTOCOL:CHANNEL" "CHANNEL")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3ACHANNEL"
   "ACCESSOR LICHAT-PROTOCOL:CHANNEL")
  (("ACCESSOR LICHAT-PROTOCOL:CHANNELS" "LICHAT-PROTOCOL:CHANNELS" "CHANNELS")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3ACHANNELS"
   "ACCESSOR LICHAT-PROTOCOL:CHANNELS")
  (("ACCESSOR LICHAT-PROTOCOL:CLOCK" "LICHAT-PROTOCOL:CLOCK" "CLOCK")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3ACLOCK"
   "ACCESSOR LICHAT-PROTOCOL:CLOCK")
  (("ACCESSOR LICHAT-PROTOCOL:CONNECTIONS" "LICHAT-PROTOCOL:CONNECTIONS" "CONNECTIONS")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3ACONNECTIONS"
   "ACCESSOR LICHAT-PROTOCOL:CONNECTIONS")
  (("ACCESSOR LICHAT-PROTOCOL:CONTENT-TYPE" "LICHAT-PROTOCOL:CONTENT-TYPE" "CONTENT-TYPE")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3ACONTENT-TYPE"
   "ACCESSOR LICHAT-PROTOCOL:CONTENT-TYPE")
  (("ACCESSOR LICHAT-PROTOCOL:EXTENSIONS" "LICHAT-PROTOCOL:EXTENSIONS" "EXTENSIONS")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3AEXTENSIONS"
   "ACCESSOR LICHAT-PROTOCOL:EXTENSIONS")
  (("ACCESSOR LICHAT-PROTOCOL:FILENAME" "LICHAT-PROTOCOL:FILENAME" "FILENAME")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3AFILENAME"
   "ACCESSOR LICHAT-PROTOCOL:FILENAME")
  (("ACCESSOR LICHAT-PROTOCOL:FROM" "LICHAT-PROTOCOL:FROM" "FROM")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3AFROM"
   "ACCESSOR LICHAT-PROTOCOL:FROM")
  (("ACCESSOR LICHAT-PROTOCOL:ID" "LICHAT-PROTOCOL:ID" "ID")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3AID"
   "ACCESSOR LICHAT-PROTOCOL:ID")
  (("ACCESSOR LICHAT-PROTOCOL:LIFETIME" "LICHAT-PROTOCOL:LIFETIME" "LIFETIME")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3ALIFETIME"
   "ACCESSOR LICHAT-PROTOCOL:LIFETIME")
  (("ACCESSOR LICHAT-PROTOCOL:NAME" "LICHAT-PROTOCOL:NAME" "NAME")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3ANAME"
   "ACCESSOR LICHAT-PROTOCOL:NAME")
  (("ACCESSOR LICHAT-PROTOCOL:NAMES" "LICHAT-PROTOCOL:NAMES" "NAMES")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3ANAMES"
   "ACCESSOR LICHAT-PROTOCOL:NAMES")
  (("ACCESSOR LICHAT-PROTOCOL:PASSWORD" "LICHAT-PROTOCOL:PASSWORD" "PASSWORD")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3APASSWORD"
   "ACCESSOR LICHAT-PROTOCOL:PASSWORD")
  (("ACCESSOR LICHAT-PROTOCOL:PAYLOAD" "LICHAT-PROTOCOL:PAYLOAD" "PAYLOAD")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3APAYLOAD"
   "ACCESSOR LICHAT-PROTOCOL:PAYLOAD")
  (("ACCESSOR LICHAT-PROTOCOL:PERMISSIONS" "LICHAT-PROTOCOL:PERMISSIONS" "PERMISSIONS")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3APERMISSIONS"
   "ACCESSOR LICHAT-PROTOCOL:PERMISSIONS")
  (("ACCESSOR LICHAT-PROTOCOL:REGISTERED" "LICHAT-PROTOCOL:REGISTERED" "REGISTERED")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3AREGISTERED"
   "ACCESSOR LICHAT-PROTOCOL:REGISTERED")
  (("ACCESSOR LICHAT-PROTOCOL:TARGET" "LICHAT-PROTOCOL:TARGET" "TARGET")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3ATARGET"
   "ACCESSOR LICHAT-PROTOCOL:TARGET")
  (("ACCESSOR LICHAT-PROTOCOL:TEXT" "LICHAT-PROTOCOL:TEXT" "TEXT")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3ATEXT"
   "ACCESSOR LICHAT-PROTOCOL:TEXT")
  (("ACCESSOR LICHAT-PROTOCOL:UPDATE-ID" "LICHAT-PROTOCOL:UPDATE-ID" "UPDATE-ID")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3AUPDATE-ID"
   "ACCESSOR LICHAT-PROTOCOL:UPDATE-ID")
  (("ACCESSOR LICHAT-PROTOCOL:USER" "LICHAT-PROTOCOL:USER" "USER")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3AUSER"
   "ACCESSOR LICHAT-PROTOCOL:USER")
  (("ACCESSOR LICHAT-PROTOCOL:USERS" "LICHAT-PROTOCOL:USERS" "USERS")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3AUSERS"
   "ACCESSOR LICHAT-PROTOCOL:USERS")
  (("ACCESSOR LICHAT-PROTOCOL:VERSION" "LICHAT-PROTOCOL:VERSION" "VERSION")
   "https://shirakumo.github.io/lichat-protocol/#ACCESSOR%20LICHAT-PROTOCOL%3AVERSION"
   "ACCESSOR LICHAT-PROTOCOL:VERSION")
  (("FUNCTION LICHAT-PROTOCOL:CHANNELNAME-P" "LICHAT-PROTOCOL:CHANNELNAME-P" "CHANNELNAME-P")
   "https://shirakumo.github.io/lichat-protocol/#FUNCTION%20LICHAT-PROTOCOL%3ACHANNELNAME-P"
   "FUNCTION LICHAT-PROTOCOL:CHANNELNAME-P")
  (("FUNCTION LICHAT-PROTOCOL:CHECK-UPDATE-OPTIONS" "LICHAT-PROTOCOL:CHECK-UPDATE-OPTIONS" "CHECK-UPDATE-OPTIONS")
   "https://shirakumo.github.io/lichat-protocol/#FUNCTION%20LICHAT-PROTOCOL%3ACHECK-UPDATE-OPTIONS"
   "FUNCTION LICHAT-PROTOCOL:CHECK-UPDATE-OPTIONS")
  (("FUNCTION LICHAT-PROTOCOL:FROM-WIRE" "LICHAT-PROTOCOL:FROM-WIRE" "FROM-WIRE")
   "https://shirakumo.github.io/lichat-protocol/#FUNCTION%20LICHAT-PROTOCOL%3AFROM-WIRE"
   "FUNCTION LICHAT-PROTOCOL:FROM-WIRE")
  (("FUNCTION LICHAT-PROTOCOL:ID-P" "LICHAT-PROTOCOL:ID-P" "ID-P")
   "https://shirakumo.github.io/lichat-protocol/#FUNCTION%20LICHAT-PROTOCOL%3AID-P"
   "FUNCTION LICHAT-PROTOCOL:ID-P")
  (("FUNCTION LICHAT-PROTOCOL:NEXT-ID" "LICHAT-PROTOCOL:NEXT-ID" "NEXT-ID")
   "https://shirakumo.github.io/lichat-protocol/#FUNCTION%20LICHAT-PROTOCOL%3ANEXT-ID"
   "FUNCTION LICHAT-PROTOCOL:NEXT-ID")
  (("FUNCTION LICHAT-PROTOCOL:PASSWORD-P" "LICHAT-PROTOCOL:PASSWORD-P" "PASSWORD-P")
   "https://shirakumo.github.io/lichat-protocol/#FUNCTION%20LICHAT-PROTOCOL%3APASSWORD-P"
   "FUNCTION LICHAT-PROTOCOL:PASSWORD-P")
  (("FUNCTION LICHAT-PROTOCOL:PRINT-SEXPR" "LICHAT-PROTOCOL:PRINT-SEXPR" "PRINT-SEXPR")
   "https://shirakumo.github.io/lichat-protocol/#FUNCTION%20LICHAT-PROTOCOL%3APRINT-SEXPR"
   "FUNCTION LICHAT-PROTOCOL:PRINT-SEXPR")
  (("FUNCTION LICHAT-PROTOCOL:PROTOCOL-VERSION" "LICHAT-PROTOCOL:PROTOCOL-VERSION" "PROTOCOL-VERSION")
   "https://shirakumo.github.io/lichat-protocol/#FUNCTION%20LICHAT-PROTOCOL%3APROTOCOL-VERSION"
   "FUNCTION LICHAT-PROTOCOL:PROTOCOL-VERSION")
  (("FUNCTION LICHAT-PROTOCOL:READ-SEXPR" "LICHAT-PROTOCOL:READ-SEXPR" "READ-SEXPR")
   "https://shirakumo.github.io/lichat-protocol/#FUNCTION%20LICHAT-PROTOCOL%3AREAD-SEXPR"
   "FUNCTION LICHAT-PROTOCOL:READ-SEXPR")
  (("FUNCTION LICHAT-PROTOCOL:TO-WIRE" "LICHAT-PROTOCOL:TO-WIRE" "TO-WIRE")
   "https://shirakumo.github.io/lichat-protocol/#FUNCTION%20LICHAT-PROTOCOL%3ATO-WIRE"
   "FUNCTION LICHAT-PROTOCOL:TO-WIRE")
  (("FUNCTION LICHAT-PROTOCOL:USERNAME-P" "LICHAT-PROTOCOL:USERNAME-P" "USERNAME-P")
   "https://shirakumo.github.io/lichat-protocol/#FUNCTION%20LICHAT-PROTOCOL%3AUSERNAME-P"
   "FUNCTION LICHAT-PROTOCOL:USERNAME-P")
  (("FUNCTION LICHAT-PROTOCOL:WHITESPACE-P" "LICHAT-PROTOCOL:WHITESPACE-P" "WHITESPACE-P")
   "https://shirakumo.github.io/lichat-protocol/#FUNCTION%20LICHAT-PROTOCOL%3AWHITESPACE-P"
   "FUNCTION LICHAT-PROTOCOL:WHITESPACE-P")
  (("GENERIC LICHAT-PROTOCOL:OBJECT" "LICHAT-PROTOCOL:OBJECT" "OBJECT")
   "https://shirakumo.github.io/lichat-protocol/#GENERIC%20LICHAT-PROTOCOL%3AOBJECT"
   "GENERIC LICHAT-PROTOCOL:OBJECT")
  (("GENERIC LICHAT-PROTOCOL:SYMBOL-DESIGNATOR" "LICHAT-PROTOCOL:SYMBOL-DESIGNATOR" "SYMBOL-DESIGNATOR")
   "https://shirakumo.github.io/lichat-protocol/#GENERIC%20LICHAT-PROTOCOL%3ASYMBOL-DESIGNATOR"
   "GENERIC LICHAT-PROTOCOL:SYMBOL-DESIGNATOR")
  (("GENERIC LICHAT-PROTOCOL:UPDATE" "LICHAT-PROTOCOL:UPDATE" "UPDATE")
   "https://shirakumo.github.io/lichat-protocol/#GENERIC%20LICHAT-PROTOCOL%3AUPDATE"
   "GENERIC LICHAT-PROTOCOL:UPDATE")
  (("MACRO LICHAT-PROTOCOL:DEFINE-PROTOCOL-CLASS" "LICHAT-PROTOCOL:DEFINE-PROTOCOL-CLASS" "DEFINE-PROTOCOL-CLASS")
   "https://shirakumo.github.io/lichat-protocol/#MACRO%20LICHAT-PROTOCOL%3ADEFINE-PROTOCOL-CLASS"
   "MACRO LICHAT-PROTOCOL:DEFINE-PROTOCOL-CLASS"))

(define-table-lookup radiance
  (("About Radiance")
   "https://shirakumo.github.io/radiance/"
   "About Radiance")
  (("Getting It")
   "https://shirakumo.github.io/radiance/"
   "Getting It")
  (("A Lengthy and In-Depth Example")
   "https://shirakumo.github.io/radiance/"
   "A Lengthy and In-Depth Example")
  (("A Simple Example")
   "https://shirakumo.github.io/radiance/"
   "A Simple Example")
  (("1. Radiance Concepts & Parts" "1." "1" "Radiance Concepts & Parts")
   "https://shirakumo.github.io/radiance/"
   "1. Radiance Concepts & Parts")
  (("2. Standard Interfaces" "2." "2" "Standard Interfaces")
   "https://shirakumo.github.io/radiance/"
   "2. Standard Interfaces")
  (("Also See")
   "https://shirakumo.github.io/radiance/"
   "Also See")
  (("1.1 URI" "1.1" "1.1" "URI")
   "https://shirakumo.github.io/radiance/"
   "1.1 URI")
  (("1.2 Request and Response" "1.2" "1.2" "Request and Response")
   "https://shirakumo.github.io/radiance/"
   "1.2 Request and Response")
  (("1.3 Route" "1.3" "1.3" "Route")
   "https://shirakumo.github.io/radiance/"
   "1.3 Route")
  (("1.4 URI Dispatcher" "1.4" "1.4" "URI Dispatcher")
   "https://shirakumo.github.io/radiance/"
   "1.4 URI Dispatcher")
  (("1.5 Page" "1.5" "1.5" "Page")
   "https://shirakumo.github.io/radiance/"
   "1.5 Page")
  (("1.6 API Endpoint" "1.6" "1.6" "API Endpoint")
   "https://shirakumo.github.io/radiance/"
   "1.6 API Endpoint")
  (("1.7 Options" "1.7" "1.7" "Options")
   "https://shirakumo.github.io/radiance/"
   "1.7 Options")
  (("1.8 Module" "1.8" "1.8" "Module")
   "https://shirakumo.github.io/radiance/"
   "1.8 Module")
  (("1.9 Hooks" "1.9" "1.9" "Hooks")
   "https://shirakumo.github.io/radiance/"
   "1.9 Hooks")
  (("1.10 Interface" "1.10" "1.10" "Interface")
   "https://shirakumo.github.io/radiance/"
   "1.10 Interface")
  (("1.11 Environment" "1.11" "1.11" "Environment")
   "https://shirakumo.github.io/radiance/"
   "1.11 Environment")
  (("1.12 Instance Management" "1.12" "1.12" "Instance Management")
   "https://shirakumo.github.io/radiance/"
   "1.12 Instance Management")
  (("2.1 admin" "2.1" "2.1" "admin")
   "https://shirakumo.github.io/radiance/"
   "2.1 admin")
  (("2.2 auth" "2.2" "2.2" "auth")
   "https://shirakumo.github.io/radiance/"
   "2.2 auth")
  (("2.3 ban" "2.3" "2.3" "ban")
   "https://shirakumo.github.io/radiance/"
   "2.3 ban")
  (("2.4 cache" "2.4" "2.4" "cache")
   "https://shirakumo.github.io/radiance/"
   "2.4 cache")
  (("2.5 database" "2.5" "2.5" "database")
   "https://shirakumo.github.io/radiance/"
   "2.5 database")
  (("2.6 logger" "2.6" "2.6" "logger")
   "https://shirakumo.github.io/radiance/"
   "2.6 logger")
  (("2.7 Mail" "2.7" "2.7" "Mail")
   "https://shirakumo.github.io/radiance/"
   "2.7 Mail")
  (("2.8 profile" "2.8" "2.8" "profile")
   "https://shirakumo.github.io/radiance/"
   "2.8 profile")
  (("2.9 rate" "2.9" "2.9" "rate")
   "https://shirakumo.github.io/radiance/"
   "2.9 rate")
  (("2.10 server" "2.10" "2.10" "server")
   "https://shirakumo.github.io/radiance/"
   "2.10 server")
  (("2.11 session" "2.11" "2.11" "session")
   "https://shirakumo.github.io/radiance/"
   "2.11 session")
  (("2.12 user" "2.12" "2.12" "user")
   "https://shirakumo.github.io/radiance/"
   "2.12 user")
  (("SPECIAL RADIANCE-CORE:*DEBUGGER*" "RADIANCE-CORE:*DEBUGGER*" "*DEBUGGER*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20RADIANCE-CORE%3A%2ADEBUGGER%2A"
   "SPECIAL RADIANCE-CORE:*DEBUGGER*")
  (("SPECIAL RADIANCE-CORE:*DEFAULT-API-FORMAT*" "RADIANCE-CORE:*DEFAULT-API-FORMAT*" "*DEFAULT-API-FORMAT*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20RADIANCE-CORE%3A%2ADEFAULT-API-FORMAT%2A"
   "SPECIAL RADIANCE-CORE:*DEFAULT-API-FORMAT*")
  (("SPECIAL RADIANCE-CORE:*DEFAULT-CONTENT-TYPE*" "RADIANCE-CORE:*DEFAULT-CONTENT-TYPE*" "*DEFAULT-CONTENT-TYPE*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20RADIANCE-CORE%3A%2ADEFAULT-CONTENT-TYPE%2A"
   "SPECIAL RADIANCE-CORE:*DEFAULT-CONTENT-TYPE*")
  (("SPECIAL RADIANCE-CORE:*DEFAULT-EXTERNAL-FORMAT*" "RADIANCE-CORE:*DEFAULT-EXTERNAL-FORMAT*" "*DEFAULT-EXTERNAL-FORMAT*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20RADIANCE-CORE%3A%2ADEFAULT-EXTERNAL-FORMAT%2A"
   "SPECIAL RADIANCE-CORE:*DEFAULT-EXTERNAL-FORMAT*")
  (("SPECIAL RADIANCE-CORE:*ENVIRONMENT-ROOT*" "RADIANCE-CORE:*ENVIRONMENT-ROOT*" "*ENVIRONMENT-ROOT*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20RADIANCE-CORE%3A%2AENVIRONMENT-ROOT%2A"
   "SPECIAL RADIANCE-CORE:*ENVIRONMENT-ROOT*")
  (("SPECIAL RADIANCE-CORE:*MODULES-DIRECTORY*" "RADIANCE-CORE:*MODULES-DIRECTORY*" "*MODULES-DIRECTORY*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20RADIANCE-CORE%3A%2AMODULES-DIRECTORY%2A"
   "SPECIAL RADIANCE-CORE:*MODULES-DIRECTORY*")
  (("SPECIAL RADIANCE-CORE:*RANDOM-STRING-CHARACTERS*" "RADIANCE-CORE:*RANDOM-STRING-CHARACTERS*" "*RANDOM-STRING-CHARACTERS*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20RADIANCE-CORE%3A%2ARANDOM-STRING-CHARACTERS%2A"
   "SPECIAL RADIANCE-CORE:*RANDOM-STRING-CHARACTERS*")
  (("SPECIAL RADIANCE-CORE:*REQUEST*" "RADIANCE-CORE:*REQUEST*" "*REQUEST*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20RADIANCE-CORE%3A%2AREQUEST%2A"
   "SPECIAL RADIANCE-CORE:*REQUEST*")
  (("SPECIAL RADIANCE-CORE:*RESPONSE*" "RADIANCE-CORE:*RESPONSE*" "*RESPONSE*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20RADIANCE-CORE%3A%2ARESPONSE%2A"
   "SPECIAL RADIANCE-CORE:*RESPONSE*")
  (("SPECIAL RADIANCE-CORE:*STARTUP-TIME*" "RADIANCE-CORE:*STARTUP-TIME*" "*STARTUP-TIME*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20RADIANCE-CORE%3A%2ASTARTUP-TIME%2A"
   "SPECIAL RADIANCE-CORE:*STARTUP-TIME*")
  (("CLASS RADIANCE-CORE:API-ENDPOINT" "RADIANCE-CORE:API-ENDPOINT" "API-ENDPOINT")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3AAPI-ENDPOINT"
   "CLASS RADIANCE-CORE:API-ENDPOINT")
  (("CLASS RADIANCE-CORE:COOKIE" "RADIANCE-CORE:COOKIE" "COOKIE")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3ACOOKIE"
   "CLASS RADIANCE-CORE:COOKIE")
  (("CLASS RADIANCE-CORE:DOCUMENTABLE" "RADIANCE-CORE:DOCUMENTABLE" "DOCUMENTABLE")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3ADOCUMENTABLE"
   "CLASS RADIANCE-CORE:DOCUMENTABLE")
  (("CLASS RADIANCE-CORE:HOOK" "RADIANCE-CORE:HOOK" "HOOK")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3AHOOK"
   "CLASS RADIANCE-CORE:HOOK")
  (("CLASS RADIANCE-CORE:MODULE" "RADIANCE-CORE:MODULE" "MODULE")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3AMODULE"
   "CLASS RADIANCE-CORE:MODULE")
  (("CLASS RADIANCE-CORE:OPTION" "RADIANCE-CORE:OPTION" "OPTION")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3AOPTION"
   "CLASS RADIANCE-CORE:OPTION")
  (("CLASS RADIANCE-CORE:REQUEST" "RADIANCE-CORE:REQUEST" "REQUEST")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3AREQUEST"
   "CLASS RADIANCE-CORE:REQUEST")
  (("CLASS RADIANCE-CORE:RESOURCE-TYPE" "RADIANCE-CORE:RESOURCE-TYPE" "RESOURCE-TYPE")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3ARESOURCE-TYPE"
   "CLASS RADIANCE-CORE:RESOURCE-TYPE")
  (("CLASS RADIANCE-CORE:RESPONSE" "RADIANCE-CORE:RESPONSE" "RESPONSE")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3ARESPONSE"
   "CLASS RADIANCE-CORE:RESPONSE")
  (("CLASS RADIANCE-CORE:ROUTE" "RADIANCE-CORE:ROUTE" "ROUTE")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3AROUTE"
   "CLASS RADIANCE-CORE:ROUTE")
  (("CLASS RADIANCE-CORE:URI" "RADIANCE-CORE:URI" "URI")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3AURI"
   "CLASS RADIANCE-CORE:URI")
  (("CLASS RADIANCE-CORE:URI-DISPATCHER" "RADIANCE-CORE:URI-DISPATCHER" "URI-DISPATCHER")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3AURI-DISPATCHER"
   "CLASS RADIANCE-CORE:URI-DISPATCHER")
  (("CLASS RADIANCE-CORE:VIRTUAL-MODULE" "RADIANCE-CORE:VIRTUAL-MODULE" "VIRTUAL-MODULE")
   "https://shirakumo.github.io/radiance/#CLASS%20RADIANCE-CORE%3AVIRTUAL-MODULE"
   "CLASS RADIANCE-CORE:VIRTUAL-MODULE")
  (("RESOURCE-TYPE RADIANCE-CORE:API" "RADIANCE-CORE:API" "API")
   "https://shirakumo.github.io/radiance/#RESOURCE-TYPE%20RADIANCE-CORE%3AAPI"
   "RESOURCE-TYPE RADIANCE-CORE:API")
  (("RESOURCE-TYPE RADIANCE-CORE:DOMAIN" "RADIANCE-CORE:DOMAIN" "DOMAIN")
   "https://shirakumo.github.io/radiance/#RESOURCE-TYPE%20RADIANCE-CORE%3ADOMAIN"
   "RESOURCE-TYPE RADIANCE-CORE:DOMAIN")
  (("RESOURCE-TYPE RADIANCE-CORE:PAGE" "RADIANCE-CORE:PAGE" "PAGE")
   "https://shirakumo.github.io/radiance/#RESOURCE-TYPE%20RADIANCE-CORE%3APAGE"
   "RESOURCE-TYPE RADIANCE-CORE:PAGE")
  (("RESOURCE-TYPE RADIANCE-CORE:STATIC" "RADIANCE-CORE:STATIC" "STATIC")
   "https://shirakumo.github.io/radiance/#RESOURCE-TYPE%20RADIANCE-CORE%3ASTATIC"
   "RESOURCE-TYPE RADIANCE-CORE:STATIC")
  (("option PAGE RADIANCE-CORE:ACCESS" "PAGE RADIANCE-CORE:ACCESS" "ACCESS")
   "https://shirakumo.github.io/radiance/#option%20PAGE%20RADIANCE-CORE%3AACCESS"
   "Option PAGE RADIANCE-CORE:ACCESS")
  (("option PAGE RADIANCE-CORE:HOOK" "PAGE RADIANCE-CORE:HOOK" "HOOK")
   "https://shirakumo.github.io/radiance/#option%20PAGE%20RADIANCE-CORE%3AHOOK"
   "Option PAGE RADIANCE-CORE:HOOK")
  (("option API RADIANCE-CORE:ACCESS" "API RADIANCE-CORE:ACCESS" "ACCESS")
   "https://shirakumo.github.io/radiance/#option%20API%20RADIANCE-CORE%3AACCESS"
   "Option API RADIANCE-CORE:ACCESS")
  (("ROUTE RADIANCE-CORE:DOMAIN" "RADIANCE-CORE:DOMAIN" "DOMAIN")
   "https://shirakumo.github.io/radiance/#ROUTE%20RADIANCE-CORE%3ADOMAIN"
   "ROUTE RADIANCE-CORE:DOMAIN")
  (("ROUTE RADIANCE-CORE:DOMAIN" "RADIANCE-CORE:DOMAIN" "DOMAIN")
   "https://shirakumo.github.io/radiance/#ROUTE%20RADIANCE-CORE%3ADOMAIN"
   "ROUTE RADIANCE-CORE:DOMAIN")
  (("ROUTE RADIANCE-CORE:VIRTUAL-MODULE" "RADIANCE-CORE:VIRTUAL-MODULE" "VIRTUAL-MODULE")
   "https://shirakumo.github.io/radiance/#ROUTE%20RADIANCE-CORE%3AVIRTUAL-MODULE"
   "ROUTE RADIANCE-CORE:VIRTUAL-MODULE")
  (("ROUTE RADIANCE-CORE:VIRTUAL-MODULE" "RADIANCE-CORE:VIRTUAL-MODULE" "VIRTUAL-MODULE")
   "https://shirakumo.github.io/radiance/#ROUTE%20RADIANCE-CORE%3AVIRTUAL-MODULE"
   "ROUTE RADIANCE-CORE:VIRTUAL-MODULE")
  (("CONDITION RADIANCE-CORE:API-ARGUMENT-INVALID" "RADIANCE-CORE:API-ARGUMENT-INVALID" "API-ARGUMENT-INVALID")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AAPI-ARGUMENT-INVALID"
   "CONDITION RADIANCE-CORE:API-ARGUMENT-INVALID")
  (("CONDITION RADIANCE-CORE:API-ARGUMENT-MISSING" "RADIANCE-CORE:API-ARGUMENT-MISSING" "API-ARGUMENT-MISSING")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AAPI-ARGUMENT-MISSING"
   "CONDITION RADIANCE-CORE:API-ARGUMENT-MISSING")
  (("CONDITION RADIANCE-CORE:API-AUTH-ERROR" "RADIANCE-CORE:API-AUTH-ERROR" "API-AUTH-ERROR")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AAPI-AUTH-ERROR"
   "CONDITION RADIANCE-CORE:API-AUTH-ERROR")
  (("CONDITION RADIANCE-CORE:API-CALL-NOT-FOUND" "RADIANCE-CORE:API-CALL-NOT-FOUND" "API-CALL-NOT-FOUND")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AAPI-CALL-NOT-FOUND"
   "CONDITION RADIANCE-CORE:API-CALL-NOT-FOUND")
  (("CONDITION RADIANCE-CORE:API-ERROR" "RADIANCE-CORE:API-ERROR" "API-ERROR")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AAPI-ERROR"
   "CONDITION RADIANCE-CORE:API-ERROR")
  (("CONDITION RADIANCE-CORE:API-RESPONSE-EMPTY" "RADIANCE-CORE:API-RESPONSE-EMPTY" "API-RESPONSE-EMPTY")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AAPI-RESPONSE-EMPTY"
   "CONDITION RADIANCE-CORE:API-RESPONSE-EMPTY")
  (("CONDITION RADIANCE-CORE:API-UNKNOWN-FORMAT" "RADIANCE-CORE:API-UNKNOWN-FORMAT" "API-UNKNOWN-FORMAT")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AAPI-UNKNOWN-FORMAT"
   "CONDITION RADIANCE-CORE:API-UNKNOWN-FORMAT")
  (("CONDITION RADIANCE-CORE:ENVIRONMENT-NOT-SET" "RADIANCE-CORE:ENVIRONMENT-NOT-SET" "ENVIRONMENT-NOT-SET")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AENVIRONMENT-NOT-SET"
   "CONDITION RADIANCE-CORE:ENVIRONMENT-NOT-SET")
  (("CONDITION RADIANCE-CORE:FILE-TO-SERVE-DOES-NOT-EXIST" "RADIANCE-CORE:FILE-TO-SERVE-DOES-NOT-EXIST" "FILE-TO-SERVE-DOES-NOT-EXIST")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AFILE-TO-SERVE-DOES-NOT-EXIST"
   "CONDITION RADIANCE-CORE:FILE-TO-SERVE-DOES-NOT-EXIST")
  (("CONDITION RADIANCE-CORE:INTERFACE-IMPLEMENTATION-NOT-SET" "RADIANCE-CORE:INTERFACE-IMPLEMENTATION-NOT-SET" "INTERFACE-IMPLEMENTATION-NOT-SET")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AINTERFACE-IMPLEMENTATION-NOT-SET"
   "CONDITION RADIANCE-CORE:INTERFACE-IMPLEMENTATION-NOT-SET")
  (("CONDITION RADIANCE-CORE:INTERNAL-ERROR" "RADIANCE-CORE:INTERNAL-ERROR" "INTERNAL-ERROR")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AINTERNAL-ERROR"
   "CONDITION RADIANCE-CORE:INTERNAL-ERROR")
  (("CONDITION RADIANCE-CORE:NO-SUCH-POST-PARAMETER" "RADIANCE-CORE:NO-SUCH-POST-PARAMETER" "NO-SUCH-POST-PARAMETER")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3ANO-SUCH-POST-PARAMETER"
   "CONDITION RADIANCE-CORE:NO-SUCH-POST-PARAMETER")
  (("CONDITION RADIANCE-CORE:POST-PARAMETER-NOT-A-FILE" "RADIANCE-CORE:POST-PARAMETER-NOT-A-FILE" "POST-PARAMETER-NOT-A-FILE")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3APOST-PARAMETER-NOT-A-FILE"
   "CONDITION RADIANCE-CORE:POST-PARAMETER-NOT-A-FILE")
  (("CONDITION RADIANCE-CORE:RADIANCE-CONDITION" "RADIANCE-CORE:RADIANCE-CONDITION" "RADIANCE-CONDITION")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3ARADIANCE-CONDITION"
   "CONDITION RADIANCE-CORE:RADIANCE-CONDITION")
  (("CONDITION RADIANCE-CORE:REQUEST-DENIED" "RADIANCE-CORE:REQUEST-DENIED" "REQUEST-DENIED")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AREQUEST-DENIED"
   "CONDITION RADIANCE-CORE:REQUEST-DENIED")
  (("CONDITION RADIANCE-CORE:REQUEST-EMPTY" "RADIANCE-CORE:REQUEST-EMPTY" "REQUEST-EMPTY")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AREQUEST-EMPTY"
   "CONDITION RADIANCE-CORE:REQUEST-EMPTY")
  (("CONDITION RADIANCE-CORE:REQUEST-ERROR" "RADIANCE-CORE:REQUEST-ERROR" "REQUEST-ERROR")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AREQUEST-ERROR"
   "CONDITION RADIANCE-CORE:REQUEST-ERROR")
  (("CONDITION RADIANCE-CORE:REQUEST-NOT-FOUND" "RADIANCE-CORE:REQUEST-NOT-FOUND" "REQUEST-NOT-FOUND")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AREQUEST-NOT-FOUND"
   "CONDITION RADIANCE-CORE:REQUEST-NOT-FOUND")
  (("CONDITION RADIANCE-CORE:UNPARSABLE-URI-STRING" "RADIANCE-CORE:UNPARSABLE-URI-STRING" "UNPARSABLE-URI-STRING")
   "https://shirakumo.github.io/radiance/#CONDITION%20RADIANCE-CORE%3AUNPARSABLE-URI-STRING"
   "CONDITION RADIANCE-CORE:UNPARSABLE-URI-STRING")
  (("HOOK RADIANCE-CORE:ENVIRONMENT-CHANGE" "RADIANCE-CORE:ENVIRONMENT-CHANGE" "ENVIRONMENT-CHANGE")
   "https://shirakumo.github.io/radiance/#HOOK%20RADIANCE-CORE%3AENVIRONMENT-CHANGE"
   "HOOK RADIANCE-CORE:ENVIRONMENT-CHANGE")
  (("HOOK RADIANCE-CORE:REQUEST" "RADIANCE-CORE:REQUEST" "REQUEST")
   "https://shirakumo.github.io/radiance/#HOOK%20RADIANCE-CORE%3AREQUEST"
   "HOOK RADIANCE-CORE:REQUEST")
  (("HOOK RADIANCE-CORE:SERVER-READY" "RADIANCE-CORE:SERVER-READY" "SERVER-READY")
   "https://shirakumo.github.io/radiance/#HOOK%20RADIANCE-CORE%3ASERVER-READY"
   "HOOK RADIANCE-CORE:SERVER-READY")
  (("HOOK RADIANCE-CORE:SERVER-SHUTDOWN" "RADIANCE-CORE:SERVER-SHUTDOWN" "SERVER-SHUTDOWN")
   "https://shirakumo.github.io/radiance/#HOOK%20RADIANCE-CORE%3ASERVER-SHUTDOWN"
   "HOOK RADIANCE-CORE:SERVER-SHUTDOWN")
  (("HOOK RADIANCE-CORE:SERVER-START" "RADIANCE-CORE:SERVER-START" "SERVER-START")
   "https://shirakumo.github.io/radiance/#HOOK%20RADIANCE-CORE%3ASERVER-START"
   "HOOK RADIANCE-CORE:SERVER-START")
  (("HOOK RADIANCE-CORE:SERVER-STOP" "RADIANCE-CORE:SERVER-STOP" "SERVER-STOP")
   "https://shirakumo.github.io/radiance/#HOOK%20RADIANCE-CORE%3ASERVER-STOP"
   "HOOK RADIANCE-CORE:SERVER-STOP")
  (("HOOK RADIANCE-CORE:SHUTDOWN" "RADIANCE-CORE:SHUTDOWN" "SHUTDOWN")
   "https://shirakumo.github.io/radiance/#HOOK%20RADIANCE-CORE%3ASHUTDOWN"
   "HOOK RADIANCE-CORE:SHUTDOWN")
  (("HOOK RADIANCE-CORE:SHUTDOWN-DONE" "RADIANCE-CORE:SHUTDOWN-DONE" "SHUTDOWN-DONE")
   "https://shirakumo.github.io/radiance/#HOOK%20RADIANCE-CORE%3ASHUTDOWN-DONE"
   "HOOK RADIANCE-CORE:SHUTDOWN-DONE")
  (("HOOK RADIANCE-CORE:STARTUP" "RADIANCE-CORE:STARTUP" "STARTUP")
   "https://shirakumo.github.io/radiance/#HOOK%20RADIANCE-CORE%3ASTARTUP"
   "HOOK RADIANCE-CORE:STARTUP")
  (("HOOK RADIANCE-CORE:STARTUP-DONE" "RADIANCE-CORE:STARTUP-DONE" "STARTUP-DONE")
   "https://shirakumo.github.io/radiance/#HOOK%20RADIANCE-CORE%3ASTARTUP-DONE"
   "HOOK RADIANCE-CORE:STARTUP-DONE")
  (("ACCESSOR RADIANCE-CORE:API-ENDPOINT" "RADIANCE-CORE:API-ENDPOINT" "API-ENDPOINT")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AAPI-ENDPOINT"
   "ACCESSOR RADIANCE-CORE:API-ENDPOINT")
  (("ACCESSOR RADIANCE-CORE:API-FORMAT" "RADIANCE-CORE:API-FORMAT" "API-FORMAT")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AAPI-FORMAT"
   "ACCESSOR RADIANCE-CORE:API-FORMAT")
  (("ACCESSOR RADIANCE-CORE:ARGSLIST" "RADIANCE-CORE:ARGSLIST" "ARGSLIST")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AARGSLIST"
   "ACCESSOR RADIANCE-CORE:ARGSLIST")
  (("ACCESSOR RADIANCE-CORE:CONTENT-TYPE" "RADIANCE-CORE:CONTENT-TYPE" "CONTENT-TYPE")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ACONTENT-TYPE"
   "ACCESSOR RADIANCE-CORE:CONTENT-TYPE")
  (("ACCESSOR RADIANCE-CORE:COOKIE" "RADIANCE-CORE:COOKIE" "COOKIE")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ACOOKIE"
   "ACCESSOR RADIANCE-CORE:COOKIE")
  (("ACCESSOR RADIANCE-CORE:COOKIES" "RADIANCE-CORE:COOKIES" "COOKIES")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ACOOKIES"
   "ACCESSOR RADIANCE-CORE:COOKIES")
  (("ACCESSOR RADIANCE-CORE:DATA" "RADIANCE-CORE:DATA" "DATA")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ADATA"
   "ACCESSOR RADIANCE-CORE:DATA")
  (("ACCESSOR RADIANCE-CORE:DIRECTION" "RADIANCE-CORE:DIRECTION" "DIRECTION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ADIRECTION"
   "ACCESSOR RADIANCE-CORE:DIRECTION")
  (("ACCESSOR RADIANCE-CORE:DISPATCH-FUNCTION" "RADIANCE-CORE:DISPATCH-FUNCTION" "DISPATCH-FUNCTION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ADISPATCH-FUNCTION"
   "ACCESSOR RADIANCE-CORE:DISPATCH-FUNCTION")
  (("ACCESSOR RADIANCE-CORE:DOMAIN" "RADIANCE-CORE:DOMAIN" "DOMAIN")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ADOMAIN"
   "ACCESSOR RADIANCE-CORE:DOMAIN")
  (("ACCESSOR RADIANCE-CORE:DOMAINS" "RADIANCE-CORE:DOMAINS" "DOMAINS")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ADOMAINS"
   "ACCESSOR RADIANCE-CORE:DOMAINS")
  (("ACCESSOR RADIANCE-CORE:ENVIRONMENT" "RADIANCE-CORE:ENVIRONMENT" "ENVIRONMENT")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AENVIRONMENT"
   "ACCESSOR RADIANCE-CORE:ENVIRONMENT")
  (("ACCESSOR RADIANCE-CORE:EXPANDER" "RADIANCE-CORE:EXPANDER" "EXPANDER")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AEXPANDER"
   "ACCESSOR RADIANCE-CORE:EXPANDER")
  (("ACCESSOR RADIANCE-CORE:EXPIRES" "RADIANCE-CORE:EXPIRES" "EXPIRES")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AEXPIRES"
   "ACCESSOR RADIANCE-CORE:EXPIRES")
  (("ACCESSOR RADIANCE-CORE:EXTERNAL-FORMAT" "RADIANCE-CORE:EXTERNAL-FORMAT" "EXTERNAL-FORMAT")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AEXTERNAL-FORMAT"
   "ACCESSOR RADIANCE-CORE:EXTERNAL-FORMAT")
  (("ACCESSOR RADIANCE-CORE:GET-DATA" "RADIANCE-CORE:GET-DATA" "GET-DATA")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AGET-DATA"
   "ACCESSOR RADIANCE-CORE:GET-DATA")
  (("ACCESSOR RADIANCE-CORE:HANDLER" "RADIANCE-CORE:HANDLER" "HANDLER")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AHANDLER"
   "ACCESSOR RADIANCE-CORE:HANDLER")
  (("ACCESSOR RADIANCE-CORE:HEADER" "RADIANCE-CORE:HEADER" "HEADER")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AHEADER"
   "ACCESSOR RADIANCE-CORE:HEADER")
  (("ACCESSOR RADIANCE-CORE:HEADERS" "RADIANCE-CORE:HEADERS" "HEADERS")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AHEADERS"
   "ACCESSOR RADIANCE-CORE:HEADERS")
  (("ACCESSOR RADIANCE-CORE:HOOK" "RADIANCE-CORE:HOOK" "HOOK")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AHOOK"
   "ACCESSOR RADIANCE-CORE:HOOK")
  (("ACCESSOR RADIANCE-CORE:HTTP-METHOD" "RADIANCE-CORE:HTTP-METHOD" "HTTP-METHOD")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AHTTP-METHOD"
   "ACCESSOR RADIANCE-CORE:HTTP-METHOD")
  (("ACCESSOR RADIANCE-CORE:HTTP-ONLY" "RADIANCE-CORE:HTTP-ONLY" "HTTP-ONLY")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AHTTP-ONLY"
   "ACCESSOR RADIANCE-CORE:HTTP-ONLY")
  (("ACCESSOR RADIANCE-CORE:IMPLEMENTATION" "RADIANCE-CORE:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AIMPLEMENTATION"
   "ACCESSOR RADIANCE-CORE:IMPLEMENTATION")
  (("ACCESSOR RADIANCE-CORE:ISSUE-TIME" "RADIANCE-CORE:ISSUE-TIME" "ISSUE-TIME")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AISSUE-TIME"
   "ACCESSOR RADIANCE-CORE:ISSUE-TIME")
  (("ACCESSOR RADIANCE-CORE:LOCATORS" "RADIANCE-CORE:LOCATORS" "LOCATORS")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ALOCATORS"
   "ACCESSOR RADIANCE-CORE:LOCATORS")
  (("ACCESSOR RADIANCE-CORE:MATCHER" "RADIANCE-CORE:MATCHER" "MATCHER")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AMATCHER"
   "ACCESSOR RADIANCE-CORE:MATCHER")
  (("ACCESSOR RADIANCE-CORE:MCONFIG" "RADIANCE-CORE:MCONFIG" "MCONFIG")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AMCONFIG"
   "ACCESSOR RADIANCE-CORE:MCONFIG")
  (("ACCESSOR RADIANCE-CORE:MESSAGE" "RADIANCE-CORE:MESSAGE" "MESSAGE")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AMESSAGE"
   "ACCESSOR RADIANCE-CORE:MESSAGE")
  (("ACCESSOR RADIANCE-CORE:MODULE-PERMISSIONS" "RADIANCE-CORE:MODULE-PERMISSIONS" "MODULE-PERMISSIONS")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AMODULE-PERMISSIONS"
   "ACCESSOR RADIANCE-CORE:MODULE-PERMISSIONS")
  (("ACCESSOR RADIANCE-CORE:MODULE-STORAGE" "RADIANCE-CORE:MODULE-STORAGE" "MODULE-STORAGE")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AMODULE-STORAGE"
   "ACCESSOR RADIANCE-CORE:MODULE-STORAGE")
  (("ACCESSOR RADIANCE-CORE:NAME" "RADIANCE-CORE:NAME" "NAME")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ANAME"
   "ACCESSOR RADIANCE-CORE:NAME")
  (("ACCESSOR RADIANCE-CORE:OPTION" "RADIANCE-CORE:OPTION" "OPTION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AOPTION"
   "ACCESSOR RADIANCE-CORE:OPTION")
  (("ACCESSOR RADIANCE-CORE:OPTION-TYPE" "RADIANCE-CORE:OPTION-TYPE" "OPTION-TYPE")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AOPTION-TYPE"
   "ACCESSOR RADIANCE-CORE:OPTION-TYPE")
  (("ACCESSOR RADIANCE-CORE:PATH" "RADIANCE-CORE:PATH" "PATH")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3APATH"
   "ACCESSOR RADIANCE-CORE:PATH")
  (("ACCESSOR RADIANCE-CORE:PORT" "RADIANCE-CORE:PORT" "PORT")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3APORT"
   "ACCESSOR RADIANCE-CORE:PORT")
  (("ACCESSOR RADIANCE-CORE:POST-DATA" "RADIANCE-CORE:POST-DATA" "POST-DATA")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3APOST-DATA"
   "ACCESSOR RADIANCE-CORE:POST-DATA")
  (("ACCESSOR RADIANCE-CORE:PRIORITY" "RADIANCE-CORE:PRIORITY" "PRIORITY")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3APRIORITY"
   "ACCESSOR RADIANCE-CORE:PRIORITY")
  (("ACCESSOR RADIANCE-CORE:REFERER" "RADIANCE-CORE:REFERER" "REFERER")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AREFERER"
   "ACCESSOR RADIANCE-CORE:REFERER")
  (("ACCESSOR RADIANCE-CORE:REMOTE" "RADIANCE-CORE:REMOTE" "REMOTE")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AREMOTE"
   "ACCESSOR RADIANCE-CORE:REMOTE")
  (("ACCESSOR RADIANCE-CORE:REQUEST-HANDLER" "RADIANCE-CORE:REQUEST-HANDLER" "REQUEST-HANDLER")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AREQUEST-HANDLER"
   "ACCESSOR RADIANCE-CORE:REQUEST-HANDLER")
  (("ACCESSOR RADIANCE-CORE:RESOURCE-LOCATOR" "RADIANCE-CORE:RESOURCE-LOCATOR" "RESOURCE-LOCATOR")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ARESOURCE-LOCATOR"
   "ACCESSOR RADIANCE-CORE:RESOURCE-LOCATOR")
  (("ACCESSOR RADIANCE-CORE:RESOURCE-TYPE" "RADIANCE-CORE:RESOURCE-TYPE" "RESOURCE-TYPE")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ARESOURCE-TYPE"
   "ACCESSOR RADIANCE-CORE:RESOURCE-TYPE")
  (("ACCESSOR RADIANCE-CORE:RETURN-CODE" "RADIANCE-CORE:RETURN-CODE" "RETURN-CODE")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ARETURN-CODE"
   "ACCESSOR RADIANCE-CORE:RETURN-CODE")
  (("ACCESSOR RADIANCE-CORE:ROUTE" "RADIANCE-CORE:ROUTE" "ROUTE")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AROUTE"
   "ACCESSOR RADIANCE-CORE:ROUTE")
  (("ACCESSOR RADIANCE-CORE:SECURE" "RADIANCE-CORE:SECURE" "SECURE")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ASECURE"
   "ACCESSOR RADIANCE-CORE:SECURE")
  (("ACCESSOR RADIANCE-CORE:TRANSLATOR" "RADIANCE-CORE:TRANSLATOR" "TRANSLATOR")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3ATRANSLATOR"
   "ACCESSOR RADIANCE-CORE:TRANSLATOR")
  (("ACCESSOR RADIANCE-CORE:URI" "RADIANCE-CORE:URI" "URI")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AURI"
   "ACCESSOR RADIANCE-CORE:URI")
  (("ACCESSOR RADIANCE-CORE:URI-DISPATCHER" "RADIANCE-CORE:URI-DISPATCHER" "URI-DISPATCHER")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AURI-DISPATCHER"
   "ACCESSOR RADIANCE-CORE:URI-DISPATCHER")
  (("ACCESSOR RADIANCE-CORE:USER-AGENT" "RADIANCE-CORE:USER-AGENT" "USER-AGENT")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AUSER-AGENT"
   "ACCESSOR RADIANCE-CORE:USER-AGENT")
  (("ACCESSOR RADIANCE-CORE:VALUE" "RADIANCE-CORE:VALUE" "VALUE")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AVALUE"
   "ACCESSOR RADIANCE-CORE:VALUE")
  (("ACCESSOR RADIANCE-CORE:VIRTUAL-MODULE-NAME" "RADIANCE-CORE:VIRTUAL-MODULE-NAME" "VIRTUAL-MODULE-NAME")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RADIANCE-CORE%3AVIRTUAL-MODULE-NAME"
   "ACCESSOR RADIANCE-CORE:VIRTUAL-MODULE-NAME")
  (("FUNCTION RADIANCE-CORE:*REQUEST*" "RADIANCE-CORE:*REQUEST*" "*REQUEST*")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3A%2AREQUEST%2A"
   "FUNCTION RADIANCE-CORE:*REQUEST*")
  (("FUNCTION RADIANCE-CORE:*RESPONSE*" "RADIANCE-CORE:*RESPONSE*" "*RESPONSE*")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3A%2ARESPONSE%2A"
   "FUNCTION RADIANCE-CORE:*RESPONSE*")
  (("FUNCTION RADIANCE-CORE:ABORT-HANDLING" "RADIANCE-CORE:ABORT-HANDLING" "ABORT-HANDLING")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AABORT-HANDLING"
   "FUNCTION RADIANCE-CORE:ABORT-HANDLING")
  (("FUNCTION RADIANCE-CORE:ADD-DOMAIN" "RADIANCE-CORE:ADD-DOMAIN" "ADD-DOMAIN")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AADD-DOMAIN"
   "FUNCTION RADIANCE-CORE:ADD-DOMAIN")
  (("FUNCTION RADIANCE-CORE:API-OUTPUT" "RADIANCE-CORE:API-OUTPUT" "API-OUTPUT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AAPI-OUTPUT"
   "FUNCTION RADIANCE-CORE:API-OUTPUT")
  (("FUNCTION RADIANCE-CORE:CALL-API" "RADIANCE-CORE:CALL-API" "CALL-API")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ACALL-API"
   "FUNCTION RADIANCE-CORE:CALL-API")
  (("FUNCTION RADIANCE-CORE:CALL-API-REQUEST" "RADIANCE-CORE:CALL-API-REQUEST" "CALL-API-REQUEST")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ACALL-API-REQUEST"
   "FUNCTION RADIANCE-CORE:CALL-API-REQUEST")
  (("FUNCTION RADIANCE-CORE:CHECK-ENVIRONMENT" "RADIANCE-CORE:CHECK-ENVIRONMENT" "CHECK-ENVIRONMENT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ACHECK-ENVIRONMENT"
   "FUNCTION RADIANCE-CORE:CHECK-ENVIRONMENT")
  (("FUNCTION RADIANCE-CORE:COOKIE-HEADER" "RADIANCE-CORE:COOKIE-HEADER" "COOKIE-HEADER")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ACOOKIE-HEADER"
   "FUNCTION RADIANCE-CORE:COOKIE-HEADER")
  (("FUNCTION RADIANCE-CORE:COPY-URI" "RADIANCE-CORE:COPY-URI" "COPY-URI")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ACOPY-URI"
   "FUNCTION RADIANCE-CORE:COPY-URI")
  (("FUNCTION RADIANCE-CORE:CREATE-MODULE" "RADIANCE-CORE:CREATE-MODULE" "CREATE-MODULE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ACREATE-MODULE"
   "FUNCTION RADIANCE-CORE:CREATE-MODULE")
  (("FUNCTION RADIANCE-CORE:DEFAULTED-MCONFIG" "RADIANCE-CORE:DEFAULTED-MCONFIG" "DEFAULTED-MCONFIG")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ADEFAULTED-MCONFIG"
   "FUNCTION RADIANCE-CORE:DEFAULTED-MCONFIG")
  (("FUNCTION RADIANCE-CORE:DELETE-MODULE" "RADIANCE-CORE:DELETE-MODULE" "DELETE-MODULE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ADELETE-MODULE"
   "FUNCTION RADIANCE-CORE:DELETE-MODULE")
  (("FUNCTION RADIANCE-CORE:DESCRIBE-MODULE" "RADIANCE-CORE:DESCRIBE-MODULE" "DESCRIBE-MODULE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ADESCRIBE-MODULE"
   "FUNCTION RADIANCE-CORE:DESCRIBE-MODULE")
  (("FUNCTION RADIANCE-CORE:DISPATCH" "RADIANCE-CORE:DISPATCH" "DISPATCH")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ADISPATCH"
   "FUNCTION RADIANCE-CORE:DISPATCH")
  (("FUNCTION RADIANCE-CORE:ENSURE-URI" "RADIANCE-CORE:ENSURE-URI" "ENSURE-URI")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AENSURE-URI"
   "FUNCTION RADIANCE-CORE:ENSURE-URI")
  (("FUNCTION RADIANCE-CORE:EXECUTE-REQUEST" "RADIANCE-CORE:EXECUTE-REQUEST" "EXECUTE-REQUEST")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AEXECUTE-REQUEST"
   "FUNCTION RADIANCE-CORE:EXECUTE-REQUEST")
  (("FUNCTION RADIANCE-CORE:EXPAND-OPTIONS" "RADIANCE-CORE:EXPAND-OPTIONS" "EXPAND-OPTIONS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AEXPAND-OPTIONS"
   "FUNCTION RADIANCE-CORE:EXPAND-OPTIONS")
  (("FUNCTION RADIANCE-CORE:EXTERNAL-URI" "RADIANCE-CORE:EXTERNAL-URI" "EXTERNAL-URI")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AEXTERNAL-URI"
   "FUNCTION RADIANCE-CORE:EXTERNAL-URI")
  (("FUNCTION RADIANCE-CORE:FILE" "RADIANCE-CORE:FILE" "FILE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AFILE"
   "FUNCTION RADIANCE-CORE:FILE")
  (("FUNCTION RADIANCE-CORE:FILE-SIZE" "RADIANCE-CORE:FILE-SIZE" "FILE-SIZE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AFILE-SIZE"
   "FUNCTION RADIANCE-CORE:FILE-SIZE")
  (("FUNCTION RADIANCE-CORE:FIND-ALL-MODULES" "RADIANCE-CORE:FIND-ALL-MODULES" "FIND-ALL-MODULES")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AFIND-ALL-MODULES"
   "FUNCTION RADIANCE-CORE:FIND-ALL-MODULES")
  (("FUNCTION RADIANCE-CORE:FIND-IMPLEMENTATION" "RADIANCE-CORE:FIND-IMPLEMENTATION" "FIND-IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AFIND-IMPLEMENTATION"
   "FUNCTION RADIANCE-CORE:FIND-IMPLEMENTATION")
  (("FUNCTION RADIANCE-CORE:FORMAT-CLOCK-TIME" "RADIANCE-CORE:FORMAT-CLOCK-TIME" "FORMAT-CLOCK-TIME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AFORMAT-CLOCK-TIME"
   "FUNCTION RADIANCE-CORE:FORMAT-CLOCK-TIME")
  (("FUNCTION RADIANCE-CORE:FORMAT-FANCY-DATE" "RADIANCE-CORE:FORMAT-FANCY-DATE" "FORMAT-FANCY-DATE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AFORMAT-FANCY-DATE"
   "FUNCTION RADIANCE-CORE:FORMAT-FANCY-DATE")
  (("FUNCTION RADIANCE-CORE:FORMAT-HUMAN-DATE" "RADIANCE-CORE:FORMAT-HUMAN-DATE" "FORMAT-HUMAN-DATE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AFORMAT-HUMAN-DATE"
   "FUNCTION RADIANCE-CORE:FORMAT-HUMAN-DATE")
  (("FUNCTION RADIANCE-CORE:FORMAT-MACHINE-DATE" "RADIANCE-CORE:FORMAT-MACHINE-DATE" "FORMAT-MACHINE-DATE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AFORMAT-MACHINE-DATE"
   "FUNCTION RADIANCE-CORE:FORMAT-MACHINE-DATE")
  (("FUNCTION RADIANCE-CORE:FORMAT-RELATIVE-TIME" "RADIANCE-CORE:FORMAT-RELATIVE-TIME" "FORMAT-RELATIVE-TIME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AFORMAT-RELATIVE-TIME"
   "FUNCTION RADIANCE-CORE:FORMAT-RELATIVE-TIME")
  (("FUNCTION RADIANCE-CORE:FORMAT-TIME" "RADIANCE-CORE:FORMAT-TIME" "FORMAT-TIME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AFORMAT-TIME"
   "FUNCTION RADIANCE-CORE:FORMAT-TIME")
  (("FUNCTION RADIANCE-CORE:FORMAT-URI" "RADIANCE-CORE:FORMAT-URI" "FORMAT-URI")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AFORMAT-URI"
   "FUNCTION RADIANCE-CORE:FORMAT-URI")
  (("FUNCTION RADIANCE-CORE:GET-UNIX-TIME" "RADIANCE-CORE:GET-UNIX-TIME" "GET-UNIX-TIME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AGET-UNIX-TIME"
   "FUNCTION RADIANCE-CORE:GET-UNIX-TIME")
  (("FUNCTION RADIANCE-CORE:GET-VAR" "RADIANCE-CORE:GET-VAR" "GET-VAR")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AGET-VAR"
   "FUNCTION RADIANCE-CORE:GET-VAR")
  (("FUNCTION RADIANCE-CORE:HANDLE-CONDITION" "RADIANCE-CORE:HANDLE-CONDITION" "HANDLE-CONDITION")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AHANDLE-CONDITION"
   "FUNCTION RADIANCE-CORE:HANDLE-CONDITION")
  (("FUNCTION RADIANCE-CORE:IMPLEMENTS" "RADIANCE-CORE:IMPLEMENTS" "IMPLEMENTS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AIMPLEMENTS"
   "FUNCTION RADIANCE-CORE:IMPLEMENTS")
  (("FUNCTION RADIANCE-CORE:INTERFACE" "RADIANCE-CORE:INTERFACE" "INTERFACE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AINTERFACE"
   "FUNCTION RADIANCE-CORE:INTERFACE")
  (("FUNCTION RADIANCE-CORE:INTERFACE-P" "RADIANCE-CORE:INTERFACE-P" "INTERFACE-P")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AINTERFACE-P"
   "FUNCTION RADIANCE-CORE:INTERFACE-P")
  (("FUNCTION RADIANCE-CORE:INTERNAL-URI" "RADIANCE-CORE:INTERNAL-URI" "INTERNAL-URI")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AINTERNAL-URI"
   "FUNCTION RADIANCE-CORE:INTERNAL-URI")
  (("FUNCTION RADIANCE-CORE:LIST-API-ENDPOINTS" "RADIANCE-CORE:LIST-API-ENDPOINTS" "LIST-API-ENDPOINTS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ALIST-API-ENDPOINTS"
   "FUNCTION RADIANCE-CORE:LIST-API-ENDPOINTS")
  (("FUNCTION RADIANCE-CORE:LIST-API-FORMATS" "RADIANCE-CORE:LIST-API-FORMATS" "LIST-API-FORMATS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ALIST-API-FORMATS"
   "FUNCTION RADIANCE-CORE:LIST-API-FORMATS")
  (("FUNCTION RADIANCE-CORE:LIST-HOOKS" "RADIANCE-CORE:LIST-HOOKS" "LIST-HOOKS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ALIST-HOOKS"
   "FUNCTION RADIANCE-CORE:LIST-HOOKS")
  (("FUNCTION RADIANCE-CORE:LIST-MODULES" "RADIANCE-CORE:LIST-MODULES" "LIST-MODULES")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ALIST-MODULES"
   "FUNCTION RADIANCE-CORE:LIST-MODULES")
  (("FUNCTION RADIANCE-CORE:LIST-OPTIONS" "RADIANCE-CORE:LIST-OPTIONS" "LIST-OPTIONS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ALIST-OPTIONS"
   "FUNCTION RADIANCE-CORE:LIST-OPTIONS")
  (("FUNCTION RADIANCE-CORE:LIST-RESOURCE-TYPES" "RADIANCE-CORE:LIST-RESOURCE-TYPES" "LIST-RESOURCE-TYPES")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ALIST-RESOURCE-TYPES"
   "FUNCTION RADIANCE-CORE:LIST-RESOURCE-TYPES")
  (("FUNCTION RADIANCE-CORE:LIST-ROUTES" "RADIANCE-CORE:LIST-ROUTES" "LIST-ROUTES")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ALIST-ROUTES"
   "FUNCTION RADIANCE-CORE:LIST-ROUTES")
  (("FUNCTION RADIANCE-CORE:LIST-URI-DISPATCHERS" "RADIANCE-CORE:LIST-URI-DISPATCHERS" "LIST-URI-DISPATCHERS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ALIST-URI-DISPATCHERS"
   "FUNCTION RADIANCE-CORE:LIST-URI-DISPATCHERS")
  (("FUNCTION RADIANCE-CORE:LOAD-IMPLEMENTATION" "RADIANCE-CORE:LOAD-IMPLEMENTATION" "LOAD-IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ALOAD-IMPLEMENTATION"
   "FUNCTION RADIANCE-CORE:LOAD-IMPLEMENTATION")
  (("FUNCTION RADIANCE-CORE:MAKE-RANDOM-STRING" "RADIANCE-CORE:MAKE-RANDOM-STRING" "MAKE-RANDOM-STRING")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMAKE-RANDOM-STRING"
   "FUNCTION RADIANCE-CORE:MAKE-RANDOM-STRING")
  (("FUNCTION RADIANCE-CORE:MAKE-URI" "RADIANCE-CORE:MAKE-URI" "MAKE-URI")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMAKE-URI"
   "FUNCTION RADIANCE-CORE:MAKE-URI")
  (("FUNCTION RADIANCE-CORE:MAKE-URL" "RADIANCE-CORE:MAKE-URL" "MAKE-URL")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMAKE-URL"
   "FUNCTION RADIANCE-CORE:MAKE-URL")
  (("FUNCTION RADIANCE-CORE:MCONFIG-PATHNAME" "RADIANCE-CORE:MCONFIG-PATHNAME" "MCONFIG-PATHNAME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMCONFIG-PATHNAME"
   "FUNCTION RADIANCE-CORE:MCONFIG-PATHNAME")
  (("FUNCTION RADIANCE-CORE:MCONFIG-STORAGE" "RADIANCE-CORE:MCONFIG-STORAGE" "MCONFIG-STORAGE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMCONFIG-STORAGE"
   "FUNCTION RADIANCE-CORE:MCONFIG-STORAGE")
  (("FUNCTION RADIANCE-CORE:MERGE-URIS" "RADIANCE-CORE:MERGE-URIS" "MERGE-URIS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMERGE-URIS"
   "FUNCTION RADIANCE-CORE:MERGE-URIS")
  (("FUNCTION RADIANCE-CORE:MODULE" "RADIANCE-CORE:MODULE" "MODULE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMODULE"
   "FUNCTION RADIANCE-CORE:MODULE")
  (("FUNCTION RADIANCE-CORE:MODULE-API-ENDPOINTS" "RADIANCE-CORE:MODULE-API-ENDPOINTS" "MODULE-API-ENDPOINTS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMODULE-API-ENDPOINTS"
   "FUNCTION RADIANCE-CORE:MODULE-API-ENDPOINTS")
  (("FUNCTION RADIANCE-CORE:MODULE-DEPENDENCIES" "RADIANCE-CORE:MODULE-DEPENDENCIES" "MODULE-DEPENDENCIES")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMODULE-DEPENDENCIES"
   "FUNCTION RADIANCE-CORE:MODULE-DEPENDENCIES")
  (("FUNCTION RADIANCE-CORE:MODULE-DOMAIN" "RADIANCE-CORE:MODULE-DOMAIN" "MODULE-DOMAIN")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMODULE-DOMAIN"
   "FUNCTION RADIANCE-CORE:MODULE-DOMAIN")
  (("FUNCTION RADIANCE-CORE:MODULE-IDENTIFIER" "RADIANCE-CORE:MODULE-IDENTIFIER" "MODULE-IDENTIFIER")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMODULE-IDENTIFIER"
   "FUNCTION RADIANCE-CORE:MODULE-IDENTIFIER")
  (("FUNCTION RADIANCE-CORE:MODULE-NAME" "RADIANCE-CORE:MODULE-NAME" "MODULE-NAME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMODULE-NAME"
   "FUNCTION RADIANCE-CORE:MODULE-NAME")
  (("FUNCTION RADIANCE-CORE:MODULE-P" "RADIANCE-CORE:MODULE-P" "MODULE-P")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMODULE-P"
   "FUNCTION RADIANCE-CORE:MODULE-P")
  (("FUNCTION RADIANCE-CORE:MODULE-PAGES" "RADIANCE-CORE:MODULE-PAGES" "MODULE-PAGES")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMODULE-PAGES"
   "FUNCTION RADIANCE-CORE:MODULE-PAGES")
  (("FUNCTION RADIANCE-CORE:MODULE-REQUIRED-INTERFACES" "RADIANCE-CORE:MODULE-REQUIRED-INTERFACES" "MODULE-REQUIRED-INTERFACES")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMODULE-REQUIRED-INTERFACES"
   "FUNCTION RADIANCE-CORE:MODULE-REQUIRED-INTERFACES")
  (("FUNCTION RADIANCE-CORE:MODULE-REQUIRED-SYSTEMS" "RADIANCE-CORE:MODULE-REQUIRED-SYSTEMS" "MODULE-REQUIRED-SYSTEMS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMODULE-REQUIRED-SYSTEMS"
   "FUNCTION RADIANCE-CORE:MODULE-REQUIRED-SYSTEMS")
  (("FUNCTION RADIANCE-CORE:MODULE-STORAGE-REMOVE" "RADIANCE-CORE:MODULE-STORAGE-REMOVE" "MODULE-STORAGE-REMOVE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AMODULE-STORAGE-REMOVE"
   "FUNCTION RADIANCE-CORE:MODULE-STORAGE-REMOVE")
  (("FUNCTION RADIANCE-CORE:PARSE-PATH-SAFELY" "RADIANCE-CORE:PARSE-PATH-SAFELY" "PARSE-PATH-SAFELY")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3APARSE-PATH-SAFELY"
   "FUNCTION RADIANCE-CORE:PARSE-PATH-SAFELY")
  (("FUNCTION RADIANCE-CORE:PARSE-URI" "RADIANCE-CORE:PARSE-URI" "PARSE-URI")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3APARSE-URI"
   "FUNCTION RADIANCE-CORE:PARSE-URI")
  (("FUNCTION RADIANCE-CORE:POST-VAR" "RADIANCE-CORE:POST-VAR" "POST-VAR")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3APOST-VAR"
   "FUNCTION RADIANCE-CORE:POST-VAR")
  (("FUNCTION RADIANCE-CORE:POST/GET" "RADIANCE-CORE:POST/GET" "POST/GET")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3APOST%2FGET"
   "FUNCTION RADIANCE-CORE:POST/GET")
  (("FUNCTION RADIANCE-CORE:REDIRECT" "RADIANCE-CORE:REDIRECT" "REDIRECT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREDIRECT"
   "FUNCTION RADIANCE-CORE:REDIRECT")
  (("FUNCTION RADIANCE-CORE:RELOAD-ENVIRONMENT" "RADIANCE-CORE:RELOAD-ENVIRONMENT" "RELOAD-ENVIRONMENT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ARELOAD-ENVIRONMENT"
   "FUNCTION RADIANCE-CORE:RELOAD-ENVIRONMENT")
  (("FUNCTION RADIANCE-CORE:REMMCONFIG" "RADIANCE-CORE:REMMCONFIG" "REMMCONFIG")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREMMCONFIG"
   "FUNCTION RADIANCE-CORE:REMMCONFIG")
  (("FUNCTION RADIANCE-CORE:REMOVE-API-ENDPOINT" "RADIANCE-CORE:REMOVE-API-ENDPOINT" "REMOVE-API-ENDPOINT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREMOVE-API-ENDPOINT"
   "FUNCTION RADIANCE-CORE:REMOVE-API-ENDPOINT")
  (("FUNCTION RADIANCE-CORE:REMOVE-API-FORMAT" "RADIANCE-CORE:REMOVE-API-FORMAT" "REMOVE-API-FORMAT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREMOVE-API-FORMAT"
   "FUNCTION RADIANCE-CORE:REMOVE-API-FORMAT")
  (("FUNCTION RADIANCE-CORE:REMOVE-DOMAIN" "RADIANCE-CORE:REMOVE-DOMAIN" "REMOVE-DOMAIN")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREMOVE-DOMAIN"
   "FUNCTION RADIANCE-CORE:REMOVE-DOMAIN")
  (("FUNCTION RADIANCE-CORE:REMOVE-HOOK" "RADIANCE-CORE:REMOVE-HOOK" "REMOVE-HOOK")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREMOVE-HOOK"
   "FUNCTION RADIANCE-CORE:REMOVE-HOOK")
  (("FUNCTION RADIANCE-CORE:REMOVE-OPTION" "RADIANCE-CORE:REMOVE-OPTION" "REMOVE-OPTION")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREMOVE-OPTION"
   "FUNCTION RADIANCE-CORE:REMOVE-OPTION")
  (("FUNCTION RADIANCE-CORE:REMOVE-PAGE" "RADIANCE-CORE:REMOVE-PAGE" "REMOVE-PAGE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREMOVE-PAGE"
   "FUNCTION RADIANCE-CORE:REMOVE-PAGE")
  (("FUNCTION RADIANCE-CORE:REMOVE-RESOURCE-TYPE" "RADIANCE-CORE:REMOVE-RESOURCE-TYPE" "REMOVE-RESOURCE-TYPE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREMOVE-RESOURCE-TYPE"
   "FUNCTION RADIANCE-CORE:REMOVE-RESOURCE-TYPE")
  (("FUNCTION RADIANCE-CORE:REMOVE-ROUTE" "RADIANCE-CORE:REMOVE-ROUTE" "REMOVE-ROUTE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREMOVE-ROUTE"
   "FUNCTION RADIANCE-CORE:REMOVE-ROUTE")
  (("FUNCTION RADIANCE-CORE:REMOVE-TRIGGER" "RADIANCE-CORE:REMOVE-TRIGGER" "REMOVE-TRIGGER")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREMOVE-TRIGGER"
   "FUNCTION RADIANCE-CORE:REMOVE-TRIGGER")
  (("FUNCTION RADIANCE-CORE:REMOVE-URI-DISPATCHER" "RADIANCE-CORE:REMOVE-URI-DISPATCHER" "REMOVE-URI-DISPATCHER")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREMOVE-URI-DISPATCHER"
   "FUNCTION RADIANCE-CORE:REMOVE-URI-DISPATCHER")
  (("FUNCTION RADIANCE-CORE:RENDER-ERROR-PAGE" "RADIANCE-CORE:RENDER-ERROR-PAGE" "RENDER-ERROR-PAGE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ARENDER-ERROR-PAGE"
   "FUNCTION RADIANCE-CORE:RENDER-ERROR-PAGE")
  (("FUNCTION RADIANCE-CORE:REPRESENT-URI" "RADIANCE-CORE:REPRESENT-URI" "REPRESENT-URI")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREPRESENT-URI"
   "FUNCTION RADIANCE-CORE:REPRESENT-URI")
  (("FUNCTION RADIANCE-CORE:REQUEST" "RADIANCE-CORE:REQUEST" "REQUEST")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREQUEST"
   "FUNCTION RADIANCE-CORE:REQUEST")
  (("FUNCTION RADIANCE-CORE:REQUEST-RUN-TIME" "RADIANCE-CORE:REQUEST-RUN-TIME" "REQUEST-RUN-TIME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AREQUEST-RUN-TIME"
   "FUNCTION RADIANCE-CORE:REQUEST-RUN-TIME")
  (("FUNCTION RADIANCE-CORE:RESET-INTERFACE" "RADIANCE-CORE:RESET-INTERFACE" "RESET-INTERFACE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ARESET-INTERFACE"
   "FUNCTION RADIANCE-CORE:RESET-INTERFACE")
  (("FUNCTION RADIANCE-CORE:RESOLVE-BASE" "RADIANCE-CORE:RESOLVE-BASE" "RESOLVE-BASE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ARESOLVE-BASE"
   "FUNCTION RADIANCE-CORE:RESOLVE-BASE")
  (("FUNCTION RADIANCE-CORE:RESOURCE" "RADIANCE-CORE:RESOURCE" "RESOURCE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ARESOURCE"
   "FUNCTION RADIANCE-CORE:RESOURCE")
  (("FUNCTION RADIANCE-CORE:SERVE-FILE" "RADIANCE-CORE:SERVE-FILE" "SERVE-FILE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ASERVE-FILE"
   "FUNCTION RADIANCE-CORE:SERVE-FILE")
  (("FUNCTION RADIANCE-CORE:SHUTDOWN" "RADIANCE-CORE:SHUTDOWN" "SHUTDOWN")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ASHUTDOWN"
   "FUNCTION RADIANCE-CORE:SHUTDOWN")
  (("FUNCTION RADIANCE-CORE:STARTED-P" "RADIANCE-CORE:STARTED-P" "STARTED-P")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ASTARTED-P"
   "FUNCTION RADIANCE-CORE:STARTED-P")
  (("FUNCTION RADIANCE-CORE:STARTUP" "RADIANCE-CORE:STARTUP" "STARTUP")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ASTARTUP"
   "FUNCTION RADIANCE-CORE:STARTUP")
  (("FUNCTION RADIANCE-CORE:STATIC-FILE" "RADIANCE-CORE:STATIC-FILE" "STATIC-FILE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ASTATIC-FILE"
   "FUNCTION RADIANCE-CORE:STATIC-FILE")
  (("FUNCTION RADIANCE-CORE:TEMPLATE-FILE" "RADIANCE-CORE:TEMPLATE-FILE" "TEMPLATE-FILE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ATEMPLATE-FILE"
   "FUNCTION RADIANCE-CORE:TEMPLATE-FILE")
  (("FUNCTION RADIANCE-CORE:TRIGGER" "RADIANCE-CORE:TRIGGER" "TRIGGER")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3ATRIGGER"
   "FUNCTION RADIANCE-CORE:TRIGGER")
  (("FUNCTION RADIANCE-CORE:UNIVERSAL-TO-UNIX-TIME" "RADIANCE-CORE:UNIVERSAL-TO-UNIX-TIME" "UNIVERSAL-TO-UNIX-TIME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AUNIVERSAL-TO-UNIX-TIME"
   "FUNCTION RADIANCE-CORE:UNIVERSAL-TO-UNIX-TIME")
  (("FUNCTION RADIANCE-CORE:UNIX-TO-UNIVERSAL-TIME" "RADIANCE-CORE:UNIX-TO-UNIVERSAL-TIME" "UNIX-TO-UNIVERSAL-TIME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AUNIX-TO-UNIVERSAL-TIME"
   "FUNCTION RADIANCE-CORE:UNIX-TO-UNIVERSAL-TIME")
  (("FUNCTION RADIANCE-CORE:UPTIME" "RADIANCE-CORE:UPTIME" "UPTIME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AUPTIME"
   "FUNCTION RADIANCE-CORE:UPTIME")
  (("FUNCTION RADIANCE-CORE:URI-DISPATCHER>" "RADIANCE-CORE:URI-DISPATCHER>" "URI-DISPATCHER>")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AURI-DISPATCHER%3E"
   "FUNCTION RADIANCE-CORE:URI-DISPATCHER>")
  (("FUNCTION RADIANCE-CORE:URI-MATCHES" "RADIANCE-CORE:URI-MATCHES" "URI-MATCHES")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AURI-MATCHES"
   "FUNCTION RADIANCE-CORE:URI-MATCHES")
  (("FUNCTION RADIANCE-CORE:URI-STRING" "RADIANCE-CORE:URI-STRING" "URI-STRING")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AURI-STRING"
   "FUNCTION RADIANCE-CORE:URI-STRING")
  (("FUNCTION RADIANCE-CORE:URI-TO-URL" "RADIANCE-CORE:URI-TO-URL" "URI-TO-URL")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AURI-TO-URL"
   "FUNCTION RADIANCE-CORE:URI-TO-URL")
  (("FUNCTION RADIANCE-CORE:URI<" "RADIANCE-CORE:URI<" "URI<")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AURI%3C"
   "FUNCTION RADIANCE-CORE:URI<")
  (("FUNCTION RADIANCE-CORE:URI=" "RADIANCE-CORE:URI=" "URI=")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AURI%3D"
   "FUNCTION RADIANCE-CORE:URI=")
  (("FUNCTION RADIANCE-CORE:URI>" "RADIANCE-CORE:URI>" "URI>")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AURI%3E"
   "FUNCTION RADIANCE-CORE:URI>")
  (("FUNCTION RADIANCE-CORE:URL-ENCODE" "RADIANCE-CORE:URL-ENCODE" "URL-ENCODE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AURL-ENCODE"
   "FUNCTION RADIANCE-CORE:URL-ENCODE")
  (("FUNCTION RADIANCE-CORE:VIRTUAL-MODULE" "RADIANCE-CORE:VIRTUAL-MODULE" "VIRTUAL-MODULE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RADIANCE-CORE%3AVIRTUAL-MODULE"
   "FUNCTION RADIANCE-CORE:VIRTUAL-MODULE")
  (("URI-DISPATCHER RADIANCE-CORE:API" "RADIANCE-CORE:API" "API")
   "https://shirakumo.github.io/radiance/#URI-DISPATCHER%20RADIANCE-CORE%3AAPI"
   "URI-DISPATCHER RADIANCE-CORE:API")
  (("URI-DISPATCHER RADIANCE-CORE:FAVICON" "RADIANCE-CORE:FAVICON" "FAVICON")
   "https://shirakumo.github.io/radiance/#URI-DISPATCHER%20RADIANCE-CORE%3AFAVICON"
   "URI-DISPATCHER RADIANCE-CORE:FAVICON")
  (("URI-DISPATCHER RADIANCE-CORE:ROBOTS" "RADIANCE-CORE:ROBOTS" "ROBOTS")
   "https://shirakumo.github.io/radiance/#URI-DISPATCHER%20RADIANCE-CORE%3AROBOTS"
   "URI-DISPATCHER RADIANCE-CORE:ROBOTS")
  (("URI-DISPATCHER RADIANCE-CORE:STATIC" "RADIANCE-CORE:STATIC" "STATIC")
   "https://shirakumo.github.io/radiance/#URI-DISPATCHER%20RADIANCE-CORE%3ASTATIC"
   "URI-DISPATCHER RADIANCE-CORE:STATIC")
  (("GENERIC RADIANCE-CORE:API-SERIALIZE" "RADIANCE-CORE:API-SERIALIZE" "API-SERIALIZE")
   "https://shirakumo.github.io/radiance/#GENERIC%20RADIANCE-CORE%3AAPI-SERIALIZE"
   "GENERIC RADIANCE-CORE:API-SERIALIZE")
  (("MACRO RADIANCE-CORE:@STATIC" "RADIANCE-CORE:@STATIC" "@STATIC")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3A%40STATIC"
   "MACRO RADIANCE-CORE:@STATIC")
  (("MACRO RADIANCE-CORE:@TEMPLATE" "RADIANCE-CORE:@TEMPLATE" "@TEMPLATE")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3A%40TEMPLATE"
   "MACRO RADIANCE-CORE:@TEMPLATE")
  (("MACRO RADIANCE-CORE:API-ERROR" "RADIANCE-CORE:API-ERROR" "API-ERROR")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3AAPI-ERROR"
   "MACRO RADIANCE-CORE:API-ERROR")
  (("MACRO RADIANCE-CORE:CONFIG" "RADIANCE-CORE:CONFIG" "CONFIG")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ACONFIG"
   "MACRO RADIANCE-CORE:CONFIG")
  (("MACRO RADIANCE-CORE:CURRENT-MODULE" "RADIANCE-CORE:CURRENT-MODULE" "CURRENT-MODULE")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ACURRENT-MODULE"
   "MACRO RADIANCE-CORE:CURRENT-MODULE")
  (("MACRO RADIANCE-CORE:DEFAULTED-CONFIG" "RADIANCE-CORE:DEFAULTED-CONFIG" "DEFAULTED-CONFIG")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFAULTED-CONFIG"
   "MACRO RADIANCE-CORE:DEFAULTED-CONFIG")
  (("MACRO RADIANCE-CORE:DEFINE-API" "RADIANCE-CORE:DEFINE-API" "DEFINE-API")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-API"
   "MACRO RADIANCE-CORE:DEFINE-API")
  (("MACRO RADIANCE-CORE:DEFINE-API-FORMAT" "RADIANCE-CORE:DEFINE-API-FORMAT" "DEFINE-API-FORMAT")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-API-FORMAT"
   "MACRO RADIANCE-CORE:DEFINE-API-FORMAT")
  (("MACRO RADIANCE-CORE:DEFINE-DOCUMENTABLE" "RADIANCE-CORE:DEFINE-DOCUMENTABLE" "DEFINE-DOCUMENTABLE")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-DOCUMENTABLE"
   "MACRO RADIANCE-CORE:DEFINE-DOCUMENTABLE")
  (("MACRO RADIANCE-CORE:DEFINE-HOOK" "RADIANCE-CORE:DEFINE-HOOK" "DEFINE-HOOK")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-HOOK"
   "MACRO RADIANCE-CORE:DEFINE-HOOK")
  (("MACRO RADIANCE-CORE:DEFINE-HOOK-SWITCH" "RADIANCE-CORE:DEFINE-HOOK-SWITCH" "DEFINE-HOOK-SWITCH")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-HOOK-SWITCH"
   "MACRO RADIANCE-CORE:DEFINE-HOOK-SWITCH")
  (("MACRO RADIANCE-CORE:DEFINE-IMPLEMENT-TRIGGER" "RADIANCE-CORE:DEFINE-IMPLEMENT-TRIGGER" "DEFINE-IMPLEMENT-TRIGGER")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-IMPLEMENT-TRIGGER"
   "MACRO RADIANCE-CORE:DEFINE-IMPLEMENT-TRIGGER")
  (("MACRO RADIANCE-CORE:DEFINE-INTERFACE" "RADIANCE-CORE:DEFINE-INTERFACE" "DEFINE-INTERFACE")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-INTERFACE"
   "MACRO RADIANCE-CORE:DEFINE-INTERFACE")
  (("MACRO RADIANCE-CORE:DEFINE-INTERFACE-EXTENSION" "RADIANCE-CORE:DEFINE-INTERFACE-EXTENSION" "DEFINE-INTERFACE-EXTENSION")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-INTERFACE-EXTENSION"
   "MACRO RADIANCE-CORE:DEFINE-INTERFACE-EXTENSION")
  (("MACRO RADIANCE-CORE:DEFINE-MATCHING-ROUTE" "RADIANCE-CORE:DEFINE-MATCHING-ROUTE" "DEFINE-MATCHING-ROUTE")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-MATCHING-ROUTE"
   "MACRO RADIANCE-CORE:DEFINE-MATCHING-ROUTE")
  (("MACRO RADIANCE-CORE:DEFINE-MODULE" "RADIANCE-CORE:DEFINE-MODULE" "DEFINE-MODULE")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-MODULE"
   "MACRO RADIANCE-CORE:DEFINE-MODULE")
  (("MACRO RADIANCE-CORE:DEFINE-MODULE-EXTENSION" "RADIANCE-CORE:DEFINE-MODULE-EXTENSION" "DEFINE-MODULE-EXTENSION")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-MODULE-EXTENSION"
   "MACRO RADIANCE-CORE:DEFINE-MODULE-EXTENSION")
  (("MACRO RADIANCE-CORE:DEFINE-OPTION" "RADIANCE-CORE:DEFINE-OPTION" "DEFINE-OPTION")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-OPTION"
   "MACRO RADIANCE-CORE:DEFINE-OPTION")
  (("MACRO RADIANCE-CORE:DEFINE-PAGE" "RADIANCE-CORE:DEFINE-PAGE" "DEFINE-PAGE")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-PAGE"
   "MACRO RADIANCE-CORE:DEFINE-PAGE")
  (("MACRO RADIANCE-CORE:DEFINE-RESOURCE-LOCATOR" "RADIANCE-CORE:DEFINE-RESOURCE-LOCATOR" "DEFINE-RESOURCE-LOCATOR")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-RESOURCE-LOCATOR"
   "MACRO RADIANCE-CORE:DEFINE-RESOURCE-LOCATOR")
  (("MACRO RADIANCE-CORE:DEFINE-RESOURCE-TYPE" "RADIANCE-CORE:DEFINE-RESOURCE-TYPE" "DEFINE-RESOURCE-TYPE")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-RESOURCE-TYPE"
   "MACRO RADIANCE-CORE:DEFINE-RESOURCE-TYPE")
  (("MACRO RADIANCE-CORE:DEFINE-ROUTE" "RADIANCE-CORE:DEFINE-ROUTE" "DEFINE-ROUTE")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-ROUTE"
   "MACRO RADIANCE-CORE:DEFINE-ROUTE")
  (("MACRO RADIANCE-CORE:DEFINE-STRING-ROUTE" "RADIANCE-CORE:DEFINE-STRING-ROUTE" "DEFINE-STRING-ROUTE")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-STRING-ROUTE"
   "MACRO RADIANCE-CORE:DEFINE-STRING-ROUTE")
  (("MACRO RADIANCE-CORE:DEFINE-TARGET-ROUTE" "RADIANCE-CORE:DEFINE-TARGET-ROUTE" "DEFINE-TARGET-ROUTE")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-TARGET-ROUTE"
   "MACRO RADIANCE-CORE:DEFINE-TARGET-ROUTE")
  (("MACRO RADIANCE-CORE:DEFINE-TRIGGER" "RADIANCE-CORE:DEFINE-TRIGGER" "DEFINE-TRIGGER")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-TRIGGER"
   "MACRO RADIANCE-CORE:DEFINE-TRIGGER")
  (("MACRO RADIANCE-CORE:DEFINE-URI-DISPATCHER" "RADIANCE-CORE:DEFINE-URI-DISPATCHER" "DEFINE-URI-DISPATCHER")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3ADEFINE-URI-DISPATCHER"
   "MACRO RADIANCE-CORE:DEFINE-URI-DISPATCHER")
  (("MACRO RADIANCE-CORE:OR*" "RADIANCE-CORE:OR*" "OR*")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3AOR%2A"
   "MACRO RADIANCE-CORE:OR*")
  (("MACRO RADIANCE-CORE:PERM" "RADIANCE-CORE:PERM" "PERM")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3APERM"
   "MACRO RADIANCE-CORE:PERM")
  (("MACRO RADIANCE-CORE:REMCONFIG" "RADIANCE-CORE:REMCONFIG" "REMCONFIG")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3AREMCONFIG"
   "MACRO RADIANCE-CORE:REMCONFIG")
  (("MACRO RADIANCE-CORE:WITH-ACTIONS" "RADIANCE-CORE:WITH-ACTIONS" "WITH-ACTIONS")
   "https://shirakumo.github.io/radiance/#MACRO%20RADIANCE-CORE%3AWITH-ACTIONS"
   "MACRO RADIANCE-CORE:WITH-ACTIONS")
  (("SPECIAL ADMIN:*IMPLEMENTATION*" "ADMIN:*IMPLEMENTATION*" "*IMPLEMENTATION*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20ADMIN%3A%2AIMPLEMENTATION%2A"
   "SPECIAL ADMIN:*IMPLEMENTATION*")
  (("RESOURCE-TYPE ADMIN:PAGE" "ADMIN:PAGE" "PAGE")
   "https://shirakumo.github.io/radiance/#RESOURCE-TYPE%20ADMIN%3APAGE"
   "RESOURCE-TYPE ADMIN:PAGE")
  (("option PANEL ADMIN:ACCESS" "PANEL ADMIN:ACCESS" "ACCESS")
   "https://shirakumo.github.io/radiance/#option%20PANEL%20ADMIN%3AACCESS"
   "Option PANEL ADMIN:ACCESS")
  (("HOOK ADMIN:IMPLEMENTED" "ADMIN:IMPLEMENTED" "IMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20ADMIN%3AIMPLEMENTED"
   "HOOK ADMIN:IMPLEMENTED")
  (("HOOK ADMIN:UNIMPLEMENTED" "ADMIN:UNIMPLEMENTED" "UNIMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20ADMIN%3AUNIMPLEMENTED"
   "HOOK ADMIN:UNIMPLEMENTED")
  (("ACCESSOR ADMIN:IMPLEMENTATION" "ADMIN:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20ADMIN%3AIMPLEMENTATION"
   "ACCESSOR ADMIN:IMPLEMENTATION")
  (("FUNCTION ADMIN:LIST-PANELS" "ADMIN:LIST-PANELS" "LIST-PANELS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20ADMIN%3ALIST-PANELS"
   "FUNCTION ADMIN:LIST-PANELS")
  (("FUNCTION ADMIN:REMOVE-PANEL" "ADMIN:REMOVE-PANEL" "REMOVE-PANEL")
   "https://shirakumo.github.io/radiance/#FUNCTION%20ADMIN%3AREMOVE-PANEL"
   "FUNCTION ADMIN:REMOVE-PANEL")
  (("MACRO ADMIN:DEFINE-PANEL" "ADMIN:DEFINE-PANEL" "DEFINE-PANEL")
   "https://shirakumo.github.io/radiance/#MACRO%20ADMIN%3ADEFINE-PANEL"
   "MACRO ADMIN:DEFINE-PANEL")
  (("SPECIAL AUTH:*IMPLEMENTATION*" "AUTH:*IMPLEMENTATION*" "*IMPLEMENTATION*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20AUTH%3A%2AIMPLEMENTATION%2A"
   "SPECIAL AUTH:*IMPLEMENTATION*")
  (("SPECIAL AUTH:*LOGIN-TIMEOUT*" "AUTH:*LOGIN-TIMEOUT*" "*LOGIN-TIMEOUT*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20AUTH%3A%2ALOGIN-TIMEOUT%2A"
   "SPECIAL AUTH:*LOGIN-TIMEOUT*")
  (("RESOURCE-TYPE AUTH:PAGE" "AUTH:PAGE" "PAGE")
   "https://shirakumo.github.io/radiance/#RESOURCE-TYPE%20AUTH%3APAGE"
   "RESOURCE-TYPE AUTH:PAGE")
  (("HOOK AUTH:ASSOCIATE" "AUTH:ASSOCIATE" "ASSOCIATE")
   "https://shirakumo.github.io/radiance/#HOOK%20AUTH%3AASSOCIATE"
   "HOOK AUTH:ASSOCIATE")
  (("HOOK AUTH:IMPLEMENTED" "AUTH:IMPLEMENTED" "IMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20AUTH%3AIMPLEMENTED"
   "HOOK AUTH:IMPLEMENTED")
  (("HOOK AUTH:UNIMPLEMENTED" "AUTH:UNIMPLEMENTED" "UNIMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20AUTH%3AUNIMPLEMENTED"
   "HOOK AUTH:UNIMPLEMENTED")
  (("ACCESSOR AUTH:IMPLEMENTATION" "AUTH:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20AUTH%3AIMPLEMENTATION"
   "ACCESSOR AUTH:IMPLEMENTATION")
  (("FUNCTION AUTH:ASSOCIATE" "AUTH:ASSOCIATE" "ASSOCIATE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20AUTH%3AASSOCIATE"
   "FUNCTION AUTH:ASSOCIATE")
  (("FUNCTION AUTH:CURRENT" "AUTH:CURRENT" "CURRENT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20AUTH%3ACURRENT"
   "FUNCTION AUTH:CURRENT")
  (("SPECIAL BAN:*IMPLEMENTATION*" "BAN:*IMPLEMENTATION*" "*IMPLEMENTATION*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20BAN%3A%2AIMPLEMENTATION%2A"
   "SPECIAL BAN:*IMPLEMENTATION*")
  (("HOOK BAN:IMPLEMENTED" "BAN:IMPLEMENTED" "IMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20BAN%3AIMPLEMENTED"
   "HOOK BAN:IMPLEMENTED")
  (("HOOK BAN:UNIMPLEMENTED" "BAN:UNIMPLEMENTED" "UNIMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20BAN%3AUNIMPLEMENTED"
   "HOOK BAN:UNIMPLEMENTED")
  (("ACCESSOR BAN:IMPLEMENTATION" "BAN:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20BAN%3AIMPLEMENTATION"
   "ACCESSOR BAN:IMPLEMENTATION")
  (("FUNCTION BAN:JAIL" "BAN:JAIL" "JAIL")
   "https://shirakumo.github.io/radiance/#FUNCTION%20BAN%3AJAIL"
   "FUNCTION BAN:JAIL")
  (("FUNCTION BAN:JAIL-TIME" "BAN:JAIL-TIME" "JAIL-TIME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20BAN%3AJAIL-TIME"
   "FUNCTION BAN:JAIL-TIME")
  (("FUNCTION BAN:LIST" "BAN:LIST" "LIST")
   "https://shirakumo.github.io/radiance/#FUNCTION%20BAN%3ALIST"
   "FUNCTION BAN:LIST")
  (("FUNCTION BAN:RELEASE" "BAN:RELEASE" "RELEASE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20BAN%3ARELEASE"
   "FUNCTION BAN:RELEASE")
  (("SPECIAL CACHE:*IMPLEMENTATION*" "CACHE:*IMPLEMENTATION*" "*IMPLEMENTATION*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20CACHE%3A%2AIMPLEMENTATION%2A"
   "SPECIAL CACHE:*IMPLEMENTATION*")
  (("HOOK CACHE:IMPLEMENTED" "CACHE:IMPLEMENTED" "IMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20CACHE%3AIMPLEMENTED"
   "HOOK CACHE:IMPLEMENTED")
  (("HOOK CACHE:UNIMPLEMENTED" "CACHE:UNIMPLEMENTED" "UNIMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20CACHE%3AUNIMPLEMENTED"
   "HOOK CACHE:UNIMPLEMENTED")
  (("ACCESSOR CACHE:IMPLEMENTATION" "CACHE:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20CACHE%3AIMPLEMENTATION"
   "ACCESSOR CACHE:IMPLEMENTATION")
  (("FUNCTION CACHE:GET" "CACHE:GET" "GET")
   "https://shirakumo.github.io/radiance/#FUNCTION%20CACHE%3AGET"
   "FUNCTION CACHE:GET")
  (("FUNCTION CACHE:RENEW" "CACHE:RENEW" "RENEW")
   "https://shirakumo.github.io/radiance/#FUNCTION%20CACHE%3ARENEW"
   "FUNCTION CACHE:RENEW")
  (("MACRO CACHE:WITH-CACHE" "CACHE:WITH-CACHE" "WITH-CACHE")
   "https://shirakumo.github.io/radiance/#MACRO%20CACHE%3AWITH-CACHE"
   "MACRO CACHE:WITH-CACHE")
  (("SPECIAL DATABASE:*IMPLEMENTATION*" "DATABASE:*IMPLEMENTATION*" "*IMPLEMENTATION*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20DATABASE%3A%2AIMPLEMENTATION%2A"
   "SPECIAL DATABASE:*IMPLEMENTATION*")
  (("CONDITION DATABASE:COLLECTION-ALREADY-EXISTS" "DATABASE:COLLECTION-ALREADY-EXISTS" "COLLECTION-ALREADY-EXISTS")
   "https://shirakumo.github.io/radiance/#CONDITION%20DATABASE%3ACOLLECTION-ALREADY-EXISTS"
   "CONDITION DATABASE:COLLECTION-ALREADY-EXISTS")
  (("CONDITION DATABASE:COLLECTION-CONDITION" "DATABASE:COLLECTION-CONDITION" "COLLECTION-CONDITION")
   "https://shirakumo.github.io/radiance/#CONDITION%20DATABASE%3ACOLLECTION-CONDITION"
   "CONDITION DATABASE:COLLECTION-CONDITION")
  (("CONDITION DATABASE:CONDITION" "DATABASE:CONDITION" "CONDITION")
   "https://shirakumo.github.io/radiance/#CONDITION%20DATABASE%3ACONDITION"
   "CONDITION DATABASE:CONDITION")
  (("CONDITION DATABASE:CONNECTION-ALREADY-OPEN" "DATABASE:CONNECTION-ALREADY-OPEN" "CONNECTION-ALREADY-OPEN")
   "https://shirakumo.github.io/radiance/#CONDITION%20DATABASE%3ACONNECTION-ALREADY-OPEN"
   "CONDITION DATABASE:CONNECTION-ALREADY-OPEN")
  (("CONDITION DATABASE:CONNECTION-FAILED" "DATABASE:CONNECTION-FAILED" "CONNECTION-FAILED")
   "https://shirakumo.github.io/radiance/#CONDITION%20DATABASE%3ACONNECTION-FAILED"
   "CONDITION DATABASE:CONNECTION-FAILED")
  (("CONDITION DATABASE:ID" "DATABASE:ID" "ID")
   "https://shirakumo.github.io/radiance/#CONDITION%20DATABASE%3AID"
   "CONDITION DATABASE:ID")
  (("CONDITION DATABASE:INVALID-COLLECTION" "DATABASE:INVALID-COLLECTION" "INVALID-COLLECTION")
   "https://shirakumo.github.io/radiance/#CONDITION%20DATABASE%3AINVALID-COLLECTION"
   "CONDITION DATABASE:INVALID-COLLECTION")
  (("CONDITION DATABASE:INVALID-FIELD" "DATABASE:INVALID-FIELD" "INVALID-FIELD")
   "https://shirakumo.github.io/radiance/#CONDITION%20DATABASE%3AINVALID-FIELD"
   "CONDITION DATABASE:INVALID-FIELD")
  (("STRUCTURE DATABASE:ID" "DATABASE:ID" "ID")
   "https://shirakumo.github.io/radiance/#STRUCTURE%20DATABASE%3AID"
   "STRUCTURE DATABASE:ID")
  (("HOOK DATABASE:CONNECTED" "DATABASE:CONNECTED" "CONNECTED")
   "https://shirakumo.github.io/radiance/#HOOK%20DATABASE%3ACONNECTED"
   "HOOK DATABASE:CONNECTED")
  (("HOOK DATABASE:DISCONNECTED" "DATABASE:DISCONNECTED" "DISCONNECTED")
   "https://shirakumo.github.io/radiance/#HOOK%20DATABASE%3ADISCONNECTED"
   "HOOK DATABASE:DISCONNECTED")
  (("HOOK DATABASE:IMPLEMENTED" "DATABASE:IMPLEMENTED" "IMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20DATABASE%3AIMPLEMENTED"
   "HOOK DATABASE:IMPLEMENTED")
  (("HOOK DATABASE:UNIMPLEMENTED" "DATABASE:UNIMPLEMENTED" "UNIMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20DATABASE%3AUNIMPLEMENTED"
   "HOOK DATABASE:UNIMPLEMENTED")
  (("ACCESSOR DATABASE:IMPLEMENTATION" "DATABASE:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20DATABASE%3AIMPLEMENTATION"
   "ACCESSOR DATABASE:IMPLEMENTATION")
  (("FUNCTION DATABASE:COLLECTION-EXISTS-P" "DATABASE:COLLECTION-EXISTS-P" "COLLECTION-EXISTS-P")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3ACOLLECTION-EXISTS-P"
   "FUNCTION DATABASE:COLLECTION-EXISTS-P")
  (("FUNCTION DATABASE:COLLECTIONS" "DATABASE:COLLECTIONS" "COLLECTIONS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3ACOLLECTIONS"
   "FUNCTION DATABASE:COLLECTIONS")
  (("FUNCTION DATABASE:CONNECT" "DATABASE:CONNECT" "CONNECT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3ACONNECT"
   "FUNCTION DATABASE:CONNECT")
  (("FUNCTION DATABASE:CONNECTED-P" "DATABASE:CONNECTED-P" "CONNECTED-P")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3ACONNECTED-P"
   "FUNCTION DATABASE:CONNECTED-P")
  (("FUNCTION DATABASE:COUNT" "DATABASE:COUNT" "COUNT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3ACOUNT"
   "FUNCTION DATABASE:COUNT")
  (("FUNCTION DATABASE:CREATE" "DATABASE:CREATE" "CREATE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3ACREATE"
   "FUNCTION DATABASE:CREATE")
  (("FUNCTION DATABASE:DISCONNECT" "DATABASE:DISCONNECT" "DISCONNECT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3ADISCONNECT"
   "FUNCTION DATABASE:DISCONNECT")
  (("FUNCTION DATABASE:DROP" "DATABASE:DROP" "DROP")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3ADROP"
   "FUNCTION DATABASE:DROP")
  (("FUNCTION DATABASE:EMPTY" "DATABASE:EMPTY" "EMPTY")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3AEMPTY"
   "FUNCTION DATABASE:EMPTY")
  (("FUNCTION DATABASE:ENSURE-ID" "DATABASE:ENSURE-ID" "ENSURE-ID")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3AENSURE-ID"
   "FUNCTION DATABASE:ENSURE-ID")
  (("FUNCTION DATABASE:INSERT" "DATABASE:INSERT" "INSERT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3AINSERT"
   "FUNCTION DATABASE:INSERT")
  (("FUNCTION DATABASE:ITERATE" "DATABASE:ITERATE" "ITERATE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3AITERATE"
   "FUNCTION DATABASE:ITERATE")
  (("FUNCTION DATABASE:REMOVE" "DATABASE:REMOVE" "REMOVE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3AREMOVE"
   "FUNCTION DATABASE:REMOVE")
  (("FUNCTION DATABASE:SELECT" "DATABASE:SELECT" "SELECT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3ASELECT"
   "FUNCTION DATABASE:SELECT")
  (("FUNCTION DATABASE:STRUCTURE" "DATABASE:STRUCTURE" "STRUCTURE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3ASTRUCTURE"
   "FUNCTION DATABASE:STRUCTURE")
  (("FUNCTION DATABASE:UPDATE" "DATABASE:UPDATE" "UPDATE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20DATABASE%3AUPDATE"
   "FUNCTION DATABASE:UPDATE")
  (("MACRO DATABASE:QUERY" "DATABASE:QUERY" "QUERY")
   "https://shirakumo.github.io/radiance/#MACRO%20DATABASE%3AQUERY"
   "MACRO DATABASE:QUERY")
  (("MACRO DATABASE:WITH-TRANSACTION" "DATABASE:WITH-TRANSACTION" "WITH-TRANSACTION")
   "https://shirakumo.github.io/radiance/#MACRO%20DATABASE%3AWITH-TRANSACTION"
   "MACRO DATABASE:WITH-TRANSACTION")
  (("SPECIAL LOGGER:*IMPLEMENTATION*" "LOGGER:*IMPLEMENTATION*" "*IMPLEMENTATION*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20LOGGER%3A%2AIMPLEMENTATION%2A"
   "SPECIAL LOGGER:*IMPLEMENTATION*")
  (("HOOK LOGGER:IMPLEMENTED" "LOGGER:IMPLEMENTED" "IMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20LOGGER%3AIMPLEMENTED"
   "HOOK LOGGER:IMPLEMENTED")
  (("HOOK LOGGER:UNIMPLEMENTED" "LOGGER:UNIMPLEMENTED" "UNIMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20LOGGER%3AUNIMPLEMENTED"
   "HOOK LOGGER:UNIMPLEMENTED")
  (("ACCESSOR LOGGER:IMPLEMENTATION" "LOGGER:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20LOGGER%3AIMPLEMENTATION"
   "ACCESSOR LOGGER:IMPLEMENTATION")
  (("FUNCTION LOGGER:DEBUG" "LOGGER:DEBUG" "DEBUG")
   "https://shirakumo.github.io/radiance/#FUNCTION%20LOGGER%3ADEBUG"
   "FUNCTION LOGGER:DEBUG")
  (("FUNCTION LOGGER:ERROR" "LOGGER:ERROR" "ERROR")
   "https://shirakumo.github.io/radiance/#FUNCTION%20LOGGER%3AERROR"
   "FUNCTION LOGGER:ERROR")
  (("FUNCTION LOGGER:FATAL" "LOGGER:FATAL" "FATAL")
   "https://shirakumo.github.io/radiance/#FUNCTION%20LOGGER%3AFATAL"
   "FUNCTION LOGGER:FATAL")
  (("FUNCTION LOGGER:INFO" "LOGGER:INFO" "INFO")
   "https://shirakumo.github.io/radiance/#FUNCTION%20LOGGER%3AINFO"
   "FUNCTION LOGGER:INFO")
  (("FUNCTION LOGGER:LOG" "LOGGER:LOG" "LOG")
   "https://shirakumo.github.io/radiance/#FUNCTION%20LOGGER%3ALOG"
   "FUNCTION LOGGER:LOG")
  (("FUNCTION LOGGER:SEVERE" "LOGGER:SEVERE" "SEVERE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20LOGGER%3ASEVERE"
   "FUNCTION LOGGER:SEVERE")
  (("FUNCTION LOGGER:TRACE" "LOGGER:TRACE" "TRACE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20LOGGER%3ATRACE"
   "FUNCTION LOGGER:TRACE")
  (("FUNCTION LOGGER:WARN" "LOGGER:WARN" "WARN")
   "https://shirakumo.github.io/radiance/#FUNCTION%20LOGGER%3AWARN"
   "FUNCTION LOGGER:WARN")
  (("SPECIAL MAIL:*IMPLEMENTATION*" "MAIL:*IMPLEMENTATION*" "*IMPLEMENTATION*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20MAIL%3A%2AIMPLEMENTATION%2A"
   "SPECIAL MAIL:*IMPLEMENTATION*")
  (("HOOK MAIL:IMPLEMENTED" "MAIL:IMPLEMENTED" "IMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20MAIL%3AIMPLEMENTED"
   "HOOK MAIL:IMPLEMENTED")
  (("HOOK MAIL:SEND" "MAIL:SEND" "SEND")
   "https://shirakumo.github.io/radiance/#HOOK%20MAIL%3ASEND"
   "HOOK MAIL:SEND")
  (("HOOK MAIL:UNIMPLEMENTED" "MAIL:UNIMPLEMENTED" "UNIMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20MAIL%3AUNIMPLEMENTED"
   "HOOK MAIL:UNIMPLEMENTED")
  (("ACCESSOR MAIL:IMPLEMENTATION" "MAIL:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20MAIL%3AIMPLEMENTATION"
   "ACCESSOR MAIL:IMPLEMENTATION")
  (("FUNCTION MAIL:SEND" "MAIL:SEND" "SEND")
   "https://shirakumo.github.io/radiance/#FUNCTION%20MAIL%3ASEND"
   "FUNCTION MAIL:SEND")
  (("SPECIAL PROFILE:*IMPLEMENTATION*" "PROFILE:*IMPLEMENTATION*" "*IMPLEMENTATION*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20PROFILE%3A%2AIMPLEMENTATION%2A"
   "SPECIAL PROFILE:*IMPLEMENTATION*")
  (("RESOURCE-TYPE PROFILE:PAGE" "PROFILE:PAGE" "PAGE")
   "https://shirakumo.github.io/radiance/#RESOURCE-TYPE%20PROFILE%3APAGE"
   "RESOURCE-TYPE PROFILE:PAGE")
  (("option PANEL PROFILE:ACCESS" "PANEL PROFILE:ACCESS" "ACCESS")
   "https://shirakumo.github.io/radiance/#option%20PANEL%20PROFILE%3AACCESS"
   "Option PANEL PROFILE:ACCESS")
  (("HOOK PROFILE:IMPLEMENTED" "PROFILE:IMPLEMENTED" "IMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20PROFILE%3AIMPLEMENTED"
   "HOOK PROFILE:IMPLEMENTED")
  (("HOOK PROFILE:UNIMPLEMENTED" "PROFILE:UNIMPLEMENTED" "UNIMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20PROFILE%3AUNIMPLEMENTED"
   "HOOK PROFILE:UNIMPLEMENTED")
  (("ACCESSOR PROFILE:IMPLEMENTATION" "PROFILE:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20PROFILE%3AIMPLEMENTATION"
   "ACCESSOR PROFILE:IMPLEMENTATION")
  (("FUNCTION PROFILE:ADD-FIELD" "PROFILE:ADD-FIELD" "ADD-FIELD")
   "https://shirakumo.github.io/radiance/#FUNCTION%20PROFILE%3AADD-FIELD"
   "FUNCTION PROFILE:ADD-FIELD")
  (("FUNCTION PROFILE:AVATAR" "PROFILE:AVATAR" "AVATAR")
   "https://shirakumo.github.io/radiance/#FUNCTION%20PROFILE%3AAVATAR"
   "FUNCTION PROFILE:AVATAR")
  (("FUNCTION PROFILE:FIELDS" "PROFILE:FIELDS" "FIELDS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20PROFILE%3AFIELDS"
   "FUNCTION PROFILE:FIELDS")
  (("FUNCTION PROFILE:LIST-PANELS" "PROFILE:LIST-PANELS" "LIST-PANELS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20PROFILE%3ALIST-PANELS"
   "FUNCTION PROFILE:LIST-PANELS")
  (("FUNCTION PROFILE:NAME" "PROFILE:NAME" "NAME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20PROFILE%3ANAME"
   "FUNCTION PROFILE:NAME")
  (("FUNCTION PROFILE:REMOVE-FIELD" "PROFILE:REMOVE-FIELD" "REMOVE-FIELD")
   "https://shirakumo.github.io/radiance/#FUNCTION%20PROFILE%3AREMOVE-FIELD"
   "FUNCTION PROFILE:REMOVE-FIELD")
  (("FUNCTION PROFILE:REMOVE-PANEL" "PROFILE:REMOVE-PANEL" "REMOVE-PANEL")
   "https://shirakumo.github.io/radiance/#FUNCTION%20PROFILE%3AREMOVE-PANEL"
   "FUNCTION PROFILE:REMOVE-PANEL")
  (("MACRO PROFILE:DEFINE-PANEL" "PROFILE:DEFINE-PANEL" "DEFINE-PANEL")
   "https://shirakumo.github.io/radiance/#MACRO%20PROFILE%3ADEFINE-PANEL"
   "MACRO PROFILE:DEFINE-PANEL")
  (("SPECIAL RATE:*IMPLEMENTATION*" "RATE:*IMPLEMENTATION*" "*IMPLEMENTATION*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20RATE%3A%2AIMPLEMENTATION%2A"
   "SPECIAL RATE:*IMPLEMENTATION*")
  (("HOOK RATE:IMPLEMENTED" "RATE:IMPLEMENTED" "IMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20RATE%3AIMPLEMENTED"
   "HOOK RATE:IMPLEMENTED")
  (("HOOK RATE:UNIMPLEMENTED" "RATE:UNIMPLEMENTED" "UNIMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20RATE%3AUNIMPLEMENTED"
   "HOOK RATE:UNIMPLEMENTED")
  (("ACCESSOR RATE:IMPLEMENTATION" "RATE:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20RATE%3AIMPLEMENTATION"
   "ACCESSOR RATE:IMPLEMENTATION")
  (("FUNCTION RATE:LEFT" "RATE:LEFT" "LEFT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20RATE%3ALEFT"
   "FUNCTION RATE:LEFT")
  (("MACRO RATE:DEFINE-LIMIT" "RATE:DEFINE-LIMIT" "DEFINE-LIMIT")
   "https://shirakumo.github.io/radiance/#MACRO%20RATE%3ADEFINE-LIMIT"
   "MACRO RATE:DEFINE-LIMIT")
  (("MACRO RATE:WITH-LIMITATION" "RATE:WITH-LIMITATION" "WITH-LIMITATION")
   "https://shirakumo.github.io/radiance/#MACRO%20RATE%3AWITH-LIMITATION"
   "MACRO RATE:WITH-LIMITATION")
  (("SPECIAL SERVER:*IMPLEMENTATION*" "SERVER:*IMPLEMENTATION*" "*IMPLEMENTATION*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20SERVER%3A%2AIMPLEMENTATION%2A"
   "SPECIAL SERVER:*IMPLEMENTATION*")
  (("HOOK SERVER:IMPLEMENTED" "SERVER:IMPLEMENTED" "IMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20SERVER%3AIMPLEMENTED"
   "HOOK SERVER:IMPLEMENTED")
  (("HOOK SERVER:STARTED" "SERVER:STARTED" "STARTED")
   "https://shirakumo.github.io/radiance/#HOOK%20SERVER%3ASTARTED"
   "HOOK SERVER:STARTED")
  (("HOOK SERVER:STOPPED" "SERVER:STOPPED" "STOPPED")
   "https://shirakumo.github.io/radiance/#HOOK%20SERVER%3ASTOPPED"
   "HOOK SERVER:STOPPED")
  (("HOOK SERVER:UNIMPLEMENTED" "SERVER:UNIMPLEMENTED" "UNIMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20SERVER%3AUNIMPLEMENTED"
   "HOOK SERVER:UNIMPLEMENTED")
  (("ACCESSOR SERVER:IMPLEMENTATION" "SERVER:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20SERVER%3AIMPLEMENTATION"
   "ACCESSOR SERVER:IMPLEMENTATION")
  (("FUNCTION SERVER:LISTENERS" "SERVER:LISTENERS" "LISTENERS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20SERVER%3ALISTENERS"
   "FUNCTION SERVER:LISTENERS")
  (("FUNCTION SERVER:START" "SERVER:START" "START")
   "https://shirakumo.github.io/radiance/#FUNCTION%20SERVER%3ASTART"
   "FUNCTION SERVER:START")
  (("FUNCTION SERVER:STOP" "SERVER:STOP" "STOP")
   "https://shirakumo.github.io/radiance/#FUNCTION%20SERVER%3ASTOP"
   "FUNCTION SERVER:STOP")
  (("SPECIAL SESSION:*DEFAULT-TIMEOUT*" "SESSION:*DEFAULT-TIMEOUT*" "*DEFAULT-TIMEOUT*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20SESSION%3A%2ADEFAULT-TIMEOUT%2A"
   "SPECIAL SESSION:*DEFAULT-TIMEOUT*")
  (("SPECIAL SESSION:*IMPLEMENTATION*" "SESSION:*IMPLEMENTATION*" "*IMPLEMENTATION*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20SESSION%3A%2AIMPLEMENTATION%2A"
   "SPECIAL SESSION:*IMPLEMENTATION*")
  (("CLASS SESSION:SESSION" "SESSION:SESSION" "SESSION")
   "https://shirakumo.github.io/radiance/#CLASS%20SESSION%3ASESSION"
   "CLASS SESSION:SESSION")
  (("HOOK SESSION:CREATE" "SESSION:CREATE" "CREATE")
   "https://shirakumo.github.io/radiance/#HOOK%20SESSION%3ACREATE"
   "HOOK SESSION:CREATE")
  (("HOOK SESSION:IMPLEMENTED" "SESSION:IMPLEMENTED" "IMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20SESSION%3AIMPLEMENTED"
   "HOOK SESSION:IMPLEMENTED")
  (("HOOK SESSION:UNIMPLEMENTED" "SESSION:UNIMPLEMENTED" "UNIMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20SESSION%3AUNIMPLEMENTED"
   "HOOK SESSION:UNIMPLEMENTED")
  (("ACCESSOR SESSION:FIELD" "SESSION:FIELD" "FIELD")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20SESSION%3AFIELD"
   "ACCESSOR SESSION:FIELD")
  (("ACCESSOR SESSION:IMPLEMENTATION" "SESSION:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20SESSION%3AIMPLEMENTATION"
   "ACCESSOR SESSION:IMPLEMENTATION")
  (("ACCESSOR SESSION:TIMEOUT" "SESSION:TIMEOUT" "TIMEOUT")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20SESSION%3ATIMEOUT"
   "ACCESSOR SESSION:TIMEOUT")
  (("FUNCTION SESSION:=" "SESSION:=" "=")
   "https://shirakumo.github.io/radiance/#FUNCTION%20SESSION%3A%3D"
   "FUNCTION SESSION:=")
  (("FUNCTION SESSION:ACTIVE-P" "SESSION:ACTIVE-P" "ACTIVE-P")
   "https://shirakumo.github.io/radiance/#FUNCTION%20SESSION%3AACTIVE-P"
   "FUNCTION SESSION:ACTIVE-P")
  (("FUNCTION SESSION:END" "SESSION:END" "END")
   "https://shirakumo.github.io/radiance/#FUNCTION%20SESSION%3AEND"
   "FUNCTION SESSION:END")
  (("FUNCTION SESSION:GET" "SESSION:GET" "GET")
   "https://shirakumo.github.io/radiance/#FUNCTION%20SESSION%3AGET"
   "FUNCTION SESSION:GET")
  (("FUNCTION SESSION:ID" "SESSION:ID" "ID")
   "https://shirakumo.github.io/radiance/#FUNCTION%20SESSION%3AID"
   "FUNCTION SESSION:ID")
  (("FUNCTION SESSION:LIST" "SESSION:LIST" "LIST")
   "https://shirakumo.github.io/radiance/#FUNCTION%20SESSION%3ALIST"
   "FUNCTION SESSION:LIST")
  (("FUNCTION SESSION:START" "SESSION:START" "START")
   "https://shirakumo.github.io/radiance/#FUNCTION%20SESSION%3ASTART"
   "FUNCTION SESSION:START")
  (("SPECIAL USER:*IMPLEMENTATION*" "USER:*IMPLEMENTATION*" "*IMPLEMENTATION*")
   "https://shirakumo.github.io/radiance/#SPECIAL%20USER%3A%2AIMPLEMENTATION%2A"
   "SPECIAL USER:*IMPLEMENTATION*")
  (("CLASS USER:USER" "USER:USER" "USER")
   "https://shirakumo.github.io/radiance/#CLASS%20USER%3AUSER"
   "CLASS USER:USER")
  (("CONDITION USER:CONDITION" "USER:CONDITION" "CONDITION")
   "https://shirakumo.github.io/radiance/#CONDITION%20USER%3ACONDITION"
   "CONDITION USER:CONDITION")
  (("CONDITION USER:NOT-FOUND" "USER:NOT-FOUND" "NOT-FOUND")
   "https://shirakumo.github.io/radiance/#CONDITION%20USER%3ANOT-FOUND"
   "CONDITION USER:NOT-FOUND")
  (("HOOK USER:ACTION" "USER:ACTION" "ACTION")
   "https://shirakumo.github.io/radiance/#HOOK%20USER%3AACTION"
   "HOOK USER:ACTION")
  (("HOOK USER:CREATE" "USER:CREATE" "CREATE")
   "https://shirakumo.github.io/radiance/#HOOK%20USER%3ACREATE"
   "HOOK USER:CREATE")
  (("HOOK USER:IMPLEMENTED" "USER:IMPLEMENTED" "IMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20USER%3AIMPLEMENTED"
   "HOOK USER:IMPLEMENTED")
  (("HOOK USER:READY" "USER:READY" "READY")
   "https://shirakumo.github.io/radiance/#HOOK%20USER%3AREADY"
   "HOOK USER:READY")
  (("HOOK USER:REMOVE" "USER:REMOVE" "REMOVE")
   "https://shirakumo.github.io/radiance/#HOOK%20USER%3AREMOVE"
   "HOOK USER:REMOVE")
  (("HOOK USER:UNIMPLEMENTED" "USER:UNIMPLEMENTED" "UNIMPLEMENTED")
   "https://shirakumo.github.io/radiance/#HOOK%20USER%3AUNIMPLEMENTED"
   "HOOK USER:UNIMPLEMENTED")
  (("HOOK USER:UNREADY" "USER:UNREADY" "UNREADY")
   "https://shirakumo.github.io/radiance/#HOOK%20USER%3AUNREADY"
   "HOOK USER:UNREADY")
  (("ACCESSOR USER:FIELD" "USER:FIELD" "FIELD")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20USER%3AFIELD"
   "ACCESSOR USER:FIELD")
  (("ACCESSOR USER:IMPLEMENTATION" "USER:IMPLEMENTATION" "IMPLEMENTATION")
   "https://shirakumo.github.io/radiance/#ACCESSOR%20USER%3AIMPLEMENTATION"
   "ACCESSOR USER:IMPLEMENTATION")
  (("FUNCTION USER:=" "USER:=" "=")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3A%3D"
   "FUNCTION USER:=")
  (("FUNCTION USER:ACTION" "USER:ACTION" "ACTION")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3AACTION"
   "FUNCTION USER:ACTION")
  (("FUNCTION USER:ACTIONS" "USER:ACTIONS" "ACTIONS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3AACTIONS"
   "FUNCTION USER:ACTIONS")
  (("FUNCTION USER:ADD-DEFAULT-PERMISSIONS" "USER:ADD-DEFAULT-PERMISSIONS" "ADD-DEFAULT-PERMISSIONS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3AADD-DEFAULT-PERMISSIONS"
   "FUNCTION USER:ADD-DEFAULT-PERMISSIONS")
  (("FUNCTION USER:CHECK" "USER:CHECK" "CHECK")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3ACHECK"
   "FUNCTION USER:CHECK")
  (("FUNCTION USER:FIELDS" "USER:FIELDS" "FIELDS")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3AFIELDS"
   "FUNCTION USER:FIELDS")
  (("FUNCTION USER:GET" "USER:GET" "GET")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3AGET"
   "FUNCTION USER:GET")
  (("FUNCTION USER:GRANT" "USER:GRANT" "GRANT")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3AGRANT"
   "FUNCTION USER:GRANT")
  (("FUNCTION USER:LIST" "USER:LIST" "LIST")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3ALIST"
   "FUNCTION USER:LIST")
  (("FUNCTION USER:REMOVE" "USER:REMOVE" "REMOVE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3AREMOVE"
   "FUNCTION USER:REMOVE")
  (("FUNCTION USER:REMOVE-FIELD" "USER:REMOVE-FIELD" "REMOVE-FIELD")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3AREMOVE-FIELD"
   "FUNCTION USER:REMOVE-FIELD")
  (("FUNCTION USER:REVOKE" "USER:REVOKE" "REVOKE")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3AREVOKE"
   "FUNCTION USER:REVOKE")
  (("FUNCTION USER:USERNAME" "USER:USERNAME" "USERNAME")
   "https://shirakumo.github.io/radiance/#FUNCTION%20USER%3AUSERNAME"
   "FUNCTION USER:USERNAME"))

(define-table-lookup qtools
  (("About Qtools")
   "https://shinmera.github.io/qtools/#about_qtools_"
   "About Qtools")
  (("Fundamentals")
   "https://shinmera.github.io/qtools/#fundamentals"
   "Fundamentals")
  (("Getting Started")
   "https://shinmera.github.io/qtools/#getting_started"
   "Getting Started")
  (("Deployment")
   "https://shinmera.github.io/qtools/#deployment"
   "Deployment")
  (("Qtools Components")
   "https://shinmera.github.io/qtools/#qtools_components"
   "Qtools Components")
  (("Examples")
   "https://shinmera.github.io/qtools/#examples"
   "Examples")
  (("Extending Qtools")
   "https://shinmera.github.io/qtools/#extending_qtools"
   "Extending Qtools")
  (("Qtools Concepts")
   "https://shinmera.github.io/qtools/#qtools_concepts"
   "Qtools Concepts")
  (("Support")
   "https://shinmera.github.io/qtools/#support"
   "Support")
  (("Qt")
   "https://shinmera.github.io/qtools/#qt"
   "Qt")
  (("Smoke")
   "https://shinmera.github.io/qtools/#smoke"
   "Smoke")
  (("CommonQt")
   "https://shinmera.github.io/qtools/#commonqt"
   "CommonQt")
  (("Qtools")
   "https://shinmera.github.io/qtools/#qtools"
   "Qtools")
  (("Qt")
   "https://shinmera.github.io/qtools/#qt"
   "Qt")
  (("Qtools")
   "https://shinmera.github.io/qtools/#qtools"
   "Qtools")
  (("Name Conversion")
   "https://shinmera.github.io/qtools/#name_conversion"
   "Name Conversion")
  (("Object Handling")
   "https://shinmera.github.io/qtools/#object_handling"
   "Object Handling")
  (("Widgets")
   "https://shinmera.github.io/qtools/#widgets"
   "Widgets")
  (("Q+")
   "https://shinmera.github.io/qtools/#q+"
   "Q+")
  (("Smoke Modules")
   "https://shinmera.github.io/qtools/#smoke_modules"
   "Smoke Modules")
  (("Fast Calling")
   "https://shinmera.github.io/qtools/#fast_calling"
   "Fast Calling")
  (("Copying and Finalizing")
   "https://shinmera.github.io/qtools/#copying_and_finalizing"
   "Copying and Finalizing")
  (("Adding defmethod declarations")
   "https://shinmera.github.io/qtools/#adding_defmethod_declarations"
   "Adding defmethod declarations")
  (("Extending the menu definition")
   "https://shinmera.github.io/qtools/#extending_the_menu_definition"
   "Extending the menu definition")
  (("Finalizables")
   "https://shinmera.github.io/qtools/#finalizables"
   "Finalizables")
  (("Widgets")
   "https://shinmera.github.io/qtools/#widgets"
   "Widgets")
  (("Q+")
   "https://shinmera.github.io/qtools/#q+"
   "Q+")
  (("SPECIAL QTOOLS:*BOOT-HOOKS*" "QTOOLS:*BOOT-HOOKS*" "*BOOT-HOOKS*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2ABOOT-HOOKS%2A"
   "SPECIAL QTOOLS:*BOOT-HOOKS*")
  (("SPECIAL QTOOLS:*BUILD-HOOKS*" "QTOOLS:*BUILD-HOOKS*" "*BUILD-HOOKS*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2ABUILD-HOOKS%2A"
   "SPECIAL QTOOLS:*BUILD-HOOKS*")
  (("SPECIAL QTOOLS:*DEPLOYMENT-LOCATION*" "QTOOLS:*DEPLOYMENT-LOCATION*" "*DEPLOYMENT-LOCATION*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2ADEPLOYMENT-LOCATION%2A"
   "SPECIAL QTOOLS:*DEPLOYMENT-LOCATION*")
  (("SPECIAL QTOOLS:*GENERATED-MODULES*" "QTOOLS:*GENERATED-MODULES*" "*GENERATED-MODULES*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2AGENERATED-MODULES%2A"
   "SPECIAL QTOOLS:*GENERATED-MODULES*")
  (("SPECIAL QTOOLS:*METHOD*" "QTOOLS:*METHOD*" "*METHOD*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2AMETHOD%2A"
   "SPECIAL QTOOLS:*METHOD*")
  (("SPECIAL QTOOLS:*OPERATOR-MAP*" "QTOOLS:*OPERATOR-MAP*" "*OPERATOR-MAP*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2AOPERATOR-MAP%2A"
   "SPECIAL QTOOLS:*OPERATOR-MAP*")
  (("SPECIAL QTOOLS:*QMETHODS*" "QTOOLS:*QMETHODS*" "*QMETHODS*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2AQMETHODS%2A"
   "SPECIAL QTOOLS:*QMETHODS*")
  (("SPECIAL QTOOLS:*QT-CLASS-MAP*" "QTOOLS:*QT-CLASS-MAP*" "*QT-CLASS-MAP*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2AQT-CLASS-MAP%2A"
   "SPECIAL QTOOLS:*QT-CLASS-MAP*")
  (("SPECIAL QTOOLS:*QT-CLASS-VECTOR*" "QTOOLS:*QT-CLASS-VECTOR*" "*QT-CLASS-VECTOR*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2AQT-CLASS-VECTOR%2A"
   "SPECIAL QTOOLS:*QT-CLASS-VECTOR*")
  (("SPECIAL QTOOLS:*QUIT-HOOKS*" "QTOOLS:*QUIT-HOOKS*" "*QUIT-HOOKS*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2AQUIT-HOOKS%2A"
   "SPECIAL QTOOLS:*QUIT-HOOKS*")
  (("SPECIAL QTOOLS:*SMOKE-MODULES*" "QTOOLS:*SMOKE-MODULES*" "*SMOKE-MODULES*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2ASMOKE-MODULES%2A"
   "SPECIAL QTOOLS:*SMOKE-MODULES*")
  (("SPECIAL QTOOLS:*SMOKE-MODULES-TO-RELOAD*" "QTOOLS:*SMOKE-MODULES-TO-RELOAD*" "*SMOKE-MODULES-TO-RELOAD*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2ASMOKE-MODULES-TO-RELOAD%2A"
   "SPECIAL QTOOLS:*SMOKE-MODULES-TO-RELOAD*")
  (("SPECIAL QTOOLS:*TARGET-PACKAGE*" "QTOOLS:*TARGET-PACKAGE*" "*TARGET-PACKAGE*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2ATARGET-PACKAGE%2A"
   "SPECIAL QTOOLS:*TARGET-PACKAGE*")
  (("SPECIAL QTOOLS:*WIDGET*" "QTOOLS:*WIDGET*" "*WIDGET*")
   "https://shinmera.github.io/qtools/#SPECIAL%20QTOOLS%3A%2AWIDGET%2A"
   "SPECIAL QTOOLS:*WIDGET*")
  (("CLASS QTOOLS:CLASS" "QTOOLS:CLASS" "CLASS")
   "https://shinmera.github.io/qtools/#CLASS%20QTOOLS%3ACLASS"
   "CLASS QTOOLS:CLASS")
  (("CLASS QTOOLS:FINALIZABLE" "QTOOLS:FINALIZABLE" "FINALIZABLE")
   "https://shinmera.github.io/qtools/#CLASS%20QTOOLS%3AFINALIZABLE"
   "CLASS QTOOLS:FINALIZABLE")
  (("CLASS QTOOLS:FINALIZABLE-CLASS" "QTOOLS:FINALIZABLE-CLASS" "FINALIZABLE-CLASS")
   "https://shinmera.github.io/qtools/#CLASS%20QTOOLS%3AFINALIZABLE-CLASS"
   "CLASS QTOOLS:FINALIZABLE-CLASS")
  (("CLASS QTOOLS:FINALIZABLE-SLOT" "QTOOLS:FINALIZABLE-SLOT" "FINALIZABLE-SLOT")
   "https://shinmera.github.io/qtools/#CLASS%20QTOOLS%3AFINALIZABLE-SLOT"
   "CLASS QTOOLS:FINALIZABLE-SLOT")
  (("CLASS QTOOLS:GC-FINALIZED" "QTOOLS:GC-FINALIZED" "GC-FINALIZED")
   "https://shinmera.github.io/qtools/#CLASS%20QTOOLS%3AGC-FINALIZED"
   "CLASS QTOOLS:GC-FINALIZED")
  (("CLASS QTOOLS:QT-PROGRAM-OP" "QTOOLS:QT-PROGRAM-OP" "QT-PROGRAM-OP")
   "https://shinmera.github.io/qtools/#CLASS%20QTOOLS%3AQT-PROGRAM-OP"
   "CLASS QTOOLS:QT-PROGRAM-OP")
  (("CLASS QTOOLS:WIDGET" "QTOOLS:WIDGET" "WIDGET")
   "https://shinmera.github.io/qtools/#CLASS%20QTOOLS%3AWIDGET"
   "CLASS QTOOLS:WIDGET")
  (("CLASS QTOOLS:WIDGET-CLASS" "QTOOLS:WIDGET-CLASS" "WIDGET-CLASS")
   "https://shinmera.github.io/qtools/#CLASS%20QTOOLS%3AWIDGET-CLASS"
   "CLASS QTOOLS:WIDGET-CLASS")
  (("CONDITION QTOOLS:COMPILATION-NOTE" "QTOOLS:COMPILATION-NOTE" "COMPILATION-NOTE")
   "https://shinmera.github.io/qtools/#CONDITION%20QTOOLS%3ACOMPILATION-NOTE"
   "CONDITION QTOOLS:COMPILATION-NOTE")
  (("CONDITION QTOOLS:INVALID-QT-SUPERCLASS-HIERARCHY" "QTOOLS:INVALID-QT-SUPERCLASS-HIERARCHY" "INVALID-QT-SUPERCLASS-HIERARCHY")
   "https://shinmera.github.io/qtools/#CONDITION%20QTOOLS%3AINVALID-QT-SUPERCLASS-HIERARCHY"
   "CONDITION QTOOLS:INVALID-QT-SUPERCLASS-HIERARCHY")
  (("ACCESSOR QTOOLS:CLASHING-QT-SUPERCLASS" "QTOOLS:CLASHING-QT-SUPERCLASS" "CLASHING-QT-SUPERCLASS")
   "https://shinmera.github.io/qtools/#ACCESSOR%20QTOOLS%3ACLASHING-QT-SUPERCLASS"
   "ACCESSOR QTOOLS:CLASHING-QT-SUPERCLASS")
  (("ACCESSOR QTOOLS:CLASHING-SUPERCLASS" "QTOOLS:CLASHING-SUPERCLASS" "CLASHING-SUPERCLASS")
   "https://shinmera.github.io/qtools/#ACCESSOR%20QTOOLS%3ACLASHING-SUPERCLASS"
   "ACCESSOR QTOOLS:CLASHING-SUPERCLASS")
  (("ACCESSOR QTOOLS:MENU-CONTENT-TYPE" "QTOOLS:MENU-CONTENT-TYPE" "MENU-CONTENT-TYPE")
   "https://shinmera.github.io/qtools/#ACCESSOR%20QTOOLS%3AMENU-CONTENT-TYPE"
   "ACCESSOR QTOOLS:MENU-CONTENT-TYPE")
  (("ACCESSOR QTOOLS:METHOD-DECLARATION" "QTOOLS:METHOD-DECLARATION" "METHOD-DECLARATION")
   "https://shinmera.github.io/qtools/#ACCESSOR%20QTOOLS%3AMETHOD-DECLARATION"
   "ACCESSOR QTOOLS:METHOD-DECLARATION")
  (("ACCESSOR QTOOLS:PARENT" "QTOOLS:PARENT" "PARENT")
   "https://shinmera.github.io/qtools/#ACCESSOR%20QTOOLS%3APARENT"
   "ACCESSOR QTOOLS:PARENT")
  (("ACCESSOR QTOOLS:REQUESTED-QT-SUPERCLASS" "QTOOLS:REQUESTED-QT-SUPERCLASS" "REQUESTED-QT-SUPERCLASS")
   "https://shinmera.github.io/qtools/#ACCESSOR%20QTOOLS%3AREQUESTED-QT-SUPERCLASS"
   "ACCESSOR QTOOLS:REQUESTED-QT-SUPERCLASS")
  (("ACCESSOR QTOOLS:VALUE" "QTOOLS:VALUE" "VALUE")
   "https://shinmera.github.io/qtools/#ACCESSOR%20QTOOLS%3AVALUE"
   "ACCESSOR QTOOLS:VALUE")
  (("ACCESSOR QTOOLS:WIDGET-ACTIONS" "QTOOLS:WIDGET-ACTIONS" "WIDGET-ACTIONS")
   "https://shinmera.github.io/qtools/#ACCESSOR%20QTOOLS%3AWIDGET-ACTIONS"
   "ACCESSOR QTOOLS:WIDGET-ACTIONS")
  (("ACCESSOR QTOOLS:WIDGET-CLASS-DIRECT-OPTIONS" "QTOOLS:WIDGET-CLASS-DIRECT-OPTIONS" "WIDGET-CLASS-DIRECT-OPTIONS")
   "https://shinmera.github.io/qtools/#ACCESSOR%20QTOOLS%3AWIDGET-CLASS-DIRECT-OPTIONS"
   "ACCESSOR QTOOLS:WIDGET-CLASS-DIRECT-OPTIONS")
  (("ACCESSOR QTOOLS:WIDGET-CLASS-EXTERN-OPTIONS" "QTOOLS:WIDGET-CLASS-EXTERN-OPTIONS" "WIDGET-CLASS-EXTERN-OPTIONS")
   "https://shinmera.github.io/qtools/#ACCESSOR%20QTOOLS%3AWIDGET-CLASS-EXTERN-OPTIONS"
   "ACCESSOR QTOOLS:WIDGET-CLASS-EXTERN-OPTIONS")
  (("ACCESSOR QTOOLS:WIDGET-CLASS-FINALIZERS" "QTOOLS:WIDGET-CLASS-FINALIZERS" "WIDGET-CLASS-FINALIZERS")
   "https://shinmera.github.io/qtools/#ACCESSOR%20QTOOLS%3AWIDGET-CLASS-FINALIZERS"
   "ACCESSOR QTOOLS:WIDGET-CLASS-FINALIZERS")
  (("ACCESSOR QTOOLS:WIDGET-CLASS-INITIALIZERS" "QTOOLS:WIDGET-CLASS-INITIALIZERS" "WIDGET-CLASS-INITIALIZERS")
   "https://shinmera.github.io/qtools/#ACCESSOR%20QTOOLS%3AWIDGET-CLASS-INITIALIZERS"
   "ACCESSOR QTOOLS:WIDGET-CLASS-INITIALIZERS")
  (("FUNCTION QTOOLS:BUILD-MENU-CONTENT" "QTOOLS:BUILD-MENU-CONTENT" "BUILD-MENU-CONTENT")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ABUILD-MENU-CONTENT"
   "FUNCTION QTOOLS:BUILD-MENU-CONTENT")
  (("FUNCTION QTOOLS:CALL-FINALIZERS" "QTOOLS:CALL-FINALIZERS" "CALL-FINALIZERS")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ACALL-FINALIZERS"
   "FUNCTION QTOOLS:CALL-FINALIZERS")
  (("FUNCTION QTOOLS:CALL-INITIALIZERS" "QTOOLS:CALL-INITIALIZERS" "CALL-INITIALIZERS")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ACALL-INITIALIZERS"
   "FUNCTION QTOOLS:CALL-INITIALIZERS")
  (("FUNCTION QTOOLS:CAPITALIZE-ON" "QTOOLS:CAPITALIZE-ON" "CAPITALIZE-ON")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ACAPITALIZE-ON"
   "FUNCTION QTOOLS:CAPITALIZE-ON")
  (("FUNCTION QTOOLS:CL-TYPE-FOR" "QTOOLS:CL-TYPE-FOR" "CL-TYPE-FOR")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ACL-TYPE-FOR"
   "FUNCTION QTOOLS:CL-TYPE-FOR")
  (("FUNCTION QTOOLS:CLEAR-METHOD-INFO" "QTOOLS:CLEAR-METHOD-INFO" "CLEAR-METHOD-INFO")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ACLEAR-METHOD-INFO"
   "FUNCTION QTOOLS:CLEAR-METHOD-INFO")
  (("FUNCTION QTOOLS:COMPILE-WRAPPER" "QTOOLS:COMPILE-WRAPPER" "COMPILE-WRAPPER")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ACOMPILE-WRAPPER"
   "FUNCTION QTOOLS:COMPILE-WRAPPER")
  (("FUNCTION QTOOLS:COPY-QOBJECT" "QTOOLS:COPY-QOBJECT" "COPY-QOBJECT")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ACOPY-QOBJECT"
   "FUNCTION QTOOLS:COPY-QOBJECT")
  (("FUNCTION QTOOLS:DEFAULT-APPLICATION-NAME" "QTOOLS:DEFAULT-APPLICATION-NAME" "DEFAULT-APPLICATION-NAME")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ADEFAULT-APPLICATION-NAME"
   "FUNCTION QTOOLS:DEFAULT-APPLICATION-NAME")
  (("FUNCTION QTOOLS:DESCRIBE-COPY-METHOD" "QTOOLS:DESCRIBE-COPY-METHOD" "DESCRIBE-COPY-METHOD")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ADESCRIBE-COPY-METHOD"
   "FUNCTION QTOOLS:DESCRIBE-COPY-METHOD")
  (("FUNCTION QTOOLS:DESCRIBE-FINALIZE-METHOD" "QTOOLS:DESCRIBE-FINALIZE-METHOD" "DESCRIBE-FINALIZE-METHOD")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ADESCRIBE-FINALIZE-METHOD"
   "FUNCTION QTOOLS:DESCRIBE-FINALIZE-METHOD")
  (("FUNCTION QTOOLS:DESCRIBE-PRINT-METHOD" "QTOOLS:DESCRIBE-PRINT-METHOD" "DESCRIBE-PRINT-METHOD")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ADESCRIBE-PRINT-METHOD"
   "FUNCTION QTOOLS:DESCRIBE-PRINT-METHOD")
  (("FUNCTION QTOOLS:DETERMINED-TYPE-METHOD-NAME" "QTOOLS:DETERMINED-TYPE-METHOD-NAME" "DETERMINED-TYPE-METHOD-NAME")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ADETERMINED-TYPE-METHOD-NAME"
   "FUNCTION QTOOLS:DETERMINED-TYPE-METHOD-NAME")
  (("FUNCTION QTOOLS:DIRECT-QSUBCLASS-P" "QTOOLS:DIRECT-QSUBCLASS-P" "DIRECT-QSUBCLASS-P")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ADIRECT-QSUBCLASS-P"
   "FUNCTION QTOOLS:DIRECT-QSUBCLASS-P")
  (("FUNCTION QTOOLS:DISPATCH-BY-QCLASS" "QTOOLS:DISPATCH-BY-QCLASS" "DISPATCH-BY-QCLASS")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ADISPATCH-BY-QCLASS"
   "FUNCTION QTOOLS:DISPATCH-BY-QCLASS")
  (("FUNCTION QTOOLS:ECL-TYPE-FOR" "QTOOLS:ECL-TYPE-FOR" "ECL-TYPE-FOR")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AECL-TYPE-FOR"
   "FUNCTION QTOOLS:ECL-TYPE-FOR")
  (("FUNCTION QTOOLS:EMIT-COMPILATION-NOTE" "QTOOLS:EMIT-COMPILATION-NOTE" "EMIT-COMPILATION-NOTE")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AEMIT-COMPILATION-NOTE"
   "FUNCTION QTOOLS:EMIT-COMPILATION-NOTE")
  (("FUNCTION QTOOLS:ENSURE-CLASS" "QTOOLS:ENSURE-CLASS" "ENSURE-CLASS")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AENSURE-CLASS"
   "FUNCTION QTOOLS:ENSURE-CLASS")
  (("FUNCTION QTOOLS:ENSURE-METHODS-PROCESSED" "QTOOLS:ENSURE-METHODS-PROCESSED" "ENSURE-METHODS-PROCESSED")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AENSURE-METHODS-PROCESSED"
   "FUNCTION QTOOLS:ENSURE-METHODS-PROCESSED")
  (("FUNCTION QTOOLS:ENSURE-Q+-METHOD" "QTOOLS:ENSURE-Q+-METHOD" "ENSURE-Q+-METHOD")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AENSURE-Q%2B-METHOD"
   "FUNCTION QTOOLS:ENSURE-Q+-METHOD")
  (("FUNCTION QTOOLS:ENSURE-QAPPLICATION" "QTOOLS:ENSURE-QAPPLICATION" "ENSURE-QAPPLICATION")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AENSURE-QAPPLICATION"
   "FUNCTION QTOOLS:ENSURE-QAPPLICATION")
  (("FUNCTION QTOOLS:ENSURE-QCLASS" "QTOOLS:ENSURE-QCLASS" "ENSURE-QCLASS")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AENSURE-QCLASS"
   "FUNCTION QTOOLS:ENSURE-QCLASS")
  (("FUNCTION QTOOLS:ENSURE-QOBJECT" "QTOOLS:ENSURE-QOBJECT" "ENSURE-QOBJECT")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AENSURE-QOBJECT"
   "FUNCTION QTOOLS:ENSURE-QOBJECT")
  (("FUNCTION QTOOLS:ENUMERATE-METHOD-DESCRIPTORS" "QTOOLS:ENUMERATE-METHOD-DESCRIPTORS" "ENUMERATE-METHOD-DESCRIPTORS")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AENUMERATE-METHOD-DESCRIPTORS"
   "FUNCTION QTOOLS:ENUMERATE-METHOD-DESCRIPTORS")
  (("FUNCTION QTOOLS:EQT-CLASS-NAME" "QTOOLS:EQT-CLASS-NAME" "EQT-CLASS-NAME")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AEQT-CLASS-NAME"
   "FUNCTION QTOOLS:EQT-CLASS-NAME")
  (("FUNCTION QTOOLS:EQT-TYPE-OF" "QTOOLS:EQT-TYPE-OF" "EQT-TYPE-OF")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AEQT-TYPE-OF"
   "FUNCTION QTOOLS:EQT-TYPE-OF")
  (("FUNCTION QTOOLS:FINALIZE-QOBJECT" "QTOOLS:FINALIZE-QOBJECT" "FINALIZE-QOBJECT")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AFINALIZE-QOBJECT"
   "FUNCTION QTOOLS:FINALIZE-QOBJECT")
  (("FUNCTION QTOOLS:FIND-CHILD" "QTOOLS:FIND-CHILD" "FIND-CHILD")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AFIND-CHILD"
   "FUNCTION QTOOLS:FIND-CHILD")
  (("FUNCTION QTOOLS:FIND-CHILDREN" "QTOOLS:FIND-CHILDREN" "FIND-CHILDREN")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AFIND-CHILDREN"
   "FUNCTION QTOOLS:FIND-CHILDREN")
  (("FUNCTION QTOOLS:FIND-FASTCALL-METHOD" "QTOOLS:FIND-FASTCALL-METHOD" "FIND-FASTCALL-METHOD")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AFIND-FASTCALL-METHOD"
   "FUNCTION QTOOLS:FIND-FASTCALL-METHOD")
  (("FUNCTION QTOOLS:FIND-QT-CLASS-NAME" "QTOOLS:FIND-QT-CLASS-NAME" "FIND-QT-CLASS-NAME")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AFIND-QT-CLASS-NAME"
   "FUNCTION QTOOLS:FIND-QT-CLASS-NAME")
  (("FUNCTION QTOOLS:FUSE-ALISTS" "QTOOLS:FUSE-ALISTS" "FUSE-ALISTS")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AFUSE-ALISTS"
   "FUNCTION QTOOLS:FUSE-ALISTS")
  (("FUNCTION QTOOLS:FUSE-PLISTS" "QTOOLS:FUSE-PLISTS" "FUSE-PLISTS")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AFUSE-PLISTS"
   "FUNCTION QTOOLS:FUSE-PLISTS")
  (("FUNCTION QTOOLS:GENERIC-SIGNAL" "QTOOLS:GENERIC-SIGNAL" "GENERIC-SIGNAL")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AGENERIC-SIGNAL"
   "FUNCTION QTOOLS:GENERIC-SIGNAL")
  (("FUNCTION QTOOLS:LOAD-ALL-SMOKE-MODULES" "QTOOLS:LOAD-ALL-SMOKE-MODULES" "LOAD-ALL-SMOKE-MODULES")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ALOAD-ALL-SMOKE-MODULES"
   "FUNCTION QTOOLS:LOAD-ALL-SMOKE-MODULES")
  (("FUNCTION QTOOLS:LOADED-SMOKE-MODULES" "QTOOLS:LOADED-SMOKE-MODULES" "LOADED-SMOKE-MODULES")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ALOADED-SMOKE-MODULES"
   "FUNCTION QTOOLS:LOADED-SMOKE-MODULES")
  (("FUNCTION QTOOLS:MAKE-CHORD" "QTOOLS:MAKE-CHORD" "MAKE-CHORD")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AMAKE-CHORD"
   "FUNCTION QTOOLS:MAKE-CHORD")
  (("FUNCTION QTOOLS:MAKE-GC-FINALIZED" "QTOOLS:MAKE-GC-FINALIZED" "MAKE-GC-FINALIZED")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AMAKE-GC-FINALIZED"
   "FUNCTION QTOOLS:MAKE-GC-FINALIZED")
  (("FUNCTION QTOOLS:MAP-COMPILE-ALL" "QTOOLS:MAP-COMPILE-ALL" "MAP-COMPILE-ALL")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AMAP-COMPILE-ALL"
   "FUNCTION QTOOLS:MAP-COMPILE-ALL")
  (("FUNCTION QTOOLS:MAP-LAYOUT" "QTOOLS:MAP-LAYOUT" "MAP-LAYOUT")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AMAP-LAYOUT"
   "FUNCTION QTOOLS:MAP-LAYOUT")
  (("FUNCTION QTOOLS:MAYBE-DELETE-QOBJECT" "QTOOLS:MAYBE-DELETE-QOBJECT" "MAYBE-DELETE-QOBJECT")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AMAYBE-DELETE-QOBJECT"
   "FUNCTION QTOOLS:MAYBE-DELETE-QOBJECT")
  (("FUNCTION QTOOLS:METHOD-SYMBOL" "QTOOLS:METHOD-SYMBOL" "METHOD-SYMBOL")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AMETHOD-SYMBOL"
   "FUNCTION QTOOLS:METHOD-SYMBOL")
  (("FUNCTION QTOOLS:PRINT-QOBJECT" "QTOOLS:PRINT-QOBJECT" "PRINT-QOBJECT")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3APRINT-QOBJECT"
   "FUNCTION QTOOLS:PRINT-QOBJECT")
  (("FUNCTION QTOOLS:PROCESS-ALL-METHODS" "QTOOLS:PROCESS-ALL-METHODS" "PROCESS-ALL-METHODS")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3APROCESS-ALL-METHODS"
   "FUNCTION QTOOLS:PROCESS-ALL-METHODS")
  (("FUNCTION QTOOLS:PROCESS-METHOD" "QTOOLS:PROCESS-METHOD" "PROCESS-METHOD")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3APROCESS-METHOD"
   "FUNCTION QTOOLS:PROCESS-METHOD")
  (("FUNCTION QTOOLS:Q+-COMPILE-AND-LOAD" "QTOOLS:Q+-COMPILE-AND-LOAD" "Q+-COMPILE-AND-LOAD")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AQ%2B-COMPILE-AND-LOAD"
   "FUNCTION QTOOLS:Q+-COMPILE-AND-LOAD")
  (("FUNCTION QTOOLS:Q+APROPOS" "QTOOLS:Q+APROPOS" "Q+APROPOS")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AQ%2BAPROPOS"
   "FUNCTION QTOOLS:Q+APROPOS")
  (("FUNCTION QTOOLS:QCLASS-PRECEDENCE-LIST" "QTOOLS:QCLASS-PRECEDENCE-LIST" "QCLASS-PRECEDENCE-LIST")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AQCLASS-PRECEDENCE-LIST"
   "FUNCTION QTOOLS:QCLASS-PRECEDENCE-LIST")
  (("FUNCTION QTOOLS:QINSTANCEP" "QTOOLS:QINSTANCEP" "QINSTANCEP")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AQINSTANCEP"
   "FUNCTION QTOOLS:QINSTANCEP")
  (("FUNCTION QTOOLS:QOBJECT-ALIVE-P" "QTOOLS:QOBJECT-ALIVE-P" "QOBJECT-ALIVE-P")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AQOBJECT-ALIVE-P"
   "FUNCTION QTOOLS:QOBJECT-ALIVE-P")
  (("FUNCTION QTOOLS:QT-TYPE-FOR" "QTOOLS:QT-TYPE-FOR" "QT-TYPE-FOR")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AQT-TYPE-FOR"
   "FUNCTION QTOOLS:QT-TYPE-FOR")
  (("FUNCTION QTOOLS:QT-TYPE-OF" "QTOOLS:QT-TYPE-OF" "QT-TYPE-OF")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AQT-TYPE-OF"
   "FUNCTION QTOOLS:QT-TYPE-OF")
  (("FUNCTION QTOOLS:REMOVE-FINALIZER" "QTOOLS:REMOVE-FINALIZER" "REMOVE-FINALIZER")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AREMOVE-FINALIZER"
   "FUNCTION QTOOLS:REMOVE-FINALIZER")
  (("FUNCTION QTOOLS:REMOVE-INITIALIZER" "QTOOLS:REMOVE-INITIALIZER" "REMOVE-INITIALIZER")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AREMOVE-INITIALIZER"
   "FUNCTION QTOOLS:REMOVE-INITIALIZER")
  (("FUNCTION QTOOLS:REMOVE-MENU-CONTENT-TYPE" "QTOOLS:REMOVE-MENU-CONTENT-TYPE" "REMOVE-MENU-CONTENT-TYPE")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AREMOVE-MENU-CONTENT-TYPE"
   "FUNCTION QTOOLS:REMOVE-MENU-CONTENT-TYPE")
  (("FUNCTION QTOOLS:REMOVE-METHOD-DECLARATION" "QTOOLS:REMOVE-METHOD-DECLARATION" "REMOVE-METHOD-DECLARATION")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AREMOVE-METHOD-DECLARATION"
   "FUNCTION QTOOLS:REMOVE-METHOD-DECLARATION")
  (("FUNCTION QTOOLS:REMOVE-OVERRIDE" "QTOOLS:REMOVE-OVERRIDE" "REMOVE-OVERRIDE")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AREMOVE-OVERRIDE"
   "FUNCTION QTOOLS:REMOVE-OVERRIDE")
  (("FUNCTION QTOOLS:REMOVE-SIGNAL" "QTOOLS:REMOVE-SIGNAL" "REMOVE-SIGNAL")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AREMOVE-SIGNAL"
   "FUNCTION QTOOLS:REMOVE-SIGNAL")
  (("FUNCTION QTOOLS:REMOVE-SLOT" "QTOOLS:REMOVE-SLOT" "REMOVE-SLOT")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AREMOVE-SLOT"
   "FUNCTION QTOOLS:REMOVE-SLOT")
  (("FUNCTION QTOOLS:REMOVE-SUBWIDGET" "QTOOLS:REMOVE-SUBWIDGET" "REMOVE-SUBWIDGET")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AREMOVE-SUBWIDGET"
   "FUNCTION QTOOLS:REMOVE-SUBWIDGET")
  (("FUNCTION QTOOLS:REMOVE-WIDGET-CLASS-OPTION" "QTOOLS:REMOVE-WIDGET-CLASS-OPTION" "REMOVE-WIDGET-CLASS-OPTION")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AREMOVE-WIDGET-CLASS-OPTION"
   "FUNCTION QTOOLS:REMOVE-WIDGET-CLASS-OPTION")
  (("FUNCTION QTOOLS:SET-WIDGET-CLASS-OPTION" "QTOOLS:SET-WIDGET-CLASS-OPTION" "SET-WIDGET-CLASS-OPTION")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ASET-WIDGET-CLASS-OPTION"
   "FUNCTION QTOOLS:SET-WIDGET-CLASS-OPTION")
  (("FUNCTION QTOOLS:SOFTLY-REDEFINE-WIDGET-CLASS" "QTOOLS:SOFTLY-REDEFINE-WIDGET-CLASS" "SOFTLY-REDEFINE-WIDGET-CLASS")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ASOFTLY-REDEFINE-WIDGET-CLASS"
   "FUNCTION QTOOLS:SOFTLY-REDEFINE-WIDGET-CLASS")
  (("FUNCTION QTOOLS:SPECIFIED-TYPE-METHOD-NAME" "QTOOLS:SPECIFIED-TYPE-METHOD-NAME" "SPECIFIED-TYPE-METHOD-NAME")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ASPECIFIED-TYPE-METHOD-NAME"
   "FUNCTION QTOOLS:SPECIFIED-TYPE-METHOD-NAME")
  (("FUNCTION QTOOLS:SPLIT" "QTOOLS:SPLIT" "SPLIT")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ASPLIT"
   "FUNCTION QTOOLS:SPLIT")
  (("FUNCTION QTOOLS:SWEEP-LAYOUT" "QTOOLS:SWEEP-LAYOUT" "SWEEP-LAYOUT")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ASWEEP-LAYOUT"
   "FUNCTION QTOOLS:SWEEP-LAYOUT")
  (("FUNCTION QTOOLS:TO-METHOD-NAME" "QTOOLS:TO-METHOD-NAME" "TO-METHOD-NAME")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ATO-METHOD-NAME"
   "FUNCTION QTOOLS:TO-METHOD-NAME")
  (("FUNCTION QTOOLS:TO-QBYTE-ARRAY" "QTOOLS:TO-QBYTE-ARRAY" "TO-QBYTE-ARRAY")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ATO-QBYTE-ARRAY"
   "FUNCTION QTOOLS:TO-QBYTE-ARRAY")
  (("FUNCTION QTOOLS:TO-TYPE-NAME" "QTOOLS:TO-TYPE-NAME" "TO-TYPE-NAME")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ATO-TYPE-NAME"
   "FUNCTION QTOOLS:TO-TYPE-NAME")
  (("FUNCTION QTOOLS:TRANSLATE-NAME" "QTOOLS:TRANSLATE-NAME" "TRANSLATE-NAME")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3ATRANSLATE-NAME"
   "FUNCTION QTOOLS:TRANSLATE-NAME")
  (("FUNCTION QTOOLS:WIDGET-CLASS-OPTION-P" "QTOOLS:WIDGET-CLASS-OPTION-P" "WIDGET-CLASS-OPTION-P")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AWIDGET-CLASS-OPTION-P"
   "FUNCTION QTOOLS:WIDGET-CLASS-OPTION-P")
  (("FUNCTION QTOOLS:WRITE-EVERYTHING-TO-FILE" "QTOOLS:WRITE-EVERYTHING-TO-FILE" "WRITE-EVERYTHING-TO-FILE")
   "https://shinmera.github.io/qtools/#FUNCTION%20QTOOLS%3AWRITE-EVERYTHING-TO-FILE"
   "FUNCTION QTOOLS:WRITE-EVERYTHING-TO-FILE")
  (("GENERIC QTOOLS:CONSTRUCT" "QTOOLS:CONSTRUCT" "CONSTRUCT")
   "https://shinmera.github.io/qtools/#GENERIC%20QTOOLS%3ACONSTRUCT"
   "GENERIC QTOOLS:CONSTRUCT")
  (("GENERIC QTOOLS:COPY" "QTOOLS:COPY" "COPY")
   "https://shinmera.github.io/qtools/#GENERIC%20QTOOLS%3ACOPY"
   "GENERIC QTOOLS:COPY")
  (("GENERIC QTOOLS:FINALIZE" "QTOOLS:FINALIZE" "FINALIZE")
   "https://shinmera.github.io/qtools/#GENERIC%20QTOOLS%3AFINALIZE"
   "GENERIC QTOOLS:FINALIZE")
  (("GENERIC QTOOLS:FINALIZED" "QTOOLS:FINALIZED" "FINALIZED")
   "https://shinmera.github.io/qtools/#GENERIC%20QTOOLS%3AFINALIZED"
   "GENERIC QTOOLS:FINALIZED")
  (("GENERIC QTOOLS:UNBOX" "QTOOLS:UNBOX" "UNBOX")
   "https://shinmera.github.io/qtools/#GENERIC%20QTOOLS%3AUNBOX"
   "GENERIC QTOOLS:UNBOX")
  (("MACRO QTOOLS:CONNECT!" "QTOOLS:CONNECT!" "CONNECT!")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ACONNECT%21"
   "MACRO QTOOLS:CONNECT!")
  (("MACRO QTOOLS:DEFINE-1->1-TRANSLATOR" "QTOOLS:DEFINE-1->1-TRANSLATOR" "DEFINE-1->1-TRANSLATOR")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-1-%3E1-TRANSLATOR"
   "MACRO QTOOLS:DEFINE-1->1-TRANSLATOR")
  (("MACRO QTOOLS:DEFINE-COPY-METHOD" "QTOOLS:DEFINE-COPY-METHOD" "DEFINE-COPY-METHOD")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-COPY-METHOD"
   "MACRO QTOOLS:DEFINE-COPY-METHOD")
  (("MACRO QTOOLS:DEFINE-FINALIZABLE" "QTOOLS:DEFINE-FINALIZABLE" "DEFINE-FINALIZABLE")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-FINALIZABLE"
   "MACRO QTOOLS:DEFINE-FINALIZABLE")
  (("MACRO QTOOLS:DEFINE-FINALIZE-METHOD" "QTOOLS:DEFINE-FINALIZE-METHOD" "DEFINE-FINALIZE-METHOD")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-FINALIZE-METHOD"
   "MACRO QTOOLS:DEFINE-FINALIZE-METHOD")
  (("MACRO QTOOLS:DEFINE-FINALIZER" "QTOOLS:DEFINE-FINALIZER" "DEFINE-FINALIZER")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-FINALIZER"
   "MACRO QTOOLS:DEFINE-FINALIZER")
  (("MACRO QTOOLS:DEFINE-INITIALIZER" "QTOOLS:DEFINE-INITIALIZER" "DEFINE-INITIALIZER")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-INITIALIZER"
   "MACRO QTOOLS:DEFINE-INITIALIZER")
  (("MACRO QTOOLS:DEFINE-MENU" "QTOOLS:DEFINE-MENU" "DEFINE-MENU")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-MENU"
   "MACRO QTOOLS:DEFINE-MENU")
  (("MACRO QTOOLS:DEFINE-MENU-CONTENT-TYPE" "QTOOLS:DEFINE-MENU-CONTENT-TYPE" "DEFINE-MENU-CONTENT-TYPE")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-MENU-CONTENT-TYPE"
   "MACRO QTOOLS:DEFINE-MENU-CONTENT-TYPE")
  (("MACRO QTOOLS:DEFINE-METHOD-DECLARATION" "QTOOLS:DEFINE-METHOD-DECLARATION" "DEFINE-METHOD-DECLARATION")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-METHOD-DECLARATION"
   "MACRO QTOOLS:DEFINE-METHOD-DECLARATION")
  (("MACRO QTOOLS:DEFINE-OBJECT" "QTOOLS:DEFINE-OBJECT" "DEFINE-OBJECT")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-OBJECT"
   "MACRO QTOOLS:DEFINE-OBJECT")
  (("MACRO QTOOLS:DEFINE-OVERRIDE" "QTOOLS:DEFINE-OVERRIDE" "DEFINE-OVERRIDE")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-OVERRIDE"
   "MACRO QTOOLS:DEFINE-OVERRIDE")
  (("MACRO QTOOLS:DEFINE-PRINT-METHOD" "QTOOLS:DEFINE-PRINT-METHOD" "DEFINE-PRINT-METHOD")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-PRINT-METHOD"
   "MACRO QTOOLS:DEFINE-PRINT-METHOD")
  (("MACRO QTOOLS:DEFINE-QCLASS-DISPATCH-FUNCTION" "QTOOLS:DEFINE-QCLASS-DISPATCH-FUNCTION" "DEFINE-QCLASS-DISPATCH-FUNCTION")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-QCLASS-DISPATCH-FUNCTION"
   "MACRO QTOOLS:DEFINE-QCLASS-DISPATCH-FUNCTION")
  (("MACRO QTOOLS:DEFINE-SIGNAL" "QTOOLS:DEFINE-SIGNAL" "DEFINE-SIGNAL")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-SIGNAL"
   "MACRO QTOOLS:DEFINE-SIGNAL")
  (("MACRO QTOOLS:DEFINE-SIGNAL-METHOD" "QTOOLS:DEFINE-SIGNAL-METHOD" "DEFINE-SIGNAL-METHOD")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-SIGNAL-METHOD"
   "MACRO QTOOLS:DEFINE-SIGNAL-METHOD")
  (("MACRO QTOOLS:DEFINE-SIMPLE-TRANSLATOR" "QTOOLS:DEFINE-SIMPLE-TRANSLATOR" "DEFINE-SIMPLE-TRANSLATOR")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-SIMPLE-TRANSLATOR"
   "MACRO QTOOLS:DEFINE-SIMPLE-TRANSLATOR")
  (("MACRO QTOOLS:DEFINE-SLOT" "QTOOLS:DEFINE-SLOT" "DEFINE-SLOT")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-SLOT"
   "MACRO QTOOLS:DEFINE-SLOT")
  (("MACRO QTOOLS:DEFINE-SUBOBJECT" "QTOOLS:DEFINE-SUBOBJECT" "DEFINE-SUBOBJECT")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-SUBOBJECT"
   "MACRO QTOOLS:DEFINE-SUBOBJECT")
  (("MACRO QTOOLS:DEFINE-SUBWIDGET" "QTOOLS:DEFINE-SUBWIDGET" "DEFINE-SUBWIDGET")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-SUBWIDGET"
   "MACRO QTOOLS:DEFINE-SUBWIDGET")
  (("MACRO QTOOLS:DEFINE-TRANSLATOR" "QTOOLS:DEFINE-TRANSLATOR" "DEFINE-TRANSLATOR")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-TRANSLATOR"
   "MACRO QTOOLS:DEFINE-TRANSLATOR")
  (("MACRO QTOOLS:DEFINE-WIDGET" "QTOOLS:DEFINE-WIDGET" "DEFINE-WIDGET")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADEFINE-WIDGET"
   "MACRO QTOOLS:DEFINE-WIDGET")
  (("MACRO QTOOLS:DISCONNECT!" "QTOOLS:DISCONNECT!" "DISCONNECT!")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADISCONNECT%21"
   "MACRO QTOOLS:DISCONNECT!")
  (("MACRO QTOOLS:DO-LAYOUT" "QTOOLS:DO-LAYOUT" "DO-LAYOUT")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ADO-LAYOUT"
   "MACRO QTOOLS:DO-LAYOUT")
  (("MACRO QTOOLS:FAST-CALL" "QTOOLS:FAST-CALL" "FAST-CALL")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AFAST-CALL"
   "MACRO QTOOLS:FAST-CALL")
  (("MACRO QTOOLS:FAST-DIRECT-CALL" "QTOOLS:FAST-DIRECT-CALL" "FAST-DIRECT-CALL")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AFAST-DIRECT-CALL"
   "MACRO QTOOLS:FAST-DIRECT-CALL")
  (("MACRO QTOOLS:FSETF" "QTOOLS:FSETF" "FSETF")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AFSETF"
   "MACRO QTOOLS:FSETF")
  (("MACRO QTOOLS:Q+" "QTOOLS:Q+" "Q+")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AQ%2B"
   "MACRO QTOOLS:Q+")
  (("MACRO QTOOLS:Q+FUN" "QTOOLS:Q+FUN" "Q+FUN")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AQ%2BFUN"
   "MACRO QTOOLS:Q+FUN")
  (("MACRO QTOOLS:QCLASS=CASE" "QTOOLS:QCLASS=CASE" "QCLASS=CASE")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AQCLASS%3DCASE"
   "MACRO QTOOLS:QCLASS=CASE")
  (("MACRO QTOOLS:QTENUMCASE" "QTOOLS:QTENUMCASE" "QTENUMCASE")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AQTENUMCASE"
   "MACRO QTOOLS:QTENUMCASE")
  (("MACRO QTOOLS:QTYPECASE" "QTOOLS:QTYPECASE" "QTYPECASE")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AQTYPECASE"
   "MACRO QTOOLS:QTYPECASE")
  (("MACRO QTOOLS:SIGNAL!" "QTOOLS:SIGNAL!" "SIGNAL!")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3ASIGNAL%21"
   "MACRO QTOOLS:SIGNAL!")
  (("MACRO QTOOLS:WITH-ALL-SLOTS-BOUND" "QTOOLS:WITH-ALL-SLOTS-BOUND" "WITH-ALL-SLOTS-BOUND")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AWITH-ALL-SLOTS-BOUND"
   "MACRO QTOOLS:WITH-ALL-SLOTS-BOUND")
  (("MACRO QTOOLS:WITH-CALL-STACK" "QTOOLS:WITH-CALL-STACK" "WITH-CALL-STACK")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AWITH-CALL-STACK"
   "MACRO QTOOLS:WITH-CALL-STACK")
  (("MACRO QTOOLS:WITH-FINALIZING" "QTOOLS:WITH-FINALIZING" "WITH-FINALIZING")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AWITH-FINALIZING"
   "MACRO QTOOLS:WITH-FINALIZING")
  (("MACRO QTOOLS:WITH-FINALIZING*" "QTOOLS:WITH-FINALIZING*" "WITH-FINALIZING*")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AWITH-FINALIZING%2A"
   "MACRO QTOOLS:WITH-FINALIZING*")
  (("MACRO QTOOLS:WITH-GC-FINALIZED" "QTOOLS:WITH-GC-FINALIZED" "WITH-GC-FINALIZED")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AWITH-GC-FINALIZED"
   "MACRO QTOOLS:WITH-GC-FINALIZED")
  (("MACRO QTOOLS:WITH-MAIN-WINDOW" "QTOOLS:WITH-MAIN-WINDOW" "WITH-MAIN-WINDOW")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AWITH-MAIN-WINDOW"
   "MACRO QTOOLS:WITH-MAIN-WINDOW")
  (("MACRO QTOOLS:WITH-SLOTS-BOUND" "QTOOLS:WITH-SLOTS-BOUND" "WITH-SLOTS-BOUND")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AWITH-SLOTS-BOUND"
   "MACRO QTOOLS:WITH-SLOTS-BOUND")
  (("MACRO QTOOLS:WITH-WIDGET-CLASS" "QTOOLS:WITH-WIDGET-CLASS" "WITH-WIDGET-CLASS")
   "https://shinmera.github.io/qtools/#MACRO%20QTOOLS%3AWITH-WIDGET-CLASS"
   "MACRO QTOOLS:WITH-WIDGET-CLASS"))
