#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)

(defpackage #:lichat-cmd
  (:nicknames #:org.shirakumo.maiden.clients.lichat.cmd)
  (:use)
  (:export
   #:ping
   #:pong
   #:connect
   #:disconnect
   #:register
   #:join
   #:leave
   #:create
   #:kick
   #:pull
   #:permissions
   #:message
   #:users
   #:channels
   #:user-info
   #:failure
   #:malformed-update
   #:connection-unstable
   #:too-many-connections
   #:update-failure
   #:update-id
   #:invalid-update
   #:username-mismatch
   #:incompatible-version
   #:invalid-password
   #:no-such-profile
   #:username-taken
   #:no-such-channel
   #:already-in-channel
   #:not-in-channel
   #:channelname-taken
   #:bad-name
   #:insufficient-permissions
   #:invalid-permissions
   #:no-such-user
   #:too-many-updates))

(defpackage #:lichat-rpl
  (:nicknames #:org.shirakumo.maiden.clients.lichat.rpl)
  (:use)
  (:export
   #:ping
   #:pong
   #:connect
   #:disconnect
   #:register
   #:join
   #:leave
   #:create
   #:kick
   #:pull
   #:permissions
   #:message
   #:users
   #:channels
   #:user-info
   #:failure
   #:malformed-update
   #:connection-unstable
   #:too-many-connections
   #:update-failure
   #:update-id
   #:invalid-update
   #:username-mismatch
   #:incompatible-version
   #:invalid-password
   #:no-such-profile
   #:username-taken
   #:no-such-channel
   #:already-in-channel
   #:not-in-channel
   #:channelname-taken
   #:bad-name
   #:insufficient-permissions
   #:invalid-permissions
   #:no-such-user
   #:too-many-updates))

(defpackage #:maiden-lichat
  (:nicknames #:org.shirakumo.maiden.clients.lichat)
  (:use #:cl #:maiden #:maiden-networking #:maiden-client-entities)
  (:export))
