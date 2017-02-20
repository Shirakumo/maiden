#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.client-entities)

;; clients.lisp
(docs:define-docs
  (type user-client
    "Superclass for clients that communicate with users of some form.

See FIND-USER
See USERS
See ENSURE-USER
See AUTHENTICATE
See USER
See USERNAME")

  (function find-user
    "Attempt to find the user object of the given name on the client.

Note that if the user cannot be found, it does not necessarily
mean that the user does not exist at all, or does not exist on
the server at this time. You can only find users that the system
has already learned about in some way.

For some clients this may also be an accessor that allows you to
register user objects with the client.

See USER
See USER-CLIENT")

  (function users
    "Return a list of all known user objects on the client.

See USER
See USER-CLIENT")

  (function ensure-user
    "Either return the existing user of that name, or create it new. Either way, ensure it exists.

See USER
See FIND-USER
See USER-CLIENT")

  (function authenticate
    "Return whether it is known that the given user object is really that particular user, or might also be someone else.

In other words, this returns whether the identity of the user can
be trusted or not. Note that the check is not cached, and may thus
take some time or be costly in some fashion, as outside
communication might be necessary to confirm the identity.

See AUTHENTICATED-P")

  (type channel-client
    "Superclass for clients that provide the concept of a channel over which users can communicate with each other in a group.

See FIND-CHANNEL
See CHANNELS
See ENSURE-CHANNEL
See CHANNEL")

  (function find-channel
    "Attempt to find the channel object of the given name on the client.

Note that if the channel cannot be found, it does not necessarily
mean that the channel does not exist at all, or does not exist on
the server at this time. You can only find channels that the
system has already learned about in some way.

For some clients this may also be an accessor that allows you to
register channel objects with the client.

See CHANNEL
See CHANNEL-CLIENT")

  (function channels
    "Return a list of all known channel objects on the client.

See CHANNEL
See CHANNEL-CLIENT")

  (function ensure-channel
    "Either return the existing channel of that name, or create it new. Either way, ensure it exists.

See CHANNEL
See FIND-CHANNEL
See CHANNEL-CLIENT")

  (type user-container
    "Mixin for objects that can contain users in some way.

The standard implementation this offers uses an equalp hash-table
for the user storage, mapping user names to user objects.

See USER
See USER-MAP
See USERS
See FIND-USER
See REMOVE-USER
See ENSURE-USER")

  (function user-map
    "Accessor to the hash-table that maps user names to user objects on the user-container.

See USER-CONTAINER")

  (function remove-user
    "Remove the given user from the user-container.

See USER
See USER-CONTAINER")

  (type channel-container
    "Mixin for objects that contain channels in some way.

The standard implementation this offers uses an equalp hash-table
for the channel storage, mapping channel names to channel objects.

See CHANNEL
See CHANNEL-MAP
See CHANNELS
See FIND-CHANNEL
See REMOVE-CHANNEL
See ENSURE-CHANNEL")

  (function channel-map
    "Accessor to the hash-table that maps channel names to channel objects on the channel-container

See CHANNEL-CONTAINER")

  (function remove-channel
    "Remove the given channel from the channel-container.

See CHANNEL
See CHANNEL-CONTAINER")

  (type simple-user-channel-client
    "Superclass for a client that has both users and channels.

This class already handles the association/storage of user and
channel objects. User ENSURE-USER/CHANNEL to register users and
channels.

If you need to subclass USER or CHANNEL --which you probably
should anyway-- you should subclass SIMPLE-USER and SIMPLE-CHANNEL
instead, and override ENSURE-USER and ENSURE-CHANNEL to create
instances of the appropriate classes instead.

You will have to make sure that, when a user enters a channel, the
channel is registered to the user via FIND-CHANNEL on the user,
and the user is registered to the channel via FIND-USER on the
channel. The same goes when a user leaves a channel. However, you
do not need to do anything should a channel or a user be removed
from the client altogether, as in that case this generic system
can figure out what to do on its own and ensure consistency.

See USER-CLIENT
See CHANNEL-CLIENT
See USER-CONTAINER
See CHANNEL-CONTAINER
See SIMPLE-USER
See SIMPLE-CHANNEL")

  (type simple-user
    "Superclass for simple users.

Users can be in a number of channels.

See USER
See CHANNEL-CONTAINER")

  (type simple-channel
    "Superclass for simple channels.

Channels can contain a number of users.

See CHANNEL
See USER-CONTAINER"))

;; entities.lisp
(docs:define-docs
  (type client-entity
    "Superclass for all entities that are tied to a client instance somehow.

See NAMED-ENTITY
See CLIENT")

  (type user
    "Object representing a user on a remote system somewhere.

Users have a name and may be authenticated. Additional properties
are not guaranteed, but may be provided by a client.

See CLIENT-ENTITY
See AUTHENTICATE
See AUTHENTICATED-P
See USERNAME")

  (function username
     "Accessor to the entity's name on the client network.

This may or may not be distinct from the entity's NAME.
Especially clients may often want to keep a separate NAME
and USERNAME, as the name should stay the same on the local
core, but the username may possibly change or be different
depending on the network.

See USER
See USER-CLIENT")

  (function authenticated-p
    "Returns whether the user is authenticated or not.

This value is cached. The first time this is called on a user, it
may take a while.

See USER
See AUTHENTICATE")

  (type channel
    "Object representing a channel on a remote system somewhere.

Channels are virtual \"rooms\" over which one or more users may
communicate with each other.

Channels have a name and a number of users that inhabit it.
Additional properties are not guaranteed, but may be provided by a
client.

See CLIENT-ENTITY
See USERS"))

;; events.lisp
(docs:define-docs
  (type user-event
    "Superclass for all events relating to a user.

See CLIENT-EVENT
See USER")

  (function user
    "Reader for the user object the event relates to.

See USER-EVENT")

  (type user-removed
    "Event issued when a user can no longer be tracked by the client.

This does not necessarily mean that the user has left the remote
server.

See USER-EVENT")

  (type user-added
    "Event issued when a user is now tracked by the client.

See USER-EVENT")

  (type user-name-changed
    "Event issued when the name of a user has changed for some reason.

If you track users by name rather than by their object, you should
use this to update your records. The old name is kept in the OLD-
NAME slot.

See USER-EVENT
See OLD-NAME")

  (function old-name
    "Reader for the old name the user used to have. The user object should already have the new name.

See USER-NAME-CHANGED")

  (type message-event
    "Event issued when a textual message of some kind is sent.

Note that not all message-events are channel-events.

See MESSAGE
See USER-EVENT")

  (function message
    "Accessor to the message string carried by the event.

See MESSAGE-EVENT")

  (type channel-event
    "Superclass for all events relating to a channel.

See CHANNEL
See CLIENT-EVENT")

  (function channel
    "Reader for the channel object the event relates to

See CHANNEL-EVENT")

  (type user-entered
    "Event issued when a user entered a channel.

See USER-EVENT
See CHANNEL-EVENT")

  (type user-left
    "Event issued when a user left a channel.

See USER-EVENT
See CHANNEL-EVENT"))
