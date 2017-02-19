#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.notify)

;; interface.lisp
(docs:define-docs
  (type notify
    "This agent implements an offline messaging system. It is useful for reminding people if they are not currently around.")

  (function handle-note-notification
    "This goes through all the notes for the user and replies to the even with each one of them, if the note's trigger matches the one given.

All applicable notes are removed.

See REMOVE-NOTE
See TRIGGER
See USER-NOTES")

  (function handle-note-creation
    "This handles the creation of a new note and takes care of some special cases.

No note is created if the target is empty or the
user themselves.

See MAKE-NOTE
See NORMALIZE-USER-NAME")

  (command forget-notes
    "Throws out all notes for yourself, or a specific user. Useful if you've already seen the notes, don't need them anymore, or addressed them to the wrong person.")

  (command send-join-note
    "Send a notification message that will be displayed as soon as the user joins a channel the bot is on again.")

  (command send-note
    "Send a notification message that will be displayed as soon as the user speaks again."))

;; notes.lisp
(docs:define-docs
  (type note
    "This class holds all relevant information for a notification.

Notes are a way of reminding another user of something
at a time where they're hopefully paying attention again.

After a note instance has been created, it is
automatically registered.

See ID
See FROM
See TO
See MESSAGE
See DATE
See TRIGGER
See MAKE-NOTE
See REGISTER-NOTE")

  (function from
    "Accessor to the source of the note.

See NOTE")

  (function to
    "Accessor to the recipient of the note.

See NOTE")

  (function date
    "Accessor to the universal-time that denotes the note's creation time.

See NOTE")

  (function trigger
    "Accessor to the note's trigger condition.

Can be one of
- :MESSAGE  The note is sent out when the user next
            writes a new message.
- :JOIN     The note is sent out when the user next
            joins a channel we can see.")

  (function make-note
    "Easily create a new note instance.

See NOTE")

  (function next-note-id
    "Create a new ID for a note.

This modifies the global note storage.

See MAIDEN-STORAGE:WITH-STORAGE")

  (function normalize-user-name
    "Normalize the username by converting it into a string and downcasing it.")

  (function register-note
    "Register a note in the notes storage.

This will ensure it is persisted to disk.

See MAIDEN-STORAGE:WITH-STORAGE")

  (function remove-note
    "Remove the note from the storage.

See MAIDEN-STORAGE:WITH-STORAGE")

  (function clear-notes
    "Clear all notes addressed to the given user.

See MAIDEN-STORAGE:WITH-STORAGE")

  (function user-notes
    "Retrieve a fresh list of all notes addressed to the user.

See MAIDEN-STORAGE:WITH-STORAGE"))
