(in-package #:org.shirakumo.maiden.agents.talk)

;; codes.lisp
(docs:define-docs
  (variable *language-code-map*
    "This is an alist of language names to language codes.

See LANGUAGE-CODE")

  (function language-code
    "Returns the appropriate language code for the language, if such a code is known. Otherwise returns the language string itself again."))

;; talk.lisp
(docs:define-docs
  (type talk
    "This agent provides text-to-speech reading.")

  (function device
    "Accessor to the name of the device to use for the output.

See TALK")

  (function output
    "Accessor to the cl-out123:output instance used to play back the voice.

See TALK")

  (function get-speech-stream
    "Attempts to translate the text into the language using Google Translate TTS.

If successful, returns a stream to the MP3 file returned by the API.
If unsuccessful, an error is signalled. Note that only some languages
are supported, and that only text up to 200 characters is allowed.

See LANGUAGE-CODE")

  (function call-with-speech-file
    "Calls the function with a path to a temporary file containing the TTS mp3 file for the text and language.

See GET-SPEECH-STREAM
See UIOP:WITH-TEMPORARY-FILE")

  (function with-speech-file
    "Wrap the body in an env where PATH is a pathname to an MP3 file.

See CALL-WITH-SPEECH-FILE")

  (function with-output
    "Wrap the body in an environment where OUT is a cl-out123:output that is ready for playback.

See CL-OUT123:CONNECT
See CL-OUT123:START")

  (function play-file
    "Play back the given MP3 file on the given output.

If output is not given, a new one is constructed.

See CL-MPG123:CONNECT
See CL-OUT123:PLAY
See WITH-OUTPUT")

  (function split-word-boundary
    "Attempt to split the text into a shorter version that does not exceed MAX characters.

This tries to respect sentence and word boundaries in order to make
the splitting as unawkward as possible.")

  (function talk
    "Speak the given text back.

If the text is too long for the TTS API it is split up into multiple
requests. Thus there might be an occasional gap where a new request
and playback is started in the middle of the text.

See SPLIT-WORD-BOUNDARY
See PLAY-FILE")

  (command talk-en
    "Speak the given text in English. Note that this will be played back on the bot owner's machine.")

  (command talk-lang
    "Speak the given text in the requested language, if possible. Note that this will be played back on the bot owner's machine."))
