(in-package #:org.shirakumo.maiden.agents.crimes)

;; cardcast.lisp
(docs:define-docs
  (variable *cardcast/decks*
    "Address to retrieve a list of cardcast decks from.")

  (variable *cardcast/deck*
    "Address to retrieve information about a cardcast deck from.")

  (variable *cardcast/deck/cards*
    "Address to retrieve a list of cards for a cardcast deck from.")

  (function cardcast/decks
    "Search or list decks from the cardcast server.

The returned result is the parsed JSON return value from the API.

See *CARDCAST/DECKS*")

  (function cardcast/deck
    "Request information about a specific cardcast deck.

The returned result is the parsed JSON return value from the API.

See *CARDCAST/DECK*")

  (function cardcast/deck/cards
    "Request the card list for a specific deck from the cardcast server.

The returned result is the parsed JSON return value from the API.

See *CARDCAST/DECK/CARDS*")

  (function cardcast->card
    "Translate a cardcast json object into a card instance.

TYPE being the name of a class to create an instance from.
Passed as initargs will be the ID and the TEXT.")

  (function cardcast-list->map
    "Turn a list of cardcast cards into a hash-table mapping the ID to the card instance.

See CARDCAST->CARD")

  (function load-cardcast-deck
    "Load a deck from the cardcast server and parse it into a DECK instance.

See DECK
See CARDCAST/DECK
See CARDCAST/DECK/CARDS
See CARDCAST-LIST->MAP")

  (function find-cardcast-decks
    "Returns a list of lists of name to deck id of matching decks for the given search query.

See CARDCAST/DECKS"))

;; cards.lisp
(docs:define-docs
  (variable *decks*
    "A hash-table of deck names to deck instances.

See DECK
See REMOVE-DECK")

  (function normalize-name
    "Normalise the name to a usable one.

This removes all characters except for alphanumeric ones,
downcases them, and then turns them into a symbol in the
current package.")

  (function deck
    "Acessor to the deck storage.

If a deck is attempted to be read that is not yet in the table,
it is attempted to be loaded from disk. When a deck is set, it
is automatically offloaded to disk.

See *DECKS*
See NORMALIZE-NAME
See LOAD-DECK")

  (function remove-deck
    "Completely remove the given deck.

The deck is both removed from the cached table, and from the
disk. It is not possible to recover it once the deck's instance
has been garbage collected.

See DECK")

  (type deck
    "Container class for a deck of crimes cards.

It contains a table of call cards and a table of response cards.

See NAME
See TITLE
See CALLS
See RESPONSES")

  (function title
    "Accessor to the descriptive title of the card deck.

See DECK")

  (function calls
    "Accessor to the table or list of call cards.

See DECK
See GAME
See CALL")

  (function responses
    "Accessor to the table or list of response cards.

See DECK
See GAME
See RESPONSE")

  (function list-calls
    "Return a fresh list of all call cards in the deck.

See CALLS
See CALL")

  (function list-responses
    "Return a fresh list of all response cards in the deck.

See RESPONSES
See RESPONSE")

  (function save-deck
    "Save the given deck to disk so that it can be restored later.

The deck's NAME is used as the storage designator.

See DECK
See LOAD-DECK
See MAIDEN-STORAGE:WITH-STORAGE")

  (function load-deck
    "Load the given deck from disk if possible.

If no storage for the given deck can be found, an error
is signalled.

See DECK
See SAVE-DECK
See NORMALIZE-NAME
See MAIDEN-STORAGE:WITH-STORAGE")

  (function remove-card
    "Removes the card with the given ID from the deck.

See DECK")

  (type card
    "Class to represent a card in a play deck.

See TEXT")

  (function text
    "Accessor to the text that the card holds.

The text may either be a direct string, or a list of
strings.

See CARD")

  (function card
    "Accessor to the card of the given ID in the deck.

See DECK
See CARD")

  (type call
    "Class to represent a call card in a play deck.

Call cards hold a list of string fragments as their text
property. Between each two adjacent fragment is a \"gap\"
that should be filled by a response card's text.

See CARD
See ADD-CALL")

  (function required-responses
    "Return the number of required response cards to properly fill this call card.

See CALL
See RESULT")

  (function add-call
    "Add a new call card to the deck.

See CALL
See CALLS
See PARSE-CALL")

  (function parse-call
    "Parse the given string into a list of strings suitable for a call card. The gaps in the string are represented by two or more underscores.")

  (type response
    "Class to represent a response card in a play deck.

Response cards are used to fill the gaps in call cards.

See CARD
See ADD-RESPONSE")

  (function add-response
    "Add a new response card to the deck.

See RESPONSE
See RESPONSES")

  (type result
    "Container class to represent a possible answer to a call.

Contains a single call card and a number of response cards
that should be used to fill the gaps in the call card.
Calling TEXT on a result will give the currently filled in
call card text as per the result cards entered into the result.

See CALL
See RESPONSES
See REQUIRED-RESPONSES
See REMAINING-RESPONSES
See COMPLETE-P
See ADD-RESPONSE
See TEXT")

  (function remaining-responses
    "Return the number of remaining response cards necessary to satisfy the result.

See RESULT")

  (function complete-p
    "Returns whether the result is complete or needs more response cards.

See RESULT")

  (function clean-text
    "Strip the text of superfluous whitespace and such in an effort to make it cleaner."))

;; game.lisp
(docs:define-docs
  (type player
    "Class to represent a player in a game.

This keeps track of the associated user object, the game
they are playing in, the hand and score they have, and
their current response.

See USER
See GAME
See HAND
See SCORE
See RESULT
See COMPLETE-P
See REMAINING-RESPONSES
See DRAW-CARDS
See NEXT-ROUND")

  (function game
    "Accessor to the game object the player is part of.

See GAME
See PLAYER")

  (function hand
    "Accessor to the player's list of response cards they currently hold in their hand.

See PLAYER
See RESPONSE")

  (function score
    "Accessor to the player's current score.

The score is the number of rounds that they've won.

See PLAYER")

  (function result
    "Accessor to the player's result response for the current round.

See PLAYER
See RESULT")

  (function draw-cards
    "Draws new cards for the player until the hand has the necessary amount again.

See HAND-SIZE
See HAND
See GAME
See PLAYER")

  (function next-round
    "Advance the object to the next round of the game.

For a player this means redrawing cards into the hand,
putting the used up response cards back into the game
deck, and preparing a new response object.

For a game this means removing the current call card,
checking whether someone won the game by max score,
otherwise picking the next officer and scrambling the
numbers used for each player.

See PLAYER")

  (type game
    "Class to represent a game of Crimes.

This holds all the necessary state to play a game of
crimes.

See CHANNEL
See CALLS
See RESPONSES
See PLAYERS
See SCRAMBLED
See HAND-SIZE
See WIN-SCORE
See IN-SESSION
See ADD-DECK
See OFFICER
See START
See END
See JOIN
See LEAVE
See SUBMIT
See COMPLETE-P
See WINNER
See FINISH-ROUND
See NEXT-ROUND")

  (function players
    "Accessor to the list of players participating in this game.

See GAME
See JOIN
See LEAVE")

  (function scrambled
    "Accessor to the list of index numbers for each player.

The list is in the same order as the players list and
associates a unique number to each player. This number
is used to represent the results in an anonymous manner
such that the person who played it cannot be determined.

See GAME")

  (function hand-size
    "Accessor to the number of response cards every player should hold in their hand.

See GAME
See HAND")

  (function win-score
    "Accessor to the maximum score necessary for a player to win the game.

See GAME
See SCORE")

  (function in-session
    "Accessor to whether the game is started or whether it's still in \"lobby mode\" and waiting for players to join.

See GAME")

  (function add-deck
    "Add a deck of cards to the game's set.

If this is attempted to be done while the game is in
session, an error is signalled.

See DECK
See GAME")

  (function officer
    "Returns the player who is currently designated as the officer in the game.

See PLAYER
See GAME")

  (function end
    "End the game immediately.

See IN-SESSION
See GAME")

  (function join
    "Make a user join the game.

An error is signalled if the user is already playing.
If the game is already in session, the user will only
be participating in the next round.

See GAME
See LEAVE")

  (function leave
    "Make a user leave the game

The player's cards are re-added to the game's set and
if no more players are in the game, the game is
automatically ended.

See GAME
See JOIN")

  (function submit
    "Submit a response on behalf of the player for the given game.

An error is signalled if:
- The response is not a card on the player's hand
- The player is currently the officer
- The response has already been submitted
- The index of the response is out of bounds

See PLAYER
See GAME
See ADD-RESPONSE
See RESULT")

  (function winner
    "Returns the winner of the game if the game is complete and has any players to pick from.

The secondary value is the player's score.")

  (function finish-round
    "Finish the round by picking a winner.

An error is signalled if a player is picked that is
not participating in the game, or if the officer
picks themselves. The next round is started
automatically.

See NEXT-ROUND"))

;; interface.lisp
(docs:define-docs
  (type crimes
    "This is an implementation of the popular Cards Against Humanity card game. It works by displaying the hands of the players in a private conversation. Picks and results are shown in a global channel. See 'open crimes' for how to start a game.")

  (function find-game
    "Attempt to find the game for the given event and client.

If no such game can be found, and ERROR is non-NIL,
an error is signalled.")

  (function find-player
    "Attempt to find the player of a game for the given event and client.

If no such player can be found, and ERROR is non-NIL,
an error is signalled.")

  (function user-game
    "Attempt to find the game for the user of the given event and client.

If no such game can be found, and ERROR is non-NIL,
an error is signalled.")

  (function handle-next
    "Handle the next round for the game.

This will announce the next officer and redisplay the
hands for each of the players. If the game is over it
will announce the winner and their score.")

  (function handle-complete
    "Handle the case when everyone has submitted a complete result for the round.

This displays all the choices and asks the officer to
pick a criminal.")

  (command open-game
    "Start a new game of crimes. You can specify the number of points necessary to win, and the number of cards for  each player. Once opened, add cards with 'add crimes deck' and enter the game with 'join crimes'. When everyone is ready, start the game proper with 'start crimes'.")

  (command add-deck
    "Add a new card deck to the stack of the current game. This can only be done by players.")

  (command start-game
    "Actually start the opened game. Players can still enter later, while the game is running.")

  (command end-game
    "End the currently opened game early.")

  (command join-game
    "Join the current crimes game session. You'll be entered as a new user and be shown your hand in a private conversation once the next round begins.")

  (command leave-game
    "Leave the current crimes game session. You can also end the game entirely with 'end crimes'.")

  (command submit-card
    "Submit the selected card(s) as your pick for the current round. You can use this command multiple times, or supply multiple card numbers at once. You should do this in the private conversation. You cannot do this when you are the officer.")

  (command select-winner
    "Select the criminal who won the current round. Only the officer can do this.")

  (command create-deck
    "Create a new card deck from scratch. Use 'add crime call' and 'add crime response' to add call and response cards respectively. Note that you can also download decks from cardcast with 'search for crime deck' and 'download crime deck'.")

  (command remove-deck
    "Remove an existing card deck.")

  (command list-decks
    "Show a list of the names of all locally available decks.")

  (command search-deck
    "Search for decks on CardCast. This will return a list of deck IDs that matched the query.")

  (command download-deck
    "Download a crime deck from cardcastgame.com. You must supply the deck's ID. Use the website or the command 'search for crime deck' to find the ID of a deck.")

  (command add-call
    "Add a call card to a card deck. Use two or more underscores to signify the blanks that people should fill in.")

  (command add-response
    "Add a response card to a card deck.")

  (command remove-card
    "Remove a card from a card deck. The text must match exactly."))

;; toolkit.lisp
(docs:define-docs
  (function rotatef-list
    "Rotates the list once by pushing the first item to the end.")

  (function push-to-end
    "Add the item to the end of the list."))
