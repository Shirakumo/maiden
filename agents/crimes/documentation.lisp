#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.crimes)

(docs:define-docs
  (type crimes
    "This is an implementation of the popular Cards Against Humanity card game. It works by displaying the hands of the players in a private conversation. Picks and results are shown in a global channel. See 'open crimes' for how to start a game.")

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

  (command download-deck
    "Download a crime deck from cardcastgame.com. You must supply the deck's ID. Use the website or the command 'search for crime deck' to find the ID of a deck.")

  (command add-call
    "Add a call card to a card deck. Use two or more underscores to signify the blanks that people should fill in.")

  (command add-response
    "Add a response card to a card deck.")

  (command remove-card
    "Remove a card from a card deck. The text must match exactly."))
