(in-package #:maiden-user)
(defpackage #:maiden-crimes
  (:nicknames #:org.shirakumo.maiden.agents.crimes)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities #:maiden-api-access)
  ;; cardcast.lisp
  (:export
   #:*cardcast/decks*
   #:*cardcast/deck*
   #:*cardcast/deck/cards*
   #:cardcast/decks
   #:cardcast/deck
   #:cardcast/deck/cards
   #:load-cardcast-deck
   #:find-cardcast-decks)
  ;; cards.lisp
  (:export
   #:*decks*
   #:deck
   #:remove-deck
   #:deck
   #:name
   #:title
   #:calls
   #:responses
   #:list-calls
   #:list-responses
   #:save-deck
   #:load-deck
   #:remove-card
   #:card
   #:text
   #:call
   #:add-call
   #:response
   #:add-response
   #:result
   #:call
   #:responses
   #:required-responses
   #:remaining-responses
   #:complete-p
   #:text)
  ;; game.lisp
  (:export
   #:player
   #:user
   #:game
   #:hand
   #:score
   #:result
   #:complete-p
   #:remaining-responses
   #:draw-cards
   #:next-round
   #:game
   #:channel
   #:calls
   #:responses
   #:players
   #:scrambled
   #:hand-size
   #:win-score
   #:in-session
   #:add-deck
   #:officer
   #:start
   #:end
   #:join
   #:leave
   #:submit
   #:winner
   #:finish-round
   #:next-round)
  ;; interface.lisp
  (:export
   #:crimes
   #:games
   #:user-game
   #:open-game
   #:add-deck
   #:start-game
   #:end-game
   #:join-game
   #:leave-game
   #:submit-card
   #:select-winner
   #:create-deck
   #:remove-deck
   #:list-decks
   #:search-deck
   #:download-deck
   #:add-call
   #:add-response
   #:remove-card)
  ;; toolkit.lisp
  (:export))
