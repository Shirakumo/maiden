## About Maiden-Crimes
This is a clone of the popular Cardcast / Cards Against Humanity / Xyzzy type of game. It supports custom deck creation as well as downloading decks directly from <https://cardcastgame.com>. The game works through text only and should thus be compatible with any kind of client that has support for the [maiden-client-entities](../../modules/client-entities/) protocol.

## How To
First, open up a game lobby

    ::open crimes

Following this, users can enter the game. Users can still enter the game while it is running too, though.

    ::join crimes

Next you should add card decks to the game. You can either create your own decks locally and add those, or search for ones and download them through [Cardcast](https://www.cardcastgame.com/). You only need to download them once, but you do need to add the decks to each new game you start. Here's all of the codes for the official CAH packs for your convenience:

    ::download crime deck CAHBS CAH
    ::download crime deck CAHE1 CAHE1
    ::download crime deck CAHE2 CAHE2
    ::download crime deck CAHE3 CAHE3
    ::download crime deck CAHE4 CAHE4
    ::download crime deck EU6CJ CAHE5
    ::download crime deck PEU3Q CAHE6
    ::download crime deck XMAS1 CAHH2
    ::download crime deck K4QVW CAHH3
    ::download crime deck CDJDV CAHH4
    ::download crime deck BBBOX CAHBB
    ::download crime deck NXEP0 CAH90
    ::download crime deck PNNE9 CAHSCI
    ::download crime deck KW8B6 CAHNS2
    ::download crime deck DP2VU CAHNS3
    ::download crime deck PAXP3 CAHPAX

To create a game with them, simply use the add command. You can also add by CardCast ID directly.

    ::add crimes deck CAH
    ::add crimes deck CAHBB

Once you have all the cards as you like them and all the players who want to play have joined, you can start the game proper.

    ::start crimes

From there on out, every round an officer will be selected, while everyone else is a potential criminal. The criminals will get a private message that lists all the possible cards in their hand that they can use to answer the call card. To enter responses, each player can use

    ::commit crime 2 3 4

Where `2 3 4` are the numbers of the cards the player would like to select. You can also submit multiple times, if you prefer doing that. Each time you submit, it will show you a preview of what your submission will look like. Once all players have submitted enough responses to fill the call, the responses are sorted randomly and presented to all players in the main channel. The officer then gets to convict a criminal.

    ::convict criminal 3

The criminal's name is then revealed, his score is increased, and a new round with a new officer is started. This continues until either the game is ended explicitly, all players have left the game, or a player reaches the necessary score.

See the other commands in the symbol index to see how you can create and manage decks using commands.
