## About
This agent provides a simple trivia game for players in a chat channel. It also allows players to create, edit, and manage the existing questions in the system. Unfortunately, due to it being seemingly impossible to find a machine-readable, high-quality, free set of trivia questions on the internet, no standard set of questions is included in this library.

## How To
Start a new trivia game by selecting a set of categories for the questions to use.

    ::start trivia game science technology

It should not show a first question. If someone in the channel answers the question correctly, they will be awarded points, and the next question is shown. If they are quick enough, they get a time bonus. If nobody manages to answer the question correctly, you can request a hint.

    ::hint answer

Not all questions include a hint however, and not all hints may be sufficiently indicative of the correct answer. You can also skip the question entirely.

    ::skip question

Finally, the game ends when all questions have been answered, or when it is ended explicitly.

    :end trivia game

At the end the winner is announced.

See the rest of the commands on how to manage the questions.
