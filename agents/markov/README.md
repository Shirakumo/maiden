## About
This provides a fast and compact Markov chains implementation. Markov chains are stored efficiently in memory and on disk, allowing for huge dictionaries without loss of resources or performance.

It also implements its own binary format in order to save the dictionary fast and compact.

## How To
Create a `generator` instance, and train it with `learn` or `learn-from-file`

    (defvar *g* (make-instance 'generator))
    (learn "Hey this is a sentence you can learn." *g*)
    (learn-from-file "~/king-james.txt" *g*)
    (learn-from-file "~/sicp.txt" *g*)
    (make-sentence *g*)
    (find-sentence *g* "lambda")

You can also make it learn from a chat channel by activating the module on that channel. See [maiden-activatable](../activatable/) and the commands in the symbol index.
