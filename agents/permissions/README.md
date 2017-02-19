## About
This agent implements a permissions system, allowing you to specify more finely-grained who gets to do what.

## How To
After adding the agent to the core, you'll first want to call `generate-token`, which will return you a new administrator authentication token. You should then use this token in the network/client you want to become administrator on like so:

    ::upgrade to administrator dadwa213185233231...

Note that you will have to repeat this step for every client, unless you also have an [account](../accounts/) set up that does the tracking for you. From there on out you can check access to commands and grant people rights, or deny them explicitly.

Permissions are modelled as lists of tokens, which form trees. Each token defines a "subtree" that is either granted or denied. In order to specify a denying permission, simply prefix the first token with a bang. Users are also denoted by such permission lists, where the list starts with the user's name, followed by the client's name, followed by its class description. Thus you can increasingly finely grant a specific user, or all users with that name permissions.

Beware of simply blanket-granting just a username permissions, though. Some clients cannot always guarantee the authenticity of a username, and it isn't guaranteed that the same username denotes the same individual over different networks anyway.

See the symbol index for the possible commands available to manage permissions.
