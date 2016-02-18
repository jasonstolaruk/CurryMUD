# CurryMUD

A textual Multi-User Dungeon ("MUD") server in Haskell. (If you are unfamiliar with the term "MUD," please refer to [this Wikipedia article](http://en.wikipedia.org/wiki/MUD).)

CurryMUD is essentially the hobby project and brainchild of a single developer (me). It's been in active development for over 2 years, but is still very much a work in progress.

## My goals

I hope to create a new MUD from the ground up, _written entirely in the Haskell programming language_.

CurryMUD will have the following features:
* Players will be offered an immersive virtual world environment.
* Content will be created, and development will proceed, with the aim of supporting a small community of players.
* Role-playing will be strictly enforced.
* Classless/skill-based.
* Permadeath. (When player characters die, they really die.)
* Some degree of player-created content will be allowed and encouraged.
* The state of the virtual world will be highly persisted upon server shutdown.
* As is common with most textual MUDs, client connections will be supported with a loose implementation of the telnet protocol.
* CurryMUD will always be free to play.

## What I have so far

* Incoming connections are accepted and players are prompted for their character name.
* The names of commands, as well as the names of the targets they act upon, may be abbreviated.
* Commands have a consistent structure and a unique syntax for indicating target locations and quantities.
* About 55 player commands and 40 administrator commands.
* Nearly 200 built-in emotes.
* Unique commands, accessible only when a player is in a particular room, may be created.
* PCs can introduce themselves to each other.
* PCs can "link" with each other so as to enable "tells."
* Players can interact with permanent room fixtures that are not listed in a room's inventory.
* Players can create their own ad-hoc channels.
* Question channel for OOC newbie Q&A.
* Free-form emotes and built-in emotes may be used in "tells" and channel communications.
* Functionality enabling one-on-one communication between players and administrators.
* NPCs can execute commands, either from within code or via the ":as" administrator command.
* Help files for all existing non-debug commands. Help topics.
* The virtual world is automatically persisted at regular intervals and at shutdown.
* Logging.
* ANSI color.
* Systems for reporting bugs and typos.
* Weight and encumbrance.
* Volume and container capacity.
* Durational effects that can be paused and resumed.
* Objects can be configured to automatically disappear when left on the ground for some time.

I am still in the initial stage of developing basic commands. There is very little content in the virtual world.

## About the code

The code is available here on GitHub under the 3-clause BSD license (refer to the [LICENSE file](https://github.com/jasonstolaruk/CurryMUD/blob/master/LICENSE)). Please note that **I am not accepting PRs at this time**.

### Notable features

* A `ReaderT` monad transformer stack with the world state inside a single `IORef`.
* `STM`-based concurrency.
* Using `aeson` with `conduit` and `sqlite-simple` for persistence.
* Heavy use of the `lens` library.
* Heavy use of GHC extensions, including:
  * `LambdaCase`
  * `MonadComprehensions`
  * `MultiWayIf`
  * `NamedFieldPuns`
  * `OverloadedStrings`
  * `ParallelListComp`
  * `PatternSynonyms`
  * `RebindableSyntax`
  * `RecordWildCards`
  * `TupleSections`
  * `ViewPatterns`
* About 80 modules, excluding tests.
* About 60 unit and property tests exist (I'm using the [tasty testing framework](https://hackage.haskell.org/package/tasty)).

### How to try it out

If you'd like to try out what I have so far, clone the repo from your home directory (the server expects to find various folders located under `$HOME/CurryMUD`). Having done that, please build and run the project using [stack](http://docs.haskellstack.org/en/stable/README.html) (otherwise, I cannot guarantee that CurryMUD will build on your machine). Please note that I do not plan on explicitly supporting Windows.

Once you have the server up and running, just telnet to localhost port 9696 to play.

## How to contact me

Feel free to email me at the address associated with [my GitHub account](https://github.com/jasonstolaruk) if you have any questions.
