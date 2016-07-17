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

* Incoming connections are accepted; players are prompted for their character name and password.
* Character creation.
* The names of commands, as well as the names of the targets they act upon, may be abbreviated.
* Commands have a consistent structure and a unique syntax for indicating target locations and quantities.
* About 80 player commands and 45 administrator commands.
* Nearly 200 built-in emotes.
* Unique commands, accessible only when a player is in a particular room, may be created.
* PCs can introduce themselves to each other.
* PCs can "link" with each other so as to enable "tells."
* Players can create their own ad-hoc channels.
* Question channel for OOC newbie Q&A.
* Free-form emotes and built-in emotes may be used in "tells" and channel communications.
* Racial say. Whisper.
* Functionality enabling one-on-one communication between players and administrators.
* Players can interact with permanent room fixtures that are not listed in a room's inventory.
* NPCs can execute commands, either from within code or via the ":as" administrator command.
* Help files for all existing non-debug commands. Help topics.
* The virtual world is automatically persisted at regular intervals and at shutdown.
* Logging.
* ANSI color.
* Systems for reporting bugs and typos.
* Commands to aid in the process of resetting a forgotten password.
* Weight and encumbrance.
* Volume and container capacity.
* Vessels for containing liquids. Vessels may be filled and emptied.
* Eating foods and drinking liquids. Digestion.
* Smell and taste. Listen.
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
  * `DuplicateRecordFields` (new in GHC 8)
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
* About 90 modules, excluding tests.
* About 60 unit and property tests exist (I'm using the [tasty testing framework](https://hackage.haskell.org/package/tasty)).

### How to try it out

I do not plan on explicitly supporting Windows.

Please use [stack](http://docs.haskellstack.org/en/stable/README.html) (otherwise, I cannot guarantee that CurryMUD will build on your machine).

0. [Install stack.](http://docs.haskellstack.org/en/stable/install_and_upgrade/)
0. Clone the repo from your home directory (the server expects to find various folders under `$HOME/CurryMUD`).
0. Inside `$HOME/CurryMUD`, run `stack setup` to get GHC 8 on your machine. (The `stack.yaml` file points to the [nightly resolver](https://www.stackage.org/snapshots), which uses GHC 8.)
0. Run `stack build` to compile the `curry` binary and libraries.
0. Run `stack install` to copy the `curry` binary to `$HOME/.local/bin`.
0. Execute the `curry` binary.
0. Telnet to `localhost` port 9696 to play.

CurryMUD presently cannot be loaded into GHCi due to [a GHC bug](https://ghc.haskell.org/trac/ghc/ticket/12007).

## How to contact me

Feel free to email me at the address associated with [my GitHub account](https://github.com/jasonstolaruk) if you have any questions.
