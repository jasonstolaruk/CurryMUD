# CurryMUD

A textual Multi-User Dungeon ("MUD") server in Haskell. (If you are unfamiliar with the term "MUD," please refer to [this Wikipedia article](http://en.wikipedia.org/wiki/MUD).)

CurryMUD is essentially the hobby project and brainchild of a single developer (me). It's been in active development for over 3 years, but is still very much a work in progress.

## My goals

My aim is to create a single unique, playable MUD named "CurryMUD." I am writing this MUD entirely in Haskell, from scratch.

Creating a framework which others can leverage to develop their own MUDs is _not_ an explicit goal of mine, nor is this a collaborative effort (I am not accepting PRs). Having said that, the code is available here on GitHub, so other parties _are_ free to examine the code and develop their own forks. [Please refer to the license](https://github.com/jasonstolaruk/CurryMUD/blob/master/LICENSE), which is a 3-clause BSD license with additional unique clauses regarding the creation of derivative MUDs.

CurryMUD will have the following features:

* Players will be offered an immersive virtual world environment.
* Content will be created and development will proceed with the aim of supporting a small community of players.
* Role-playing will be strictly enforced.
* The focus will be on PvE (as opposed to PvP).
* Classless/skill-based.
* Permadeath. (When player characters die, they really die.)
* Some degree of player-created content will be allowed and encouraged.
* The state of the virtual world will be highly persisted upon server shutdown.
* As is common with most textual MUDs, client connections will be supported with a loose implementation of the telnet protocol.
* CurryMUD will always be free to play. No pay-to-win.

## What I have so far

* About 90 player commands, 55 administrator commands, and 65 commands for debugging purposes. :nut_and_bolt:
* About 220 built-in emotes. :clap:
* Help files for all existing non-debug commands. Help topics. :information_desk_person:
* Commands have a consistent structure and a unique syntax for indicating target locations and quantities. :dart:
* Unique commands, accessible only when a player is in a particular room, may be created. :house_with_garden:
* The names of commands, as well as the names of the targets they act upon, may be abbreviated. :abc:
* Logging. :scroll:
* About 85 player commands, 50 administrator commands, and 60 commands for debugging purposes. :1234:
* About 220 built-in emotes. :clap:
* Help files for all existing non-debug commands. Help topics. :information_desk_person:
* Commands have a consistent structure and a unique syntax for indicating target locations and quantities. :dart:
* Unique commands, accessible only when a player is in a particular room, may be created. :house_with_garden:
* The names of commands, as well as the names of the targets they act upon, may be abbreviated. :ok:
* Logging. :scroll:
* ANSI color. :red_circle:
* Character creation. :runner:
* The virtual world is automatically persisted at regular intervals and at shutdown. :floppy_disk:
* Systems for reporting bugs and typos. :bug:
* Commands to aid in the process of resetting a forgotten password. :passport_control:
* NPCs can execute commands, either from within code or via the ":as" administrator command. :performing_arts:
* PCs can introduce themselves to each other. :bow:
* PCs can "link" with each other so as to enable "tells." :link:
* Question channel for OOC newbie Q&A. :question:
* Players can create their own ad-hoc channels. :busts_in_silhouette:
* Free-form emotes and built-in emotes may be used in "tells" and channel communications. :clap:
* Functionality enabling one-on-one communication between players and administrators. :speech_balloon:
* Gods. An origin myth describing the creation of the universe. :godmode:
* An in-game calendar. :calendar:
* Weight and encumbrance. :chart_with_downwards_trend:
* Volume and container capacity. :school_satchel:
* Vessels for containing liquids. Vessels may be filled and emptied. :wine_glass:
* Players can interact with permanent room fixtures that are not listed in a room's inventory. :fountain:
* Objects can be configured to automatically disappear when left on the ground for some time. :boom:
* Smell and taste. Listen. :nose::tongue::ear:
* Drinking. Digestion. :beer:
* Durational effects that can be paused and resumed. :dizzy:
* PC and NPC death. Corpse decomposition. :skull:
* Corpses may be sacrificed using the holy symbol of a particular god. :pray:
* Upon death, PCs may have a limited amount of time to exist in the virtual world as a spirit. :angel:
* Sending [GMCP](https://www.gammon.com.au/gmcp) `Char.Vitals` and `Info.Room`. :satellite:
* [Mudlet scripts](https://github.com/jasonstolaruk/CurryMUD/tree/master/Mudlet) for vitals gauges and mapping. :scroll:

I am still in the initial stage of developing basic commands. There is very little content in the virtual world.

## About the code

The code is available here on GitHub under [this license](https://github.com/jasonstolaruk/CurryMUD/blob/master/LICENSE) (a 3-clause BSD license with additional unique clauses regarding the creation of derivative MUDs.) Please note that **I am not accepting PRs**.

* About 40,000 lines of code/text.
* About 110 modules, excluding tests.
* About 105 unit and property tests exist (I'm using the [tasty testing framework](https://hackage.haskell.org/package/tasty)).
* A `ReaderT` monad transformer stack with the world state inside a single `IORef`.
* `STM`-based concurrency.
* Using `aeson` (with `conduit`) and `sqlite-simple` for persistence.
* Heavy use of the `lens` library.
* Heavy use of GHC extensions, including:
  * `DuplicateRecordFields` (new in GHC 8)
  * `LambdaCase`
  * `MonadComprehensions`
  * `MultiWayIf`
  * `NamedFieldPuns`
  * `ParallelListComp`
  * `PatternSynonyms`
  * `RebindableSyntax`
  * `RecordWildCards`
  * `TupleSections`
  * `ViewPatterns`
* Many functions are decorated with [the `HasCallStack` constraint](http://hackage.haskell.org/package/base-4.9.0.0/docs/GHC-Stack.html#t:HasCallStack). I hope to remove these when I'm convinced that the code is stable.

### How to try it out

Linux and macOS are supported. Sorry, but Windows is _not_ supported.

Please build with [stack](http://docs.haskellstack.org/en/stable/README.html) (otherwise I cannot guarantee that CurryMUD will build on your machine).

0. [Install stack.](http://docs.haskellstack.org/en/stable/install_and_upgrade/)
0. Clone the repo from your home directory (the server expects to find various folders under `$HOME/CurryMUD`).
0. Inside `$HOME/CurryMUD`, run `stack setup` to get GHC 8 on your machine. (The `stack.yaml` file points to [a recent nightly resolver](https://www.stackage.org/snapshots), which uses GHC 8.)
0. Run `stack build` to compile the `curry` binary and libraries.
0. Run `stack install` to copy the `curry` binary to `$HOME/.local/bin`.
0. Execute the `curry` binary.
0. Telnet to `localhost` port 9696 to play.

CurryMUD presently cannot be loaded into GHCi due to [a GHC bug](https://ghc.haskell.org/trac/ghc/ticket/12007).

## How to contact me

Feel free to email me at the address associated with [my GitHub account](https://github.com/jasonstolaruk) if you have any questions.
