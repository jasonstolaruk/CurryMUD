# CurryMUD

A textual Multi-User Dungeon ("MUD") server in Haskell. (If you are unfamiliar with the term "MUD," please refer to [this Wikipedia article](http://en.wikipedia.org/wiki/MUD).)

CurryMUD is essentially the hobby project and brainchild of a single developer (me). It's been in active development for about 2 years, but is still very much a work in progress.

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
* Financial gain is **not** a goal, and CurryMUD will always be free to play.

## What I have so far

* Incoming connections are accepted and players are prompted for their character name.
* The names of commands, as well as the names of the targets they act upon, may be abbreviated.
* Commands have a consistent structure and a unique syntax for indicating target locations and quantities.
* About 50 player commands and 25 administrator commands.
* About 180 built-in emotes.
* PCs can introduce themselves to each other.
* PCs can "link" with each other so as to enable "tells."
* PCs can create their own ad-hoc channels.
* Question channel for OOC newbie Q&A.
* Free-form emotes and built-in emotes may be used in "tells" and channel communications.
* Functionality enabling one-on-one communication between players and admins.
* Help files for (almost) all existing non-debug commands.
* The virtual world is automatically persisted at regular intervals and at shutdown.
* Logging.
* ANSI color.
* Systems for reporting bugs and typos.
* Weight and encumbrance.

I am still in the initial stage of developing basic commands. There is very little content in the virtual world.

## About the code

The code is open source and available here on GitHub under the 3-clause BSD license (refer to the [LICENSE file](https://github.com/jasonstolaruk/CurryMUD/blob/master/LICENSE)). Please note that **I am not accepting PRs at this time**.

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
  * `TemplateHaskell`
  * `TupleSections`
  * `ViewPatterns`
* About 65 modules, excluding tests.
* A handful of unit and property tests exist (I'm using the [tasty testing framework](https://hackage.haskell.org/package/tasty)).

### How to try it out

If you'd like to try out what I have so far, just clone the repo from your home directory and build the project. You'll need GHC 7.10 with base 4.8. You shouldn't have any issues compiling on Mac OS X or Linux (I do not plan on explicitly supporting Windows). If you do run into issues, please email me and let me know. Once you have the server up and running, just telnet to localhost port 9696 to play.

## How to contact me

Feel free to email me at the address associated with [my GitHub account](https://github.com/jasonstolaruk) if you have any questions.
