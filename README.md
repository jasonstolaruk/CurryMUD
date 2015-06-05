# CurryMUD

A textual Multi-User Dungeon ("MUD") server in Haskell. (If you are unfamiliar with the term "MUD," please refer to [this Wikipedia article](http://en.wikipedia.org/wiki/MUD).)

CurryMUD is essentially the hobby project and brainchild of a single developer (me). It's been in active development for well over a year, but is still very much a work in progress.

## My goals

I hope to create a new MUD from the ground up, _written entirely in the Haskell programming language_.

CurryMUD will have the following features:
* Players will be offered a safe and immersive virtual world environment in which they are free to explore identity.
* Content will be created and development will proceed with the aim of supporting a small community of players.
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
* About 40 player commands and 20 administrator commands.
* About 180 built-in emotes.
* Help files for all existing non-debug commands.
* The virtual world is automatically persisted at regular intervals and at shutdown.
* Logging.
* ANSI color.
* Systems for reporting bugs and typos.
* Functionality for various levels of communication.
* Weight and encumbrance.

I am still in the initial stage of developing basic commands. There is very little content in the virtual world.

## About the code

The code is open source and available here on GitHub under the 3-clause BSD license (refer to the [LICENSE file](https://github.com/jasonstolaruk/CurryMUD/blob/master/LICENSE)). Please note that **I am not accepting PRs at this time**.

### Notable features

* A `ReaderT` monad transformer stack with the world state inside a single `IORef`.
* `STM`-based concurrency.
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
* Over 40 modules.
* A handful of unit and property tests exist (I'm using the [tasty testing framework](https://hackage.haskell.org/package/tasty)).

### How to try it out

If you'd like to try out what I have so far, just clone the repo and build the project. You shouldn't have any issues compiling with GHC 7.8 on Mac OS X or Linux (I do not plan on explicitly supporting Windows). If you do run into issues, please email me and let me know. Once you have the server up and running, just telnet to localhost port 9696 to play.

## How to contact me

Feel free to email me (at the address associated with [my GitHub account](https://github.com/jasonstolaruk)) if you have any questions.
