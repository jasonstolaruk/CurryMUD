CurryMUD
========

A textual Multi-User Dungeon server in Haskell.

Notable features of the code:

* A `ReaderT` monad transformer stack with the world state in a `IORef`.
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

CurryMUD has been in active development for well over a year, but is still very much a work in progress.
If you'd like to try out what I have so far, just clone the repo and build the project. You shouldn't have any issues compiling with GHC 7.8 on Mac OS X or Linux (I do not plan on explicitly supporting Windows). If you do run into issues, please email me (at the address associated with my GitHub account) and let me know. Once you have the server up and running, just telnet to localhost port 9696 to play.

I'm presently not accepting PRs. Feel free to email me if you have any questions.
