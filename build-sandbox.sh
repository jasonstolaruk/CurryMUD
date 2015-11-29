#!/bin/bash

cabal sandbox delete
cabal sandbox init
cabal update
cabal install -j --only-dependencies --enable-tests
cabal install -j hlint
cabal configure --enable-tests
