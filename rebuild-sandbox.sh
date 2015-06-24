#!/bin/bash

cabal sandbox delete
cabal sandbox init
cabal update
cabal install --only-dependencies --enable-tests
cabal install hlint
cabal configure --enable-tests
