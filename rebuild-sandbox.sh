#!/bin/bash

cabal sandbox delete
cabal sandbox init
cabal update
cabal install cabal-install
cabal install --only-dependencies --enable-tests
cabal install hlint
cabal configure --enable-tests --enable-library-profiling --enable-executable-profiling
