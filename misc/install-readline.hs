#!/bin/bash

brew install readline
cabal install readline --extra-include-dirs=/usr/local/Cellar/readline/6.2.4/include/ --extra-lib-dirs=/usr/local/Cellar/readline/6.2.4/lib/ --configure-option=--with-readline-includes=/usr/local/Cellar/readline/6.2.4/include/ --configure-option=--with-readline-libraries=/usr/local/Cellar/readline/6.2.4/lib/
