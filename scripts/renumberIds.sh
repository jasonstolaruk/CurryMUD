#!/bin/bash

cd ~/CurryMUD
stack exec -- runghc ./scripts/renumberIds.hs

echo "Renumbered IDs."
