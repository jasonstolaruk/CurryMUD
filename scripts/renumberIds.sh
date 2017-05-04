#!/bin/bash

echo "Renumbering IDs."
cd ~/CurryMUD
stack exec -- runghc ./scripts/renumberIds.hs
