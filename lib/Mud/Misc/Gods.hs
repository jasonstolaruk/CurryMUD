{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.Gods where

import Mud.Data.Misc
import Mud.Data.State.MudData

import qualified Data.Set as S (Set, fromList)


godSet :: S.Set God
godSet = S.fromList [ God Aule      GodOfWealth            . Just $ (Male,   Dwarf    )
                    , God Caila     GodOfHarvest           . Just $ (Female, Human    )
                    , God Celoriel  GodOfPsionics          . Just $ (Female, Lagomorph)
                    , God Dellio    GodOfDebauchery        . Just $ (Male,   Felinoid )
                    , God Drogo     GodOfTheMoonAndMagic   . Just $ (Male,   Hobbit   )
                    , God Iminye    GodOfArtAndEngineering . Just $ (Female, Elf      )
                    , God Itulvatar GodOfVirtue            . Just $ (Female, Vulpenoid)
                    , God Morgorhd  GodOfDarkness            Nothing
                    , God Rumialys  GodOfNature            . Just $ (Male,   Nymph    ) ]
