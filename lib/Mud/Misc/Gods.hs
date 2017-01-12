{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.Gods where

import Mud.Data.Misc
import Mud.Data.State.MudData

import qualified Data.Set as S (Set, fromList)


godSet :: S.Set God
godSet = S.fromList [ God Aule      GodOfWealth            . Just $ (Male,   Dwarf    )
                    , God Drogo     GodOfTheMoonAndMagic   . Just $ (Male,   Hobbit   )
                    , God GodName   GodOfDarkness            Nothing
                    , God GodName   GodOfDebauchery        . Just $ (Male,   Felinoid )
                    , God GodName   GodOfHarvest           . Just $ (Female, Human    )
                    , God GodName   GodOfPsionics          . Just $ (Female, Lagomorph)
                    , God Iminye    GodOfArtAndEngineering . Just $ (Female, Elf      )
                    , God Itulvatar GodOfVirtue            . Just $ (Female, Vulpenoid)
                    , God Rumialys  GodOfNature            . Just $ (Male,   Nymph    ) ]
