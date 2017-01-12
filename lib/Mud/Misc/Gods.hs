{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.Gods where

import Mud.Data.Misc
import Mud.Data.State.MudData
-- import Mud.Util.Text
-- import Mud.Util.Wrapping

-- import Data.Monoid ((<>))
-- import Data.Text (Text)
import qualified Data.Set as S (Set, fromList)
-- import qualified Data.Text as T


godSet :: S.Set God -- TODO: Illegal PC names.
godSet = S.fromList [ God GodOfArtAndEngineering ""         . Just $ (Female, Elf      )
                    , God GodOfDarkness          ""           Nothing
                    , God GodOfDebauchery        "Oloro"    . Just $ (Male,   Felinoid )
                    , God GodOfHarvest           "Fen"      . Just $ (Female, Human    )
                    , God GodOfTheMoonAndMagic   ""         . Just $ (Male,   Hobbit   )
                    , God GodOfNature            "Rumialis" . Just $ (Male,   Nymph    )
                    , God GodOfPsionics          "Poe"      . Just $ (Female, Lagomorph)
                    , God GodOfVirtue            "Irier"    . Just $ (Female, Vulpenoid)
                    , God GodOfWealth            ""         . Just $ (Male,   Dwarf    ) ]
