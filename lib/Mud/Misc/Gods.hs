{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.Gods where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Util.Misc

import qualified Data.Set as S (Set, filter, fromList, toList)


godSet :: S.Set God
godSet = S.fromList [ God Aule      GodOfWealth            . Just $ (Male,   Dwarf    ) -- Son of Drogo and Celoriel.
                    , God Caila     GodOfHarvest           . Just $ (Female, Human    ) -- Daughter of Drogo and Celoriel.
                    , God Celoriel  GodOfPsionics          . Just $ (Female, Lagomorph) -- Wife of Drogo.
                    , God Dellio    GodOfDebauchery        . Just $ (Female, Felinoid ) -- Daughter of Murgorhd and Iminye.
                    , God Drogo     GodOfMoonAndMagic      . Just $ (Male,   Hobbit   ) -- Husband of Celoriel.
                    , God Iminye    GodOfArtAndEngineering . Just $ (Female, Elf      ) -- Daughter of Drogo and Celoriel.
                    , God Itulvatar GodOfLight               Nothing
                    , God Murgorhd  GodOfDarkness            Nothing
                    , God Rhayk     GodOfWar               . Just $ (Male,   Vulpenoid) -- Son of Murgorhd and Iminye.
                    , God Rumialys  GodOfNature            . Just $ (Male,   Nymph    ) ]


getGodForGodName :: GodName -> Maybe God
getGodForGodName gn = safeHead . S.toList . S.filter (\(God gn' _ _) -> gn' == gn) $ godSet
