module Mud.Misc.Readymade ( readymadeDwarf
                          , readymadeElf
                          , readymadeFelinoid
                          , readymadeHobbit
                          , readymadeHuman
                          , readymadeLagomorph
                          , readymadeNymph
                          , readymadeVulpenoid ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Lang
import Mud.Data.State.Util.Misc
import Mud.Util.Misc

import Control.Lens.Operators ((.~))

readymadeHelper :: Id -> Race -> Int -> Int -> Int -> Int -> Int -> MudStack ()
readymadeHelper i r a b c d e = tweaks [ pcTbl     .ind i.race       .~ r
                                       , mobTbl    .ind i.knownLangs .~ pure (raceToLang r)
                                       , mobTbl    .ind i.st         .~ a
                                       , mobTbl    .ind i.dx         .~ b
                                       , mobTbl    .ind i.ht         .~ c
                                       , mobTbl    .ind i.ma         .~ d
                                       , mobTbl    .ind i.ps         .~ e ]

readymadeDwarf :: Id -> MudStack ()
readymadeDwarf i = readymadeHelper i Dwarf 85 65 70 10 20

readymadeElf :: Id -> MudStack ()
readymadeElf i = readymadeHelper i Elf 30 40 40 70 70

readymadeFelinoid :: Id -> MudStack ()
readymadeFelinoid i = readymadeHelper i Felinoid 65 90 65 10 20

readymadeHobbit :: Id -> MudStack ()
readymadeHobbit i = readymadeHelper i Hobbit 40 70 50 70 20

readymadeHuman :: Id -> MudStack ()
readymadeHuman i = readymadeHelper i Human 70 50 50 10 70

readymadeLagomorph :: Id -> MudStack ()
readymadeLagomorph i = readymadeHelper i Lagomorph 50 50 50 10 90

readymadeNymph :: Id -> MudStack ()
readymadeNymph i = readymadeHelper i Nymph 40 50 50 90 20

readymadeVulpenoid :: Id -> MudStack ()
readymadeVulpenoid i = readymadeHelper i Vulpenoid 95 70 55 10 20
