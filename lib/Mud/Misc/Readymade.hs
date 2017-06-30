module Mud.Misc.Readymade (readymadeDwarf) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Lang
import Mud.Data.State.Util.Misc
import Mud.Util.Misc

import Control.Lens.Operators ((.~))


readymadeDwarf :: Id -> MudStack ()
readymadeDwarf i = readymadeHelper i Dwarf 85 65 70 10 20


readymadeHelper :: Id -> Race -> Int -> Int -> Int -> Int -> Int -> MudStack ()
readymadeHelper i r a b c d e = tweaks [ pcTbl     .ind i.race       .~ r
                                       , mobTbl    .ind i.knownLangs .~ pure (raceToLang r)
                                       , mobTbl    .ind i.st         .~ a
                                       , mobTbl    .ind i.dx         .~ b
                                       , mobTbl    .ind i.ht         .~ c
                                       , mobTbl    .ind i.ma         .~ d
                                       , mobTbl    .ind i.ps         .~ e
                                       , pickPtsTbl.ind i            .~ 0 ]
