{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Lang ( langsNoCommon
                                , mkCmdNameForLang
                                , mkInLangTxtForLang
                                , raceToLang ) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Util.List
import Mud.Util.Operators

import Data.List (delete)
import Data.Monoid ((<>))

langsNoCommon :: [Lang]
langsNoCommon = CommonLang `delete` allValues

mkCmdNameForLang :: Lang -> CmdName
mkCmdNameForLang CommonLang = "say"
mkCmdNameForLang l          = pp l

mkInLangTxtForLang :: Lang -> CmdName
mkInLangTxtForLang l = l /= CommonLang |?| (" in " <> pp l)

raceToLang :: Race -> Lang
raceToLang Dwarf     = DwarfLang
raceToLang Elf       = ElfLang
raceToLang Felinoid  = FelinoidLang
raceToLang Hobbit    = HobbitLang
raceToLang Human     = HumanLang
raceToLang Lagomorph = LagomorphLang
raceToLang Nymph     = NymphLang
raceToLang Vulpenoid = VulpenoidLang
