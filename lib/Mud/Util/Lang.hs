{-# LANGUAGE OverloadedStrings #-}

module Mud.Util.Lang where -- TODO: This module should be moved to Mud.Data.State.Util.

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Util.List
import Mud.Util.Operators

import Data.List (delete)
import Data.Monoid ((<>))


langsNoCommon :: [Lang]
langsNoCommon = CommonLang `delete` (allValues :: [Lang])


mkInLangTxtForLang :: Lang -> CmdName
mkInLangTxtForLang l = l /= CommonLang |?| (" in " <> pp l)


mkCmdNameForLang :: Lang -> CmdName
mkCmdNameForLang CommonLang = "say"
mkCmdNameForLang l          = pp l
