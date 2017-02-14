{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Lang where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Util.List
import Mud.Util.Operators

import Data.List (delete)
import Data.Monoid ((<>))


langsNoCommon :: [Lang]
langsNoCommon = CommonLang `delete` allValues


mkInLangTxtForLang :: Lang -> CmdName
mkInLangTxtForLang l = l /= CommonLang |?| (" in " <> pp l)


mkCmdNameForLang :: Lang -> CmdName
mkCmdNameForLang CommonLang = "say"
mkCmdNameForLang l          = pp l
