{-# LANGUAGE OverloadedStrings #-}

module Mud.Util.Lang where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Util.Operators

import Data.Monoid ((<>))


mkInLangTxtForLang :: Lang -> CmdName
mkInLangTxtForLang l = l /= CommonLang |?| (" in " <> pp l)


mkCmdNameForLang :: Lang -> CmdName
mkCmdNameForLang CommonLang = "say"
mkCmdNameForLang l          = pp l
