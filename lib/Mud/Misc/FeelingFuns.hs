{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.FeelingFuns (feelingFuns) where

import Mud.Data.State.MudData
-- import Mud.Data.State.Util.Get
-- import Mud.Data.State.Util.Misc
-- import Mud.Data.State.Util.Output
-- import Mud.Data.State.Util.Random
-- import Mud.Util.Misc


feelingFuns :: [(FeelingTag, FeelingFun)]
feelingFuns = [ ("potHp",        potHpFeelingFun)
              , ("potInstantHp", potHpFeelingFun) ]


-----


potHpFeelingFun :: FeelingFun
potHpFeelingFun NoVal      = ""
potHpFeelingFun (IntVal 0) = ""
potHpFeelingFun (IntVal _) = "You feel your wounds heal as a warm sensation pulsates outward from your stomach and \
                             \throughout your torso."
