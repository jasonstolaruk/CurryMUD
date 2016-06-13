{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.FeelingFuns (feelingFuns) where

import Mud.Data.State.MudData


feelingFuns :: [(FeelingTag, FeelingFun)]
feelingFuns = [ ("potHp",        potHpFeelingFun)
              , ("potInstantHp", potHpFeelingFun) ]


-----


potHpFeelingFun :: FeelingFun
potHpFeelingFun NoVal      = ""
potHpFeelingFun (IntVal 0) = ""
potHpFeelingFun (IntVal _) = "You feel your wounds heal as a warm sensation pulsates outward from your stomach and \
                             \throughout your torso."
