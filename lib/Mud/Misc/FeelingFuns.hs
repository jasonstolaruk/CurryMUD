{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.FeelingFuns (feelingFuns) where

import Mud.Data.State.MudData


feelingFuns :: [(FeelingTag, FeelingFun)]
feelingFuns = [ ("potHp",              potHpFeelingFun      )
              , ("potInstantHp",       potHpFeelingFun      )
              , ("potMp",              potMpFeelingFun      )
              , ("potInstantMp",       potMpFeelingFun      )
              , ("potPp",              potPpFeelingFun      )
              , ("potInstantPp",       potPpFeelingFun      )
              , ("potFp",              potFpFeelingFun      )
              , ("potInstantFp",       potFpFeelingFun      )
              , ("potTinnitus",        potTinnitusFeelingFun) ]


-----


potHpFeelingFun :: FeelingFun
potHpFeelingFun NoVal      = ""
potHpFeelingFun (IntVal 0) = ""
potHpFeelingFun (IntVal _) = "You feel your wounds heal as a warm sensation pulsates outward from your stomach and \
                             \throughout your torso."


-----


potMpFeelingFun :: FeelingFun
potMpFeelingFun NoVal      = ""
potMpFeelingFun (IntVal 0) = ""
potMpFeelingFun (IntVal _) = "" -- TODO


-----


potPpFeelingFun :: FeelingFun
potPpFeelingFun NoVal      = ""
potPpFeelingFun (IntVal 0) = ""
potPpFeelingFun (IntVal _) = "" -- TODO


-----


potFpFeelingFun :: FeelingFun
potFpFeelingFun NoVal      = ""
potFpFeelingFun (IntVal 0) = ""
potFpFeelingFun (IntVal _) = "" -- TODO


-----


potTinnitusFeelingFun :: FeelingFun
potTinnitusFeelingFun _ = "Your ears are ringing."
