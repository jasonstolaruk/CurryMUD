{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.FeelingFuns (feelingFuns) where

import Mud.Data.State.MudData

import Data.Text (Text)
import Data.Monoid ((<>))


feelingFuns :: [(FeelingTag, FeelingFun)]
feelingFuns = [ ("potHp",       potHpFeelingFun      )
              , ("potMp",       potMpFeelingFun      )
              , ("potPp",       potPpFeelingFun      )
              , ("potFp",       potFpFeelingFun      )
              , ("potTinnitus", potTinnitusFeelingFun) ]


-----


potHpFeelingFun :: FeelingFun
potHpFeelingFun NoVal      = ""
potHpFeelingFun (IntVal 0) = ""
potHpFeelingFun (IntVal _) = mkFeelingMsg "wounds heal"


mkFeelingMsg :: Text -> Text
mkFeelingMsg txt = "You feel your " <> txt <> " as a warm sensation pulsates outward from your stomach and throughout \
                   \your torso."


-----


potMpFeelingFun :: FeelingFun
potMpFeelingFun NoVal      = ""
potMpFeelingFun (IntVal 0) = ""
potMpFeelingFun (IntVal _) = mkFeelingMsg "mana return"


-----


potPpFeelingFun :: FeelingFun
potPpFeelingFun NoVal      = ""
potPpFeelingFun (IntVal 0) = ""
potPpFeelingFun (IntVal _) = mkFeelingMsg "psionic energy return"


-----


potFpFeelingFun :: FeelingFun
potFpFeelingFun NoVal      = ""
potFpFeelingFun (IntVal 0) = ""
potFpFeelingFun (IntVal _) = mkFeelingMsg "stamina return"


-----


potTinnitusFeelingFun :: FeelingFun
potTinnitusFeelingFun = const "Your ears are ringing."
