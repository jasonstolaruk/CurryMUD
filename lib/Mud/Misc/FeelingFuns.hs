{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.FeelingFuns (feelingFuns) where

import Mud.Data.State.MudData
import Mud.TheWorld.Liqs

import Data.Text (Text)
import Data.Monoid ((<>))


feelingFuns :: [(FeelingTag, FeelingFun)]
feelingFuns = [ (potHpTag,       potHpFeelingFun      )
              , (potMpTag,       potMpFeelingFun      )
              , (potPpTag,       potPpFeelingFun      )
              , (potFpTag,       potFpFeelingFun      )
              , (potTinnitusTag, potTinnitusFeelingFun) ]


-----


potHpFeelingFun :: FeelingFun
potHpFeelingFun FeelingNoVal        = ""
potHpFeelingFun (FeelingFixedVal 0) = ""
potHpFeelingFun (FeelingFixedVal _) = mkFeelingMsg "wounds heal"


mkFeelingMsg :: Text -> Text
mkFeelingMsg txt = "You feel your " <> txt <> " as a warm sensation pulsates outward from your stomach and throughout \
                   \your torso."


-----


potMpFeelingFun :: FeelingFun
potMpFeelingFun FeelingNoVal        = ""
potMpFeelingFun (FeelingFixedVal 0) = ""
potMpFeelingFun (FeelingFixedVal _) = mkFeelingMsg "mana return"


-----


potPpFeelingFun :: FeelingFun
potPpFeelingFun FeelingNoVal        = ""
potPpFeelingFun (FeelingFixedVal 0) = ""
potPpFeelingFun (FeelingFixedVal _) = mkFeelingMsg "psionic energy return"


-----


potFpFeelingFun :: FeelingFun
potFpFeelingFun FeelingNoVal        = ""
potFpFeelingFun (FeelingFixedVal 0) = ""
potFpFeelingFun (FeelingFixedVal _) = mkFeelingMsg "stamina return"


-----


potTinnitusFeelingFun :: FeelingFun
potTinnitusFeelingFun = const "Your ears are ringing."
