{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Misc.FeelingFuns (feelingFuns) where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.TheWorld.Liqs

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T


data DxOrHt = IsDx | IsHt


feelingFuns :: [(FeelingTag, FeelingFun)]
feelingFuns = [ ("sacrificeBonusAule",      sacrificeBonusFeelingFun Aule      )
              , ("sacrificeBonusCaila",     sacrificeBonusFeelingFun Caila     )
              , ("sacrificeBonusCeloriel",  sacrificeBonusFeelingFun Celoriel  )
              , ("sacrificeBonusDellio",    sacrificeBonusFeelingFun Dellio    )
              , ("sacrificeBonusDrogo",     sacrificeBonusFeelingFun Drogo     )
              , ("sacrificeBonusIminyeDx",  sacrificeBonusIminyeFeelingFun IsDx)
              , ("sacrificeBonusIminyeHt",  sacrificeBonusIminyeFeelingFun IsHt)
              , ("sacrificeBonusItulvatar", sacrificeBonusFeelingFun Itulvatar )
              , ("sacrificeBonusMuhrgorhd", sacrificeBonusFeelingFun Murgorhd  )
              , ("sacrificeBonusRhayk",     sacrificeBonusFeelingFun Rhayk     )
              , ("sacrificeBonusRumialys",  sacrificeBonusFeelingFun Rumialys  )
              , (potFpTag,                  potFpFeelingFun                    )
              , (potHpTag,                  potHpFeelingFun                    )
              , (potMpTag,                  potMpFeelingFun                    )
              , (potPpTag,                  potPpFeelingFun                    )
              , (potTinnitusTag,            potTinnitusFeelingFun              ) ]


-----


sacrificeBonusFeelingFun :: GodName -> FeelingFun
sacrificeBonusFeelingFun gn =
    const . T.concat $ [ "You have the extraordinary feeling that "
                       , pp gn
                       , " has blessed you. "
                       , case gn of Aule      -> "You're feeling confident and reassured."
                                    Caila     -> "The air is seemingly crackling with energy!"
                                    Celoriel  -> "Your brain is tingling."
                                    Dellio    -> "You're feeling whimsical."
                                    Drogo     -> "You feel slightly giddy, as though you've been enchanted."
                                    Iminye    -> "" -- Intentionally blank.
                                    Itulvatar -> "A warm senssation courses through your veins."
                                    Murgorhd  -> "It's an exhilarating sensation!"
                                    Rhayk     -> "You feel inspired. You're ready to take on the world!"
                                    Rumialys  -> "You are acutely aware of the presence of a divine energy in all things." ]


-----


sacrificeBonusIminyeFeelingFun :: DxOrHt -> FeelingFun
sacrificeBonusIminyeFeelingFun = const . \case
  IsDx -> "You are overwhelmed by the vast intricacies of the universe. Somehow you feel both important and \
          \insignificant at the same time."
  IsHt -> "" -- Intentionally blank.


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
