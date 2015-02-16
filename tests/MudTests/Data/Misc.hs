{-# LANGUAGE OverloadedStrings #-}

module MudTests.Data.Misc where

import Mud.Data.Misc
import Mud.TopLvlDefs.Chars
import Mud.Util.Quoting

import Data.Monoid ((<>))
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


test_serializeStdDesig :: T.Text
test_serializeStdDesig = serialize StdDesig { stdPCEntSing = Just "Taro"
                                            , isCap        = False
                                            , pcEntName    = "mhuman"
                                            , pcId         = 50
                                            , pcIds        = [50..55] }


test_serializeNonStdDesig :: T.Text
test_serializeNonStdDesig = serialize NonStdDesig { nonStdPCEntSing = "Taro"
                                                  , nonStdDesc      = "A male human" }


test_deserializeStdDesig :: PCDesig
test_deserializeStdDesig =
    deserialize . quoteWith std . T.intercalate d $ [ "", "True", "fhuman", "55", "[55,54,53,52,51,50]" ]
  where
    std = T.singleton stdDesigDelimiter
    d   = T.singleton desigDelimiter


test_deserializeNonStdDesig :: PCDesig
test_deserializeNonStdDesig = deserialize . quoteWith non $ "Hanako" <> d <> "A female human"
  where
    non = T.singleton nonStdDesigDelimiter
    d   = T.singleton desigDelimiter
