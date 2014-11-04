{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module MudTests.MiscDataTypesTests where

import Mud.MiscDataTypes
import Mud.TopLvlDefs

import Data.Monoid ((<>))
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


test_serializeStdDesig :: T.Text
test_serializeStdDesig = serialize StdDesig { stdPCEntSing = Just "Taro"
                                            , isCap        = False
                                            , pcEntName    = "mhuman"
                                            , pcId         = 50 }


test_serializeNonStdDesig :: T.Text
test_serializeNonStdDesig = serialize NonStdDesig { nonStdPCEntSing = "Taro"
                                                  , nonStdDesc      = "A male human" }

test_deserializeStdDesig :: PCDesig
test_deserializeStdDesig = deserialize $ std <> T.intercalate d [ "", "True", "fhuman", "55" ] <> std
  where
    std = T.pack [stdDesigDelimiter]
    d   = T.pack [desigDelimiter]


test_deserializeNonStdDesig :: PCDesig
test_deserializeNonStdDesig = deserialize . T.concat $ [ non, "Hanako", d, "A female human", non ]
  where
    non = T.pack [nonStdDesigDelimiter]
    d   = T.pack [desigDelimiter]
