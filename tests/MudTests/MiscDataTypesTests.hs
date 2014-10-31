{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module MudTests.MiscDataTypesTests where

import Mud.MiscDataTypes
import Mud.TopLvlDefs
import MudTests.TestHelpers

import Data.Monoid ((<>))
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


test_serializePCIdentifierNothing :: T.Text
test_serializePCIdentifierNothing = serialize PCIdentifier { pcEntSing        = Nothing
                                                           , isCap            = False
                                                           , nonStdIdentifier = Nothing
                                                           , pcEntName        = Nothing
                                                           , pcId             = Nothing }


test_serializePCIdentifierJust :: T.Text
test_serializePCIdentifierJust = serialize PCIdentifier { pcEntSing        = Just "Taro"
                                                        , isCap            = True
                                                        , nonStdIdentifier = Just "A male human"
                                                        , pcEntName        = Just "mhuman"
                                                        , pcId             = Just 50 }


test_deserializePCIdentifierNothing :: PCIdentifier
test_deserializePCIdentifierNothing = deserialize $ pids 2 <> "False" <> pids 4


test_deserializePCIdentifierJust :: PCIdentifier
test_deserializePCIdentifierJust = let pid = pcIdentifierDelimiter
                                   in deserialize $ pid <> T.intercalate pid [ "Taro", "True", "A male human", "mhuman", "50" ] <> pid
