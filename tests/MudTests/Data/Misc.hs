{-# LANGUAGE OverloadedStrings #-}

module MudTests.Data.Misc where

import Mud.Data.Misc
import Mud.TopLvlDefs.Chars
import Mud.Util.Quoting

import Data.Monoid ((<>))
import Test.Tasty.HUnit ((@?=), Assertion)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


std, non, d :: T.Text
std = T.singleton stdDesigDelimiter
non = T.singleton nonStdDesigDelimiter
d   = T.singleton desigDelimiter


test_serializeStdDesig :: Assertion
test_serializeStdDesig = actual @?= expected
  where
    actual   = serialize StdDesig { stdPCEntSing = Just "Taro"
                                  , shouldCap    = Don'tCap
                                  , pcEntName    = "mhuman"
                                  , pcId         = 50
                                  , pcIds        = [50..55] }
    expected = quoteWith std . T.intercalate d $ [ "Taro", "Don'tCap", "mhuman", "50", "[50,51,52,53,54,55]" ]


test_serializeNonStdDesig :: Assertion
test_serializeNonStdDesig = actual @?= expected
  where
    actual   = serialize NonStdDesig { nonStdPCEntSing = "Taro"
                                     , nonStdDesc      = "A male human" }
    expected = quoteWith non $ "Taro" <> d <> "A male human"


test_deserializeStdDesig :: Assertion
test_deserializeStdDesig = actual @?= expected
  where
    actual   = deserialize . quoteWith std . T.intercalate d $ [ "", "DoCap", "fhuman", "55", "[55,54,53,52,51,50]" ]
    expected = StdDesig Nothing DoCap "fhuman" 55 [ 55, 54..50 ]


test_deserializeNonStdDesig :: Assertion
test_deserializeNonStdDesig = actual @?= expected
  where
    actual   = deserialize . quoteWith non $ "Hanako" <> d <> "A female human"
    expected = NonStdDesig "Hanako" "A female human"
