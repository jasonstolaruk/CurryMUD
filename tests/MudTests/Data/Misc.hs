{-# LANGUAGE OverloadedStrings #-}

module MudTests.Data.Misc where

import           Mud.Data.Misc
import           Mud.TopLvlDefs.Chars
import           Mud.Util.Quoting

import           Data.Text (Text)
import qualified Data.Text as T
import           Test.Tasty.HUnit ((@?=), Assertion)


std, non, d :: Text
std = T.singleton stdDesigDelimiter
non = T.singleton nonStdDesigDelimiter
d   = T.singleton desigDelimiter


test_serializeStdDesig :: Assertion
test_serializeStdDesig = actual @?= expected
  where
    actual   = serialize StdDesig { desigDoExpandSing = True
                                  , desigEntName      = "mhuman"
                                  , desigCap          = Don'tCap
                                  , desigId           = 50
                                  , desigIds          = [50..55] }
    expected = quoteWith std . T.intercalate d $ [ "True", "mhuman", "Don'tCap", "50", "[50,51,52,53,54,55]" ]


test_serializeNonStdDesig :: Assertion
test_serializeNonStdDesig = actual @?= expected
  where
    actual   = serialize NonStdDesig { dEntSing = "Taro"
                                     , dDesc    = "a male human"
                                     , dCap     = Don'tCap }
    expected = quoteWith non . T.intercalate d $ [ "Taro", "a male human", "Don'tCap" ]


test_deserializeStdDesig :: Assertion
test_deserializeStdDesig = actual @?= expected
  where
    actual   = deserialize . quoteWith std . T.intercalate d $ [ "False", "fhuman", "DoCap", "55", "[55,54,53,52,51,50]" ]
    expected = StdDesig False "fhuman" DoCap 55 [ 55, 54..50 ]


test_deserializeNonStdDesig :: Assertion
test_deserializeNonStdDesig = actual @?= expected
  where
    actual   = deserialize . quoteWith non . T.intercalate d $ [ "Hanako", "A female human", "DoCap" ]
    expected = NonStdDesig "Hanako" "A female human" DoCap
