{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module MudTests.Util.Text where

import Mud.TopLvlDefs.Telnet
import Mud.Util.List hiding (countOcc)
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text

import Data.Char (chr, isSpace)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck.Modifiers (NonEmptyList(..))
import Test.Tasty.HUnit ((@?=), Assertion)
import Test.Tasty.QuickCheck ((==>), Property)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


prop_aOrAn :: Text -> Property
prop_aOrAn t = (()!# T.strip t) ==>
    let (a, b) = T.break isSpace . aOrAn $ t
    in a == ((isVowel . T.head . T.tail $ b) ? "an" :? "a")


prop_findFullNameForAbbrev_findsNothing :: NonEmptyList Char -> [Text] -> Property
prop_findFullNameForAbbrev_findsNothing (NonEmpty (T.pack -> needle)) hay = any (()!#) hay &&
                                                                            all (not . (needle `T.isInfixOf`)) hay ==>
    (()#) . findFullNameForAbbrev needle $ hay


prop_findFullNameForAbbrev_findsMatch :: NonEmptyList Char -> [Text] -> Property
prop_findFullNameForAbbrev_findsMatch (NonEmpty (T.pack -> needle)) hay = any (()!#) hay &&
                                                                          all (not . (needle `T.isInfixOf`)) hay ==>
    let nonEmpty = head . dropEmpties $ hay
        match    = needle <> nonEmpty
        hay'     = match : hay
    in findFullNameForAbbrev needle hay' == Just match


prop_parseTelnetTTypeResponse :: NonEmptyList Char -> Bool
prop_parseTelnetTTypeResponse (NonEmpty (T.pack -> ttype)) =
    parseTelnetTTypeResponse (telnetTTypeResponseL <> ttype <> telnetTTypeResponseR) == (ttype, "")


prop_parseTelnetTTypeResponse_withL :: NonEmptyList Char -> NonEmptyList Char -> Bool
prop_parseTelnetTTypeResponse_withL (NonEmpty (T.pack -> l)) (NonEmpty (T.pack -> ttype)) =
    parseTelnetTTypeResponse (T.concat [ l
                                       , telnetTTypeResponseL
                                       , ttype
                                       , telnetTTypeResponseR ]) == (ttype, l)


prop_parseTelnetTTypeResponse_withR :: NonEmptyList Char -> NonEmptyList Char -> Bool
prop_parseTelnetTTypeResponse_withR (NonEmpty (T.pack -> ttype)) (NonEmpty (T.pack -> r)) =
    parseTelnetTTypeResponse (T.concat [ telnetTTypeResponseL
                                       , ttype
                                       , telnetTTypeResponseR
                                       , r ]) == (ttype, r)


prop_parseTelnetTTypeResponse_withLAndR :: NonEmptyList Char -> NonEmptyList Char -> NonEmptyList Char -> Bool
prop_parseTelnetTTypeResponse_withLAndR (NonEmpty (T.pack -> l)) (NonEmpty (T.pack -> ttype)) (NonEmpty (T.pack -> r)) =
    parseTelnetTTypeResponse (T.concat [ l
                                       , telnetTTypeResponseL
                                       , ttype
                                       , telnetTTypeResponseR
                                       , r ]) == (ttype, l <> r)


-- ==================================================


test_countOcc_emptyNeedle :: Assertion
test_countOcc_emptyNeedle = actual @?= expected
  where
    actual   = "" `countOcc` "abc123def123ghi123"
    expected = 0


test_countOcc_emptyHaystack :: Assertion
test_countOcc_emptyHaystack = actual @?= expected
  where
    actual   = "123" `countOcc` ""
    expected = 0


test_countOcc_zero :: Assertion
test_countOcc_zero = actual @?= expected
  where
    actual   = "123" `countOcc` "abcdefghi"
    expected = 0


test_countOcc_one :: Assertion
test_countOcc_one = actual @?= expected
  where
    actual   = "123" `countOcc` "abc123defghi"
    expected = 1


test_countOcc_two :: Assertion
test_countOcc_two = actual @?= expected
  where
    actual   = "123" `countOcc` "123def123ghi"
    expected = 2


test_countOcc_three :: Assertion
test_countOcc_three = actual @?= expected
  where
    actual   = "123" `countOcc` "abc123def123ghi123"
    expected = 3


test_stripControl :: Assertion
test_stripControl = actual @?= expected
  where
    actual       = stripControl . quoteWith controlCodes $ "test"
    expected     = "test"
    controlCodes = T.pack $ [ '\0' .. '\31' ] ++ [ '\127' .. (maxBound :: Char) ]


test_stripTelnet_null :: Assertion
test_stripTelnet_null = actual @?= expected
  where
    actual   = stripTelnet ""
    expected = ""


test_stripTelnet_telnetCodes :: Assertion
test_stripTelnet_telnetCodes = actual @?= expected
  where
    actual   = stripTelnet telnetCodes
    expected = ""


telnetCodes :: Text
telnetCodes = T.pack . map chr $ [ 255, 252, 3, 255, 250, 201, 67, 111, 114, 101, 46, 83, 117, 112, 112, 111, 114, 116, 115, 46, 83, 101, 116, 32, 91, 93, 255, 240 ]


test_stripTelnet_leading :: Assertion
test_stripTelnet_leading = actual @?= expected
  where
    actual   = stripTelnet $ telnetCodes <> "test"
    expected = "test"


test_stripTelnet_trailing :: Assertion
test_stripTelnet_trailing = actual @?= expected
  where
    actual   = stripTelnet $ "test" <> telnetCodes
    expected = "test"


test_stripTelnet_leadingAndTrailing :: Assertion
test_stripTelnet_leadingAndTrailing = actual @?= expected
  where
    actual   = stripTelnet $ quoteWith telnetCodes "test"
    expected = "test"


test_stripTelnet_intercalated :: Assertion
test_stripTelnet_intercalated = actual @?= expected
  where
    actual   = stripTelnet $ T.intercalate telnetCodes [ "test1", "test2", "test3", "test4", "test5" ]
    expected = "test1test2test3test4test5"


test_stripTelnet_malformed1 :: Assertion
test_stripTelnet_malformed1 = actual @?= expected
  where
    actual   = stripTelnet . T.singleton $ telnetIAC
    expected = ""


test_stripTelnet_malformed2 :: Assertion
test_stripTelnet_malformed2 = actual @?= expected
  where
    actual   = stripTelnet . T.pack $ [ telnetIAC, telnetSB, 'a' ]
    expected = ""


test_stripTelnet_malformed3 :: Assertion
test_stripTelnet_malformed3 = actual @?= expected
  where
    actual   = stripTelnet . T.pack $ telnetIAC : telnetSB : "test"
    expected = ""


test_stripTelnet_malformed4 :: Assertion
test_stripTelnet_malformed4 = actual @?= expected
  where
    actual   = stripTelnet $ "test" `T.snoc` telnetIAC
    expected = "test"


test_stripTelnet_malformed5 :: Assertion
test_stripTelnet_malformed5 = actual @?= expected
  where
    actual   = stripTelnet $ "test" <> T.pack [ telnetIAC, telnetSB, 'a' ]
    expected = "test"


test_stripTelnet_malformed6 :: Assertion
test_stripTelnet_malformed6 = actual @?= expected
  where
    actual   = stripTelnet $ "test" <> T.pack (telnetIAC : telnetSB : "TEST")
    expected = "test"
