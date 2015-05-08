{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module MudTests.Util.Text where

import Mud.TopLvlDefs.Chars
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text

import Data.Char (chr, isSpace)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Test.QuickCheck.Modifiers (NonEmptyList(..))
import Test.Tasty.HUnit ((@?=), Assertion)
import Test.Tasty.QuickCheck ((==>), Property)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


prop_aOrAn :: T.Text -> Property
prop_aOrAn t = (not . T.null . T.strip $ t) ==>
  let (a, b) = T.break isSpace . aOrAn $ t
  in a == ((isVowel . T.head . T.tail $ b) ? "an" :? "a")


prop_findFullNameForAbbrev_findsNothing :: NonEmptyList Char -> [T.Text] -> Property
prop_findFullNameForAbbrev_findsNothing (NonEmpty (T.pack -> needle)) hay = any (not . T.null) hay &&
                                                                            all (not . (needle `T.isInfixOf`)) hay ==>
  isNothing . findFullNameForAbbrev needle $ hay


prop_findFullNameForAbbrev_findsMatch :: NonEmptyList Char -> [T.Text] -> Property
prop_findFullNameForAbbrev_findsMatch (NonEmpty (T.pack -> needle)) hay = any (not . T.null) hay &&
                                                                          all (not . (needle `T.isInfixOf`)) hay ==>
  let nonNull = head . filter (not . T.null) $ hay
      match   = needle <> nonNull
      hay'    = match : hay
  in findFullNameForAbbrev needle hay' == Just match


-- ==================================================


telnetCodes :: T.Text
telnetCodes = T.pack . map chr $ [ 255, 252, 3, 255, 250, 201, 67, 111, 114, 101, 46, 83, 117, 112, 112, 111, 114, 116, 115, 46, 83, 101, 116, 32, 91, 93, 255, 240 ]


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
