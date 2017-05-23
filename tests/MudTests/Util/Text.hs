{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module MudTests.Util.Text where

import           Mud.Util.List hiding (countOcc)
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Data.Char (isSpace)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.QuickCheck.Modifiers (NonEmptyList(..))
import           Test.Tasty.HUnit ((@?=), Assertion)
import           Test.Tasty.QuickCheck ((==>), Property)


prop_aOrAn :: Text -> Property
prop_aOrAn t = (()!# T.strip t) ==>
    let (a, b) = T.break isSpace . aOrAn $ t
    in a == ((isVowel . T.head . T.tail $ b) ? "an" :? "a")


prop_findFullNameForAbbrev_findsNothing :: NonEmptyList Char -> [Text] -> Property
prop_findFullNameForAbbrev_findsNothing (NonEmpty (T.pack -> needle)) hay = condition needle hay ==>
    (()#) . findFullNameForAbbrev needle $ hay


condition :: Text -> [Text] -> Bool
condition needle = (&&) <$> f <*> g
  where
    f = any (()!#)
    g = all (not . (needle `T.isInfixOf`))


prop_findFullNameForAbbrev_findsMatch :: NonEmptyList Char -> [Text] -> Property
prop_findFullNameForAbbrev_findsMatch (NonEmpty (T.pack -> needle)) hay = condition needle hay ==>
    let nonEmpty = head . dropEmpties $ hay
        match    = needle <> nonEmpty
        hay'     = match : hay
    in findFullNameForAbbrev needle hay' == Just match


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
