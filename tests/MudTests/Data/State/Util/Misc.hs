{-# LANGUAGE OverloadedStrings #-}

module MudTests.Data.State.Util.Misc where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Misc
import           Mud.TheWorld.Zones.AdminZone
import           Mud.TopLvlDefs.Chars
import           MudTests.TestUtil

import           Control.Lens (views)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.IntMap.Strict as IM (notMember)
import qualified Data.Text as T
import           Test.QuickCheck.Monadic (assert, monadicIO)
import           Test.Tasty.HUnit ((@?=), Assertion)
import           Test.Tasty.QuickCheck (Property)

prop_getUnusedId :: Property
prop_getUnusedId = monadicIO $ inWorld getState >>= \ms -> assert . views typeTbl (getUnusedId ms `IM.notMember`) $ ms

-- ==================================================

test_dropPrefixesForHooks_noPrefixes :: Assertion
test_dropPrefixesForHooks_noPrefixes = actual @?= expected
  where
    args     = [ "abc", "def", "ghi" ]
    actual   = dropPrefixesForHooks [ getFlowerHook, lookFlowerbedHook ] args
    expected = args

test_dropPrefixesForHooks_noMatches :: Assertion
test_dropPrefixesForHooks_noMatches = actual @?= expected
  where
    args     = [ attachPrefix ""   allChar    "abc"
               , attachPrefix "5"  amountChar "def"
               , attachPrefix "10" indexChar  "ghi" ]
    actual   = dropPrefixesForHooks [ getFlowerHook, lookFlowerbedHook ] args
    expected = args

attachPrefix :: Text -> Char -> Text -> Text
attachPrefix digits c t = digits <> (c `T.cons` t)

test_dropPrefixesForHooks_withMatches :: Assertion
test_dropPrefixesForHooks_withMatches = actual @?= expected
  where
    args     = [ attachPrefix ""   allChar    "abc"
               , attachPrefix ""   allChar    "flower"
               , attachPrefix "5"  amountChar "def"
               , attachPrefix "5"  amountChar "flowers"
               , attachPrefix "10" indexChar  "ghi"
               , attachPrefix "10" indexChar  "flowerbed" ]
    actual   = dropPrefixesForHooks [ getFlowerHook, lookFlowerbedHook ] args
    expected = [ attachPrefix ""   allChar    "abc"
               , "flower"
               , attachPrefix "5"  amountChar "def"
               , "flowers"
               , attachPrefix "10" indexChar  "ghi"
               , "flowerbed" ]

test_dropPrefixesForHooks_abbrev :: Assertion
test_dropPrefixesForHooks_abbrev = actual @?= expected
  where
    args     = [ attachPrefix "" allChar "flowe"
               , attachPrefix "" allChar "flower" ]
    actual   = dropPrefixesForHooks [ getFlowerHook, lookFlowerbedHook ] args
    expected = [ attachPrefix "" allChar "flowe", "flower" ]

test_procQuoteChars_null :: Assertion
test_procQuoteChars_null = actual @?= expected
  where
    actual   = procQuoteChars []
    expected = Just []

test_procQuoteChars_zero :: Assertion
test_procQuoteChars_zero = actual @?= expected
  where
    actual   = procQuoteChars [ "abc", "123", "def", "123", "ghi", "123" ]
    expected = Just [ "abc", "123", "def", "123", "ghi", "123" ]

test_procQuoteChars_one :: Assertion
test_procQuoteChars_one = actual @?= expected
  where
    actual   = procQuoteChars [ "abc", "123", "de" <> q <> "f", "123", "ghi", "123" ]
    expected = Nothing

q :: Text
q = T.singleton quoteChar

test_procQuoteChars_two :: Assertion
test_procQuoteChars_two = actual @?= expected
  where
    actual   = procQuoteChars [ "abc", "123", "de" <> q <> "f", "1" <> q <> "23", "ghi", "123" ]
    expected = Just [ "abc", "123", "def 123", "ghi", "123" ]

test_procQuoteChars_three :: Assertion
test_procQuoteChars_three = actual @?= expected
  where
    actual   = procQuoteChars [ "abc", "123", "de" <> q <> "f", "1" <> q <> "23", "gh" <> q <> "i", "123" ]
    expected = Nothing

test_procQuoteChars_four :: Assertion
test_procQuoteChars_four = actual @?= expected
  where
    actual   = procQuoteChars [ "abc", "123", "de" <> q <> "f", "1" <> q <> "23", "gh" <> q <> "i", "1" <> q <> "23" ]
    expected = Just [ "abc", "123", "def 123", "ghi 123" ]
