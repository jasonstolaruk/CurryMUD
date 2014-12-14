{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

import Mud.MiscDataTypes
import Mud.TopLvlDefs
import Mud.Util
import MudTests.MiscDataTypesTests
import MudTests.StateHelpersTests
import MudTests.TheWorldTests
import MudTests.UtilTests

import Data.Monoid ((<>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck as QC (testProperty)
import qualified Data.Text as T


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [ propertyTests, unitTests ]

-- ==================================================

propertyTests :: TestTree
propertyTests = testGroup "property tests" [ propTestsStateHelpers, propTestsTheWorld, propTestsUtil ]

-- --------------------------------------------------

propTestsStateHelpers :: TestTree
propTestsStateHelpers = testGroup "property tests StateHelpers" [ QC.testProperty "prop_getUnusedId" prop_getUnusedId ]

-- --------------------------------------------------

propTestsTheWorld :: TestTree
propTestsTheWorld = testGroup "property tests TheWorld" [ QC.testProperty "prop_noDupIds" prop_noDupIds ]

-- --------------------------------------------------

propTestsUtil :: TestTree
propTestsUtil = testGroup "property tests Util"
    [ QC.testProperty "prop_wordWrap" prop_wordWrap
    , QC.testProperty "prop_wordWrapIndent_wraps" prop_wordWrapIndent_wraps
    , QC.testProperty "prop_wordWrapIndent_indents" prop_wordWrapIndent_indents
    , QC.testProperty "prop_xformLeading" prop_xformLeading
    , QC.testProperty "prop_wrapLineWithIndentTag" prop_wrapLineWithIndentTag
    , QC.testProperty "prop_calcIndent" prop_calcIndent
    , QC.testProperty "prop_aOrAn" prop_aOrAn
    , QC.testProperty "prop_quoteWithAndPad_length" prop_quoteWithAndPad_length
    , QC.testProperty "prop_quoteWithAndPad_quotes" prop_quoteWithAndPad_quotes
    , QC.testProperty "prop_padOrTrunc_pads" prop_padOrTrunc_pads
    , QC.testProperty "prop_padOrTrunc_truncates" prop_padOrTrunc_truncates
    , QC.testProperty "prop_findFullNameForAbbrev_findsNothing" prop_findFullNameForAbbrev_findsNothing
    , QC.testProperty "prop_findFullNameForAbbrev_findsMatch" prop_findFullNameForAbbrev_findsMatch
    , QC.testProperty "prop_countOcc" prop_countOcc
    , QC.testProperty "prop_deleteFirstOfEach" prop_deleteFirstOfEach
    , QC.testProperty "prop_mkCountList" prop_mkCountList ]

-- ==================================================

unitTests :: TestTree
unitTests = testGroup "unit tests" [ unitTestsMiscDataTypes, unitTestsUtil ]

-- --------------------------------------------------

unitTestsUtil :: TestTree
unitTestsUtil = testGroup "unit tests Util"
    [ testCase "stripControl"                   $ test_stripControl                   @?= "test"
    , testCase "stripTelnet_null"               $ test_stripTelnet_null               @?= ""
    , testCase "stripTelnet_telnetCodes"        $ test_stripTelnet_telnetCodes        @?= ""
    , testCase "stripTelnet_leading"            $ test_stripTelnet_leading            @?= "test"
    , testCase "stripTelnet_trailing"           $ test_stripTelnet_trailing           @?= "test"
    , testCase "stripTelnet_leadingAndTrailing" $ test_stripTelnet_leadingAndTrailing @?= "test"
    , testCase "stripTelnet_intercalated"       $ test_stripTelnet_intercalated       @?= "test1test2test3test4test5"
    , testCase "stripTelnet_malformed1"         $ test_stripTelnet_malformed1         @?= ""
    , testCase "stripTelnet_malformed2"         $ test_stripTelnet_malformed2         @?= ""
    , testCase "stripTelnet_malformed3"         $ test_stripTelnet_malformed3         @?= ""
    , testCase "stripTelnet_malformed4"         $ test_stripTelnet_malformed4         @?= "test"
    , testCase "stripTelnet_malformed5"         $ test_stripTelnet_malformed5         @?= "test"
    , testCase "stripTelnet_malformed6"         $ test_stripTelnet_malformed6         @?= "test" ]

-- --------------------------------------------------

unitTestsMiscDataTypes :: TestTree
unitTestsMiscDataTypes = testGroup "unit tests MiscDataTypes"
    [ testCase "serializeStdDesig"      $ test_serializeStdDesig      @?=
        quoteWith std (T.intercalate d [ "Taro", "False", "mhuman", "50", "[50,51,52,53,54,55]" ])
    , testCase "serializeNonStdDesig"   $ test_serializeNonStdDesig   @?=
        quoteWith non ("Taro" <> d <> "A male human")
    , testCase "deserializeStdDesig"    $ test_deserializeStdDesig    @?=
        StdDesig Nothing True "fhuman" 55 [55,54..50]
    , testCase "deserializeNonStdDesig" $ test_deserializeNonStdDesig @?=
        NonStdDesig "Hanako" "A female human" ]
  where
    std = T.singleton stdDesigDelimiter
    non = T.singleton nonStdDesigDelimiter
    d   = T.singleton desigDelimiter
