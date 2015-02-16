{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

import Mud.Data.Misc
import Mud.TopLvlDefs.Chars
import Mud.Util.Quoting
import MudTests.Data.Misc
import MudTests.TheWorld.TheWorld
import MudTests.Threads
import MudTests.Util.List
import MudTests.Util.Padding
import MudTests.Util.Text
import MudTests.Util.Wrapping

import Data.Monoid ((<>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck as QC (testProperty)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "tests" [ propertyTests, unitTests ]


-- ==================================================


propertyTests :: TestTree
propertyTests = testGroup "property tests" [ propTests_Mud_TheWorld_TheWorld
                                           , propTests_Mud_Threads
                                           , propTests_Mud_Util_List
                                           , propTests_Mud_Util_Padding
                                           , propTests_Mud_Util_Text
                                           , propTests_Mud_Util_Wrapping ]


-- --------------------------------------------------


propTests_Mud_TheWorld_TheWorld :: TestTree
propTests_Mud_TheWorld_TheWorld = testGroup "property tests Mud.TheWorld.TheWorld"
    [ QC.testProperty "prop_noDupIds" prop_noDupIds ]


-- --------------------------------------------------


propTests_Mud_Threads :: TestTree
propTests_Mud_Threads = testGroup "property tests Mud.Threads"
    [ QC.testProperty "prop_getUnusedId" prop_getUnusedId ]


-- --------------------------------------------------


propTests_Mud_Util_List :: TestTree
propTests_Mud_Util_List = testGroup "property tests Mud.Util.List"
    [ QC.testProperty "prop_countOcc" prop_countOcc
    , QC.testProperty "prop_mkCountList" prop_mkCountList ]


-- --------------------------------------------------


propTests_Mud_Util_Padding :: TestTree
propTests_Mud_Util_Padding = testGroup "property tests Mud.Util.Padding"
    [ QC.testProperty "prop_quoteWithAndPad_length" prop_quoteWithAndPad_length
    , QC.testProperty "prop_quoteWithAndPad_quotes" prop_quoteWithAndPad_quotes
    , QC.testProperty "prop_padOrTrunc_pads" prop_padOrTrunc_pads
    , QC.testProperty "prop_padOrTrunc_truncates" prop_padOrTrunc_truncates ]


-- --------------------------------------------------


propTests_Mud_Util_Text :: TestTree
propTests_Mud_Util_Text = testGroup "property tests Mud.Util.Text"
    [ QC.testProperty "prop_aOrAn" prop_aOrAn
    , QC.testProperty "prop_findFullNameForAbbrev_findsNothing" prop_findFullNameForAbbrev_findsNothing
    , QC.testProperty "prop_findFullNameForAbbrev_findsMatch" prop_findFullNameForAbbrev_findsMatch ]


-- --------------------------------------------------


propTests_Mud_Util_Wrapping :: TestTree
propTests_Mud_Util_Wrapping = testGroup "property tests Mud.Util.Wrapping"
    [ QC.testProperty "prop_wrap" prop_wrap
    , QC.testProperty "prop_wrapIndent_wraps" prop_wrapIndent_wraps
    , QC.testProperty "prop_wrapIndent_indents" prop_wrapIndent_indents
    , QC.testProperty "prop_xformLeading" prop_xformLeading
    , QC.testProperty "prop_wrapLineWithIndentTag" prop_wrapLineWithIndentTag
    , QC.testProperty "prop_calcIndent" prop_calcIndent ]


-- ==================================================


unitTests :: TestTree
unitTests = testGroup "unit tests" [ unitTests_Mud_Data_Misc
                                   , unitTests_Mud_Util_Text ]


-- --------------------------------------------------


unitTests_Mud_Data_Misc :: TestTree
unitTests_Mud_Data_Misc = testGroup "unit tests Mud.Data.Misc"
    [ testCase "serializeStdDesig"      $ test_serializeStdDesig      @?=
        quoteWith std (T.intercalate d [ "Taro", "False", "mhuman", "50", "[50,51,52,53,54,55]" ])
    , testCase "serializeNonStdDesig"   $ test_serializeNonStdDesig   @?=
        quoteWith non ("Taro" <> d <> "A male human")
    , testCase "deserializeStdDesig"    $ test_deserializeStdDesig    @?=
        StdDesig Nothing True "fhuman" 55 [ 55, 54..50 ]
    , testCase "deserializeNonStdDesig" $ test_deserializeNonStdDesig @?=
        NonStdDesig "Hanako" "A female human" ]
  where
    std = T.singleton stdDesigDelimiter
    non = T.singleton nonStdDesigDelimiter
    d   = T.singleton desigDelimiter


-- --------------------------------------------------


unitTests_Mud_Util_Text :: TestTree
unitTests_Mud_Util_Text = testGroup "unit tests Mud.Util.Text"
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
