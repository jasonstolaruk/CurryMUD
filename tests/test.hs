import MudTests.Data.Misc
import MudTests.Data.State.Util.Misc
import MudTests.Data.State.Util.Random
import MudTests.TheWorld.TheWorld
import MudTests.Util.List
import MudTests.Util.Misc
import MudTests.Util.Padding
import MudTests.Util.Text
import MudTests.Util.Wrapping

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck as QC (testProperty)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "tests" [ propertyTests, unitTests ]


-- ==================================================


propertyTests :: TestTree
propertyTests = testGroup "property tests" [ propTests_Mud_Data_State_Util_Misc
                                           , propTests_Mud_Data_State_Util_Random
                                           , propTests_Mud_TheWorld_TheWorld
                                           , propTests_Mud_Util_List
                                           , propTests_Mud_Util_Padding
                                           , propTests_Mud_Util_Text
                                           , propTests_Mud_Util_Wrapping ]


-- --------------------------------------------------


propTests_Mud_Data_State_Util_Misc :: TestTree
propTests_Mud_Data_State_Util_Misc = testGroup "property tests Mud.Data.State.Util.Misc"
    [ QC.testProperty "prop_getUnusedId" prop_getUnusedId ]


-- --------------------------------------------------


propTests_Mud_Data_State_Util_Random :: TestTree
propTests_Mud_Data_State_Util_Random = testGroup "property tests Mud.Data.State.Util.Random"
    [ QC.testProperty "prop_rndmIntToRange_within_range_from_zero"  prop_rndmIntToRange_within_range_from_zero
    , QC.testProperty "prop_rndmIntToRange_within_range_from_other" prop_rndmIntToRange_within_range_from_zero
    , QC.testProperty "prop_rndmIntToRange_distribution"            prop_rndmIntToRange_distribution
    , QC.testProperty "prop_rndmIntToRangeHelper_low_max"           prop_rndmIntToRangeHelper_low_max
    , QC.testProperty "prop_rndmRs_within_range"                    prop_rndmRs_within_range
    , QC.testProperty "prop_rndmRs_no_range"                        prop_rndmRs_no_range
    , QC.testProperty "prop_rndmRs_minimal_range"                   prop_rndmRs_minimal_range ]


-- --------------------------------------------------


propTests_Mud_TheWorld_TheWorld :: TestTree
propTests_Mud_TheWorld_TheWorld = testGroup "property tests Mud.TheWorld.TheWorld"
    [ QC.testProperty "prop_noDupIds" prop_noDupIds ]


-- --------------------------------------------------


propTests_Mud_Util_List :: TestTree
propTests_Mud_Util_List = testGroup "property tests Mud.Util.List"
    [ QC.testProperty "prop_countOcc"    prop_countOcc
    , QC.testProperty "prop_mkCountList" prop_mkCountList ]


-- --------------------------------------------------


propTests_Mud_Util_Padding :: TestTree
propTests_Mud_Util_Padding = testGroup "property tests Mud.Util.Padding"
    [ QC.testProperty "prop_quoteWithAndPad_length" prop_quoteWithAndPad_length
    , QC.testProperty "prop_quoteWithAndPad_quotes" prop_quoteWithAndPad_quotes
    , QC.testProperty "prop_padOrTrunc_pads"        prop_padOrTrunc_pads
    , QC.testProperty "prop_padOrTrunc_truncates"   prop_padOrTrunc_truncates ]


-- --------------------------------------------------


propTests_Mud_Util_Text :: TestTree
propTests_Mud_Util_Text = testGroup "property tests Mud.Util.Text"
    [ QC.testProperty "prop_aOrAn"                              prop_aOrAn
    , QC.testProperty "prop_findFullNameForAbbrev_findsNothing" prop_findFullNameForAbbrev_findsNothing
    , QC.testProperty "prop_findFullNameForAbbrev_findsMatch"   prop_findFullNameForAbbrev_findsMatch ]


-- --------------------------------------------------


propTests_Mud_Util_Wrapping :: TestTree
propTests_Mud_Util_Wrapping = testGroup "property tests Mud.Util.Wrapping"
    [ QC.testProperty "prop_wrap"                  prop_wrap
    , QC.testProperty "prop_wrapIndent_wraps"      prop_wrapIndent_wraps
    , QC.testProperty "prop_wrapIndent_indents"    prop_wrapIndent_indents
    , QC.testProperty "prop_xformLeading"          prop_xformLeading
    , QC.testProperty "prop_wrapLineWithIndentTag" prop_wrapLineWithIndentTag
    , QC.testProperty "prop_calcIndent"            prop_calcIndent ]


-- ==================================================


unitTests :: TestTree
unitTests = testGroup "unit tests" [ unitTests_Mud_Data_Misc
                                   , unitTests_Mud_Data_State_Util_Misc
                                   , unitTests_Mud_Util_Misc
                                   , unitTests_Mud_Util_Text ]


-- --------------------------------------------------


unitTests_Mud_Data_Misc :: TestTree
unitTests_Mud_Data_Misc = testGroup "unit tests Mud.Data.Misc"
    [ testCase "serializeStdDesig"      test_serializeStdDesig
    , testCase "serializeNonStdDesig"   test_serializeNonStdDesig
    , testCase "deserializeStdDesig"    test_deserializeStdDesig
    , testCase "deserializeNonStdDesig" test_deserializeNonStdDesig ]


-- --------------------------------------------------


unitTests_Mud_Data_State_Util_Misc :: TestTree
unitTests_Mud_Data_State_Util_Misc = testGroup "unit tests Mud.Data.State.Util.Misc"
    [ testCase "dropPrefixesForHook_no_prefixes"  test_dropPrefixesForHook_no_prefixes
    , testCase "dropPrefixesForHook_no_matches"   test_dropPrefixesForHook_no_matches
    , testCase "dropPrefixesForHook_with_matches" test_dropPrefixesForHook_with_matches
    , testCase "dropPrefixesForHook_abbrev"       test_dropPrefixesForHook_abbrev ]


-- --------------------------------------------------


unitTests_Mud_Util_Misc :: TestTree
unitTests_Mud_Util_Misc = testGroup "unit tests Mud.Util.Misc"
    [ testCase "mWhen_IO_True"    test_mWhen_IO_True
    , testCase "mWhen_IO_False"   test_mWhen_IO_False
    , testCase "mUnless_IO_True"  test_mUnless_IO_True
    , testCase "mUnless_IO_False" test_mUnless_IO_False ]


-- --------------------------------------------------


unitTests_Mud_Util_Text :: TestTree
unitTests_Mud_Util_Text = testGroup "unit tests Mud.Util.Text"
    [ testCase "stripControl"                   test_stripControl
    , testCase "stripTelnet_null"               test_stripTelnet_null
    , testCase "stripTelnet_telnetCodes"        test_stripTelnet_telnetCodes
    , testCase "stripTelnet_leading"            test_stripTelnet_leading
    , testCase "stripTelnet_trailing"           test_stripTelnet_trailing
    , testCase "stripTelnet_leadingAndTrailing" test_stripTelnet_leadingAndTrailing
    , testCase "stripTelnet_intercalated"       test_stripTelnet_intercalated
    , testCase "stripTelnet_malformed1"         test_stripTelnet_malformed1
    , testCase "stripTelnet_malformed2"         test_stripTelnet_malformed2
    , testCase "stripTelnet_malformed3"         test_stripTelnet_malformed3
    , testCase "stripTelnet_malformed4"         test_stripTelnet_malformed4
    , testCase "stripTelnet_malformed5"         test_stripTelnet_malformed5
    , testCase "stripTelnet_malformed6"         test_stripTelnet_malformed6 ]
