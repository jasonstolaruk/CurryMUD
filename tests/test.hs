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
propTests_Mud_Data_State_Util_Misc = testGroup "Mud.Data.State.Util.Misc"
    [ QC.testProperty "prop_getUnusedId" prop_getUnusedId ]


-- --------------------------------------------------


propTests_Mud_Data_State_Util_Random :: TestTree
propTests_Mud_Data_State_Util_Random = testGroup "Mud.Data.State.Util.Random"
    [ QC.testProperty "prop_dropRndmElems"                          prop_dropRndmElems
    , QC.testProperty "prop_rndmIntToRange_within_range_from_zero"  prop_rndmIntToRange_within_range_from_zero
    , QC.testProperty "prop_rndmIntToRange_within_range_from_other" prop_rndmIntToRange_within_range_from_zero
    , QC.testProperty "prop_rndmIntToRange_distribution"            prop_rndmIntToRange_distribution
    , QC.testProperty "prop_rndmIntToRangeHelper_low_max"           prop_rndmIntToRangeHelper_low_max
    , QC.testProperty "prop_rndmRs_within_range"                    prop_rndmRs_within_range
    , QC.testProperty "prop_rndmRs_no_range"                        prop_rndmRs_no_range
    , QC.testProperty "prop_rndmRs_minimal_range"                   prop_rndmRs_minimal_range ]


-- --------------------------------------------------


propTests_Mud_TheWorld_TheWorld :: TestTree
propTests_Mud_TheWorld_TheWorld = testGroup "Mud.TheWorld.TheWorld"
    [ QC.testProperty "prop_noDupIds" prop_noDupIds ]


-- --------------------------------------------------


propTests_Mud_Util_List :: TestTree
propTests_Mud_Util_List = testGroup "Mud.Util.List"
    [ QC.testProperty "prop_dropElem"    prop_dropElem
    , QC.testProperty "prop_countOcc"    prop_countOcc
    , QC.testProperty "prop_mkCountList" prop_mkCountList ]


-- --------------------------------------------------


propTests_Mud_Util_Padding :: TestTree
propTests_Mud_Util_Padding = testGroup "Mud.Util.Padding"
    [ QC.testProperty "prop_quoteWithAndPad_length" prop_quoteWithAndPad_length
    , QC.testProperty "prop_quoteWithAndPad_quotes" prop_quoteWithAndPad_quotes
    , QC.testProperty "prop_padOrTrunc_pads"        prop_padOrTrunc_pads
    , QC.testProperty "prop_padOrTrunc_truncates"   prop_padOrTrunc_truncates ]


-- --------------------------------------------------


propTests_Mud_Util_Text :: TestTree
propTests_Mud_Util_Text = testGroup "Mud.Util.Text"
    [ QC.testProperty "prop_aOrAn"                              prop_aOrAn
    , QC.testProperty "prop_findFullNameForAbbrev_findsNothing" prop_findFullNameForAbbrev_findsNothing
    , QC.testProperty "prop_findFullNameForAbbrev_findsMatch"   prop_findFullNameForAbbrev_findsMatch ]


-- --------------------------------------------------


propTests_Mud_Util_Wrapping :: TestTree
propTests_Mud_Util_Wrapping = testGroup "Mud.Util.Wrapping"
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
unitTests_Mud_Data_Misc = testGroup "Mud.Data.Misc"
    [ testCase "test_serializeStdDesig"      test_serializeStdDesig
    , testCase "test_serializeNonStdDesig"   test_serializeNonStdDesig
    , testCase "test_deserializeStdDesig"    test_deserializeStdDesig
    , testCase "test_deserializeNonStdDesig" test_deserializeNonStdDesig ]


-- --------------------------------------------------


unitTests_Mud_Data_State_Util_Misc :: TestTree
unitTests_Mud_Data_State_Util_Misc = testGroup "Mud.Data.State.Util.Misc"
    [ testCase "test_dropPrefixesForHook_no_prefixes"  test_dropPrefixesForHooks_no_prefixes
    , testCase "test_dropPrefixesForHook_no_matches"   test_dropPrefixesForHooks_no_matches
    , testCase "test_dropPrefixesForHook_with_matches" test_dropPrefixesForHooks_with_matches
    , testCase "test_dropPrefixesForHook_abbrev"       test_dropPrefixesForHooks_abbrev
    , testCase "test_procQuoteChars_null"         test_procQuoteChars_null
    , testCase "test_procQuoteChars_zero"         test_procQuoteChars_zero
    , testCase "test_procQuoteChars_one"          test_procQuoteChars_one
    , testCase "test_procQuoteChars_two"          test_procQuoteChars_two
    , testCase "test_procQuoteChars_three"        test_procQuoteChars_three
    , testCase "test_procQuoteChars_four"         test_procQuoteChars_four ]


-- --------------------------------------------------


unitTests_Mud_Util_Misc :: TestTree
unitTests_Mud_Util_Misc = testGroup "Mud.Util.Misc"
    [ testCase "test_division_compare_results" test_division_compare_results
    , testCase "test_mWhen_IO_True"            test_mWhen_IO_True
    , testCase "test_mWhen_IO_False"           test_mWhen_IO_False
    , testCase "test_mUnless_IO_True"          test_mUnless_IO_True
    , testCase "test_mUnless_IO_False"         test_mUnless_IO_False ]


-- --------------------------------------------------


unitTests_Mud_Util_Text :: TestTree
unitTests_Mud_Util_Text = testGroup "Mud.Util.Text"
    [ testCase "test_countOcc_emptyNeedle"           test_countOcc_emptyNeedle
    , testCase "test_countOcc_emptyHaystack"         test_countOcc_emptyHaystack
    , testCase "test_countOcc_zero"                  test_countOcc_zero
    , testCase "test_countOcc_one"                   test_countOcc_one
    , testCase "test_countOcc_two"                   test_countOcc_two
    , testCase "test_countOcc_three"                 test_countOcc_three
    , testCase "test_stripControl"                   test_stripControl
    , testCase "test_stripTelnet_null"               test_stripTelnet_null
    , testCase "test_stripTelnet_telnetCodes"        test_stripTelnet_telnetCodes
    , testCase "test_stripTelnet_leading"            test_stripTelnet_leading
    , testCase "test_stripTelnet_trailing"           test_stripTelnet_trailing
    , testCase "test_stripTelnet_leadingAndTrailing" test_stripTelnet_leadingAndTrailing
    , testCase "test_stripTelnet_intercalated"       test_stripTelnet_intercalated
    , testCase "test_stripTelnet_malformed1"         test_stripTelnet_malformed1
    , testCase "test_stripTelnet_malformed2"         test_stripTelnet_malformed2
    , testCase "test_stripTelnet_malformed3"         test_stripTelnet_malformed3
    , testCase "test_stripTelnet_malformed4"         test_stripTelnet_malformed4
    , testCase "test_stripTelnet_malformed5"         test_stripTelnet_malformed5
    , testCase "test_stripTelnet_malformed6"         test_stripTelnet_malformed6 ]
