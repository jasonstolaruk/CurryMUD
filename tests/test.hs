import MudTests.Data.Misc
import MudTests.Data.State.Util.Misc
import MudTests.Data.State.Util.Random
import MudTests.Misc.CurryTime
import MudTests.TheWorld.TheWorld
import MudTests.Util.List
import MudTests.Util.Misc
import MudTests.Util.Padding
import MudTests.Util.Telnet
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
    [ QC.testProperty "prop_dropRndmElems"                       prop_dropRndmElems
    , QC.testProperty "prop_rndmIntToRange_withinRangeFromZero"  prop_rndmIntToRange_withinRangeFromZero
    , QC.testProperty "prop_rndmIntToRange_withinRangeFromOther" prop_rndmIntToRange_withinRangeFromOther
    , QC.testProperty "prop_rndmIntToRange_distribution"         prop_rndmIntToRange_distribution
    , QC.testProperty "prop_rndmIntToRangeHelper_lowMax"         prop_rndmIntToRangeHelper_lowMax
    , QC.testProperty "prop_rndmRs_withinRange"                  prop_rndmRs_withinRange
    , QC.testProperty "prop_rndmRs_noRange"                      prop_rndmRs_noRange
    , QC.testProperty "prop_rndmRs_minimalRange"                 prop_rndmRs_minimalRange ]


-- --------------------------------------------------


propTests_Mud_TheWorld_TheWorld :: TestTree
propTests_Mud_TheWorld_TheWorld = testGroup "Mud.TheWorld.TheWorld"
    [ QC.testProperty "prop_noDupIds" prop_noDupIds ]


-- --------------------------------------------------


propTests_Mud_Util_List :: TestTree
propTests_Mud_Util_List = testGroup "Mud.Util.List"
    [ QC.testProperty "prop_dropElemAt"  prop_dropElemAt
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
                                   , unitTests_Mud_Misc_CurryTime
                                   , unitTests_Mud_Util_Misc
                                   , unitTests_Mud_Util_Telnet
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
    [ testCase "test_dropPrefixesForHook_noPrefixes"  test_dropPrefixesForHooks_noPrefixes
    , testCase "test_dropPrefixesForHook_noMatches"   test_dropPrefixesForHooks_noMatches
    , testCase "test_dropPrefixesForHook_withMatches" test_dropPrefixesForHooks_withMatches
    , testCase "test_dropPrefixesForHook_abbrev"      test_dropPrefixesForHooks_abbrev
    , testCase "test_procQuoteChars_null"             test_procQuoteChars_null
    , testCase "test_procQuoteChars_zero"             test_procQuoteChars_zero
    , testCase "test_procQuoteChars_one"              test_procQuoteChars_one
    , testCase "test_procQuoteChars_two"              test_procQuoteChars_two
    , testCase "test_procQuoteChars_three"            test_procQuoteChars_three
    , testCase "test_procQuoteChars_four"             test_procQuoteChars_four ]


-- --------------------------------------------------


unitTests_Mud_Misc_CurryTime :: TestTree
unitTests_Mud_Misc_CurryTime = testGroup "Mud.Misc.CurryTime"
    [ testCase "test_secsToCurryTime_zeroSecs"                      test_secsToCurryTime_zeroSecs
    , testCase "test_secsToCurryTime_oneSec"                        test_secsToCurryTime_oneSec
    , testCase "test_secsToCurryTime_oneMin"                        test_secsToCurryTime_oneMin
    , testCase "test_secsToCurryTime_oneHour"                       test_secsToCurryTime_oneHour
    , testCase "test_secsToCurryTime_oneDay"                        test_secsToCurryTime_oneDay
    , testCase "test_secsToCurryTime_oneWeek"                       test_secsToCurryTime_oneWeek
    , testCase "test_secsToCurryTime_oneMonth"                      test_secsToCurryTime_oneMonth
    , testCase "test_secsToCurryTime_oneYear"                       test_secsToCurryTime_oneYear
    , testCase "test_secsToCurryTime_twoSecs"                       test_secsToCurryTime_twoSecs
    , testCase "test_secsToCurryTime_twoMins"                       test_secsToCurryTime_twoMins
    , testCase "test_secsToCurryTime_twoHours"                      test_secsToCurryTime_twoHours
    , testCase "test_secsToCurryTime_twoDays"                       test_secsToCurryTime_twoDays
    , testCase "test_secsToCurryTime_twoWeeks"                      test_secsToCurryTime_twoWeeks
    , testCase "test_secsToCurryTime_twoMonths"                     test_secsToCurryTime_twoMonths
    , testCase "test_secsToCurryTime_twoYears"                      test_secsToCurryTime_twoYears
    , testCase "test_secsToCurryTime_oneMinOneSec"                  test_secsToCurryTime_oneMinOneSec
    , testCase "test_secsToCurryTime_oneHourOneSec"                 test_secsToCurryTime_oneHourOneSec
    , testCase "test_secsToCurryTime_oneDayOneSec"                  test_secsToCurryTime_oneDayOneSec
    , testCase "test_secsToCurryTime_oneWeekOneSec"                 test_secsToCurryTime_oneWeekOneSec
    , testCase "test_secsToCurryTime_oneMonthOneSec"                test_secsToCurryTime_oneMonthOneSec
    , testCase "test_secsToCurryTime_oneYearOneSec"                 test_secsToCurryTime_oneYearOneSec
    , testCase "test_secsToCurryTime_sevenDays"                     test_secsToCurryTime_sevenDays
    , testCase "test_secsToCurryTime_eightDays"                     test_secsToCurryTime_eightDays
    , testCase "test_secsToCurryTime_fourteenDays"                  test_secsToCurryTime_fourteenDays
    , testCase "test_secsToCurryTime_fifteenDays"                   test_secsToCurryTime_fifteenDays
    , testCase "test_secsToCurryTime_twentyOneDays"                 test_secsToCurryTime_twentyOneDays
    , testCase "test_secsToCurryTime_twentyTwoDays"                 test_secsToCurryTime_twentyTwoDays
    , testCase "test_secsToCurryTime_oneSecLessThanAYear"           test_secsToCurryTime_oneSecLessThanAYear
    , testCase "test_secsToCurryTime_oneSecLessThanAYearPlusOne"    test_secsToCurryTime_oneSecLessThanAYearPlusOne
    , testCase "test_secsToCurryTime_oneSecLessThanAYearPlusTwo"    test_secsToCurryTime_oneSecLessThanAYearPlusTwo
    , testCase "test_secsToCurryTime_oneSecLessThanTwoYears"        test_secsToCurryTime_oneSecLessThanTwoYears
    , testCase "test_secsToCurryTime_oneSecLessThanTwoYearsPlusOne" test_secsToCurryTime_oneSecLessThanTwoYearsPlusOne
    , testCase "test_secsToCurryTime_oneSecLessThanTwoYearsPlusTwo" test_secsToCurryTime_oneSecLessThanTwoYearsPlusTwo ]


-- --------------------------------------------------


unitTests_Mud_Util_Misc :: TestTree
unitTests_Mud_Util_Misc = testGroup "Mud.Util.Misc"
    [ testCase "test_division_compareResults" test_division_compareResults
    , testCase "test_mWhen_IOTrue"            test_mWhen_IOTrue
    , testCase "test_mWhen_IOFalse"           test_mWhen_IOFalse
    , testCase "test_mUnless_IOTrue"          test_mUnless_IOTrue
    , testCase "test_mUnless_IOFalse"         test_mUnless_IOFalse ]


-- --------------------------------------------------


unitTests_Mud_Util_Telnet :: TestTree
unitTests_Mud_Util_Telnet = testGroup "Mud.Util.Telnet"
    [ testCase "test_parseTelnet_null"               test_parseTelnet_null
    , testCase "test_parseTelnet_noTelnet"           test_parseTelnet_noTelnet
    , testCase "test_parseTelnet_telnetTxt"          test_parseTelnet_telnetTxt
    , testCase "test_parseTelnet_leading"            test_parseTelnet_leading
    , testCase "test_parseTelnet_trailing"           test_parseTelnet_trailing
    , testCase "test_parseTelnet_leadingAndTrailing" test_parseTelnet_leadingAndTrailing
    , testCase "test_parseTelnet_intercalated"       test_parseTelnet_intercalated
    , testCase "test_parseTelnet_escapedIAC"         test_parseTelnet_escapedIAC
    , testCase "test_parseTelnet_malformed1"         test_parseTelnet_malformed1
    , testCase "test_parseTelnet_malformed2"         test_parseTelnet_malformed2
    , testCase "test_parseTelnet_malformed3"         test_parseTelnet_malformed3
    , testCase "test_parseTelnet_malformed4"         test_parseTelnet_malformed4
    , testCase "test_parseTelnet_malformed5"         test_parseTelnet_malformed5
    , testCase "test_parseTelnet_malformed6"         test_parseTelnet_malformed6
    , testCase "test_parseTelnet_malformed7"         test_parseTelnet_malformed7
    , testCase "test_parseTelnet_malformed8"         test_parseTelnet_malformed8
    , testCase "test_parseTelnet_malformed9"         test_parseTelnet_malformed9
    , testCase "test_parseTelnet_malformed10"        test_parseTelnet_malformed10 ]


-- --------------------------------------------------


unitTests_Mud_Util_Text :: TestTree
unitTests_Mud_Util_Text = testGroup "Mud.Util.Text"
    [ testCase "test_countOcc_emptyNeedle"   test_countOcc_emptyNeedle
    , testCase "test_countOcc_emptyHaystack" test_countOcc_emptyHaystack
    , testCase "test_countOcc_zero"          test_countOcc_zero
    , testCase "test_countOcc_one"           test_countOcc_one
    , testCase "test_countOcc_two"           test_countOcc_two
    , testCase "test_countOcc_three"         test_countOcc_three
    , testCase "test_stripControl"           test_stripControl ]
