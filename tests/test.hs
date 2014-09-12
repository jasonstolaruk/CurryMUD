{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

import MudTests.StateHelpersTests
import MudTests.TheWorldTests
import MudTests.UtilTests

import Test.Tasty (defaultMain, testGroup, TestTree)
--import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC (testProperty)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ testStateHelpers, testTheWorld, testUtil, unitTests ]

-- ==================================================

testStateHelpers :: TestTree
testStateHelpers = testGroup "StateHelpersTests" [ QC.testProperty "prop_getUnusedId" prop_getUnusedId ]

testTheWorld :: TestTree
testTheWorld = testGroup "TheWorldTests" [ QC.testProperty "prop_noDupIds" prop_noDupIds ]

-- --------------------------------------------------

testUtil :: TestTree
testUtil = testGroup "UtilTests"
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

-- --------------------------------------------------

unitTests :: TestTree
unitTests = testGroup "Unit tests" [] -- TODO

{-
  HUnit example:

  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT
  ]
-}
