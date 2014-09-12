{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module MudTests.StateHelpersTests where

import Mud.StateHelpers
import MudTests.TestHelpers

import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck as QC (Property)


prop_getUnusedId :: Property
prop_getUnusedId = monadicIO $ do
    ws <- inWorld getWS
    assert $ getUnusedId ws `notElem` allKeys ws
