{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module MudTests.StateHelpersTests where

import Mud.StateHelpers
import MudTests.TestHelpers

import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)


prop_getUnusedId :: Property
prop_getUnusedId = monadicIO $ inWorld readWSTMVar >>= \ws ->
    assert $ getUnusedId ws `notElem` allKeys ws
