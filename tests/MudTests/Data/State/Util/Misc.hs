{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module MudTests.Data.State.Util.Misc where

import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.STM
import MudTests.TestUtil

import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)


prop_getUnusedId :: Property
prop_getUnusedId = monadicIO $ inWorld readWSTMVar >>= \ws ->
    assert $ getUnusedId ws `notElem` allKeys ws
