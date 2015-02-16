module MudTests.Threads where

import Mud.Data.State.Util.STM
import Mud.Threads
import MudTests.TestUtil

import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)


prop_getUnusedId :: Property
prop_getUnusedId = monadicIO $ inWorld readWSTMVar >>= \ws ->
    assert $ getUnusedId ws `notElem` allKeys ws
