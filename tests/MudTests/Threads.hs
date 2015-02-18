module MudTests.Threads where

import Mud.Data.State.Util.STM
import Mud.Threads
import Mud.Util.Misc
import MudTests.TestUtil

import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)


prop_getUnusedId :: Property
prop_getUnusedId = monadicIO $ readWSTMVar |$| inWorld >=> \ws ->
    assert $ getUnusedId ws `notElem` allKeys ws
