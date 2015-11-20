module MudTests.Threads.Talk where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Threads.Talk
import MudTests.TestUtil

import Control.Lens (to)
import Control.Lens.Operators ((^.))
import qualified Data.IntMap.Lazy as IM (keys)
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)


prop_getUnusedId :: Property
prop_getUnusedId = monadicIO $ do
    ms <- inWorld getState
    assert $ getUnusedId ms `notElem` ms^.typeTbl.to IM.keys
