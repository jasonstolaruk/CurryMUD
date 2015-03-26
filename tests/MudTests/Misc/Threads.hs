module MudTests.Misc.Threads where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Misc.Threads
import MudTests.TestUtil

import Control.Lens (to)
import Control.Lens.Operators ((^.))
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)
import qualified Data.IntMap.Lazy as IM (keys)


prop_getUnusedId :: Property
prop_getUnusedId = monadicIO $ do
    ms <- inWorld getState
    assert $ getUnusedId ms `notElem` ms^.typeTbl.to IM.keys
