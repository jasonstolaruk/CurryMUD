module MudTests.Data.State.Util.Misc where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import MudTests.TestUtil

import Control.Lens (to)
import Control.Lens.Operators ((^.))
import qualified Data.IntMap.Lazy as IM (keys)
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)


prop_getUnusedId :: Property
prop_getUnusedId = monadicIO $ inWorld getState >>= \ms ->
    assert $ getUnusedId ms `notElem` ms^.typeTbl.to IM.keys
