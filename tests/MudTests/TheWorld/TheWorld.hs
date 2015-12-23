module MudTests.TheWorld.TheWorld where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import MudTests.TestUtil

import Control.Lens (to)
import Control.Lens.Operators ((^.))
import Data.List (group)
import qualified Data.IntMap.Lazy as IM (elems)
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)


prop_noDupIds :: Property
prop_noDupIds = monadicIO $ do
    ms <- inWorld getState
    assert . not . any ((> 1) . length) . group . concat $ ms^.invTbl.to IM.elems
