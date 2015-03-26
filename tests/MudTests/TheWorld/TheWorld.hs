module MudTests.TheWorld.TheWorld where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import MudTests.TestUtil

import Control.Lens (to)
import Control.Lens.Operators ((^.))
import Data.List (group)
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)
import qualified Data.IntMap.Lazy as IM (elems)


-- TODO: We will probably have to change the name of this module after introducing the database...


prop_noDupIds :: Property
prop_noDupIds = monadicIO $ do
    ms <- inWorld getState
    assert . not . any ((> 1) . length) . group . concat $ ms^.invTbl.to IM.elems
