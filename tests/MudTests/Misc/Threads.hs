module MudTests.Misc.Threads where

import Mud.Data.State.MudData
import Mud.Misc.Threads
import MudTests.TestUtil

import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Lens.Getter (view)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)
import qualified Data.IntMap.Lazy as IM (keys)


prop_getUnusedId :: Property
prop_getUnusedId = monadicIO $ do
    tt <- inWorld $ ask >>= liftIO . readTVarIO . view typeTblTVar
    assert $ getUnusedId tt `notElem` IM.keys tt
