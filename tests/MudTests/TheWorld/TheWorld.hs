module MudTests.TheWorld.TheWorld where

import Mud.Data.State.MudData
import MudTests.TestUtil

import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Lens.Getter (view)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.List (group)
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)
import qualified Data.IntMap.Lazy as IM (elems)


prop_noDupIds :: Property
prop_noDupIds = monadicIO $ do
    getInvTbl >>= assert . not . any ((> 1) . length) . group . concat . IM.elems
  where
    getInvTbl = inWorld $ liftIO . readTVarIO . view invTblTVar =<< ask
