{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module MudTests.TheWorld.TheWorld where

import Mud.Data.State.State
import Mud.Data.State.Util.STM
import MudTests.TestUtil

import Control.Lens.Getter (view)
import Data.List (group)
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)
import qualified Data.IntMap.Lazy as IM (elems)


prop_noDupIds :: Property
prop_noDupIds = monadicIO $ getInvTbl >>= assert . not . any ((> 1) . length) . group . concat . IM.elems
  where
    getInvTbl = inWorld $ view invTbl `fmap` readWSTMVar
