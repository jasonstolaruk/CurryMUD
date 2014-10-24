{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module MudTests.TheWorldTests where

import Mud.StateDataTypes
import Mud.StateHelpers
import MudTests.TestHelpers

import Control.Lens.Operators ((^.))
import Data.Functor ((<$>))
import Data.List (group)
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property)
import qualified Data.IntMap.Lazy as IM (elems)


prop_noDupIds :: Property
prop_noDupIds = monadicIO $ getInvTbl >>= assert . not . any ((> 1) . length) . group . concat . IM.elems
  where
    getInvTbl = inWorld ((^.invTbl) <$> readWSTMVar)
