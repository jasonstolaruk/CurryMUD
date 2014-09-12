{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module MudTests.TheWorldTests where

import Mud.StateDataTypes
import Mud.StateHelpers
import MudTests.TestHelpers

import Control.Lens.Operators ((^.))
import Data.List (group)
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck as QC (Property)
import qualified Data.IntMap.Lazy as IM (elems)


prop_noDupIds :: Property
prop_noDupIds = monadicIO $ do
    it <- inWorld (getWS >>= \ws -> return (ws^.invTbl))
    assert . not . any ((> 1) . length) . group . concat . IM.elems $ it
