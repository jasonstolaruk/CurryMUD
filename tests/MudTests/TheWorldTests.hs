{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module MudTests.TheWorldTests where

import Mud.StateDataTypes
import Mud.TheWorld
import MudTests.TestHelpers

import Data.List (group, sort)
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck as QC (Property)


prop_noDupIds :: Property
prop_noDupIds = monadicIO $ do
    is <- inWorld allKeys
    assert . not . any ((> 1) . length) . group $ is


prop_getUnusedId :: Property
prop_getUnusedId = monadicIO $ do
    i  <- inWorld getUnusedId
    is <- inWorld allKeys
    assert $ i `notElem` is


prop_findAvailKey :: Inv -> Bool
prop_findAvailKey is = res `notElem` is
  where
    res = findAvailKey . sort $ is
