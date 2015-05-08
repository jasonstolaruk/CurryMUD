{-# LANGUAGE TupleSections, ViewPatterns #-}

module MudTests.Data.State.Util.Random where

import Mud.Data.State.Util.Random
import MudTests.TestUtil

import Data.Ix (inRange)
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property, choose, forAll)


prop_rndmRs :: Property
prop_rndmRs = forAll (choose percent) $ \((1, ) -> range) -> monadicIO $ do
    res <- inWorld . rndmRs 100 $ range
    assert . all (inRange range) $ res
