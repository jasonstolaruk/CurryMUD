{-# LANGUAGE TupleSections, ViewPatterns #-}

module MudTests.Data.State.Util.Random where

import Mud.Data.State.Util.Random
import MudTests.TestUtil

import Data.Ix (inRange)
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty.QuickCheck (Property, choose, forAll)


prop_rndmRs_within_range :: Property
prop_rndmRs_within_range = forAll (choose percent) $ \((1, ) -> range) ->
    monadicIO $ assert . all (inRange range) =<< (inWorld . rndmRs 100 $ range)


prop_rndmRs_no_range :: Property
prop_rndmRs_no_range = forAll (choose percent) $ \x ->
    monadicIO $ assert . all (== x) =<< (inWorld . rndmRs 100 $ (x, x))


prop_rndmRs_minimal_range :: Property
prop_rndmRs_minimal_range = forAll (choose percent) $ \x@((+ 1) -> y) ->
    monadicIO $ assert . all (== x) . filter (/= y) =<< (inWorld . rndmRs 100 $ (x, y))
