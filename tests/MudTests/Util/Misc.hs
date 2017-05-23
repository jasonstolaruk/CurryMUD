{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MudTests.Util.Misc where

import Mud.Util.Misc

import Data.IORef (newIORef, readIORef, writeIORef)
import Test.Tasty.HUnit ((@=?), Assertion)


default (Int, Double)


-- ==================================================


test_division_compareResults :: Assertion
test_division_compareResults = a && b @=? True
  where
    x = 5 `divideRound` 2
    y = 5 `intDivide`   2
    a = x == 2 && y == 3
    b = round 2.5 == 2 && round 2.6 == 3


-----


data AOrB = A | B deriving (Eq, Show)


helper :: (IO Bool -> IO () -> IO ()) -> Bool -> AOrB -> Assertion
helper f b aOrB = newIORef A >>= \ref -> do
    f (return b) . writeIORef ref $ B
    (aOrB @=?) =<< readIORef ref


test_mWhen_IOTrue :: Assertion
test_mWhen_IOTrue = helper mWhen True B


test_mWhen_IOFalse :: Assertion
test_mWhen_IOFalse = helper mWhen False A


test_mUnless_IOTrue :: Assertion
test_mUnless_IOTrue = helper mUnless True A


test_mUnless_IOFalse :: Assertion
test_mUnless_IOFalse = helper mUnless False B
