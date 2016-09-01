{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module MudTests.Util.Misc where

import Mud.Util.Misc

import Data.IORef (newIORef, readIORef, writeIORef)
import Test.Tasty.HUnit ((@=?), Assertion)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


test_division_compare_results :: Assertion
test_division_compare_results = a && b @=? True
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


test_mWhen_IO_True :: Assertion
test_mWhen_IO_True = helper mWhen True B


test_mWhen_IO_False :: Assertion
test_mWhen_IO_False = helper mWhen False A


test_mUnless_IO_True :: Assertion
test_mUnless_IO_True = helper mUnless True A


test_mUnless_IO_False :: Assertion
test_mUnless_IO_False = helper mUnless False B
