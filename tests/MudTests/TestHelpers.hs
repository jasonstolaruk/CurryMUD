{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module MudTests.TestHelpers where

import Mud.StateDataTypes
import Mud.StateInIORefT
import Mud.TheWorld
import Mud.TopLvlDefs

import Control.Monad (replicateM)
import Data.Char (chr)
import Data.Functor ((<$>))
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (choose, Gen)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (PropertyM, run)
import qualified Data.Text as T


inWorld :: MudStack a -> PropertyM IO a
inWorld f = run helper
  where
    helper = fst <$> runStateInIORefT (initWorld >> f) (unsafePerformIO initMudState)


genAsciiAlphaNum :: Gen Char
genAsciiAlphaNum = chr <$> choose (32, 126)


genTextOfLen :: Int -> Gen T.Text
genTextOfLen n = T.pack <$> replicateM n genAsciiAlphaNum


genTextLongerThan :: Int -> Gen T.Text
genTextLongerThan x = genTextOfLen . (x +) =<< choose (1, 50)


genTextOfRandLen :: (Int, Int) -> Gen T.Text
genTextOfRandLen (nMin, nMax) = genTextOfLen =<< choose (nMin, nMax)


genCols :: Gen Int
genCols = choose (minCols, maxCols)
