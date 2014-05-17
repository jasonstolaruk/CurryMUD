{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module MudTests.TestHelpers where

import Mud.StateDataTypes
import Mud.StateInIORefT
import Mud.TheWorld
import Mud.TopLvlDefs

import Control.Lens.Operators ((^.))
import Control.Monad (replicateM)
import Data.Char (chr)
import Data.Functor ((<$>))
import Data.Text.Strict.Lens (packed)
import qualified Data.Text as T
import Test.QuickCheck (choose, Gen)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (PropertyM, run)


initWorldForMonadicTests :: MudStack ()
initWorldForMonadicTests = createWorld >> sortAllInvs


inWorld :: MudStack a -> PropertyM IO a
inWorld f = run $ runStateInIORefT (initWorldForMonadicTests >> f) initMudState >>= \(a, _) -> return a


genAlphaNum :: Gen Char
genAlphaNum = chr <$> choose (32, 126)


genTextOfLen :: Int -> Gen T.Text
genTextOfLen n = (^.packed) <$> replicateM n genAlphaNum


genTextLongerThan :: Int -> Gen T.Text
genTextLongerThan x = choose (1, 50) >>= \y ->
    genTextOfLen $ x + y


genTextOfRandLen :: (Int, Int) -> Gen T.Text
genTextOfRandLen (nMin, nMax) = choose (nMin, nMax) >>= \n ->
    genTextOfLen n


genCols :: Gen Int
genCols = choose (minCols, maxCols)
