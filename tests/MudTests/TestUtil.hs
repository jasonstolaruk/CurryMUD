module MudTests.TestUtil where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.TheWorld.TheWorld
import Mud.TopLvlDefs.Misc

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Char (chr)
import Data.Functor ((<$>))
import Test.QuickCheck (Gen, choose)
import Test.QuickCheck.Monadic (PropertyM, run)
import qualified Data.Text as T


inWorld :: MudStack a -> PropertyM IO a
inWorld f = run helper
  where
    helper = runReaderT (initWorld >> f) =<< (liftIO . initMudData $ Don'tLog)


genAsciiAlphaNum :: Gen Char
genAsciiAlphaNum = chr <$> choose (32, 126)


genTextOfLen :: Int -> Gen T.Text
genTextOfLen n = T.pack <$> replicateM n genAsciiAlphaNum


genTextOfRandLen :: (Int, Int) -> Gen T.Text
genTextOfRandLen (nMin, nMax) = genTextOfLen =<< choose (nMin, nMax)


genTextLongerThan :: Int -> Gen T.Text
genTextLongerThan x = genTextOfLen . (x +) =<< choose (1, 50)


genCols :: Gen Int
genCols = choose (minCols, maxCols)
