module MudTests.TestUtil where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.TheWorld.TheWorld
import           Mud.TopLvlDefs.Misc

import           Control.Monad (replicateM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT)
import           Data.Char (chr)
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.QuickCheck (Gen, choose)
import           Test.QuickCheck.Monadic (PropertyM, run)


inWorld :: MudStack a -> PropertyM IO a
inWorld f = run helper
  where
    helper = runReaderT (initWorld >> f) =<< (liftIO . initMudData $ Don'tLog)


genAsciiAlphaNum :: Gen Char
genAsciiAlphaNum = chr <$> choose (32, 126)


genCols :: Gen Int
genCols = choose (minCols, maxCols)


genTxtOfLen :: Int -> Gen Text
genTxtOfLen n = T.pack <$> replicateM n genAsciiAlphaNum


genTxtOfRndmLen :: (Int, Int) -> Gen Text
genTxtOfRndmLen (nMin, nMax) = genTxtOfLen =<< choose (nMin, nMax)


genTxtLongerThan :: Int -> Gen Text
genTxtLongerThan n = genTxtOfLen . (n +) =<< choose (1, 50)
