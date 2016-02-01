{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Data.State.Util.Random ( percent
                                  , rndmDo
                                  , rndmDos
                                  , rndmElem
                                  , rndmIntToElem
                                  , rndmIntToPer
                                  , rndmIntToRange
                                  , rndmIntToRangeHelper
                                  , rndmPer
                                  , rndmR
                                  , rndmRs
                                  , rndmVector ) where

import Mud.Data.State.MudData
import Mud.Util.Misc hiding (blowUp)
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Util.Misc as U (blowUp)

import Control.Lens (view)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Ix (inRange)
import Data.Text (Text)
import qualified Data.Vector.Unboxed as V (Vector)
import System.Random.MWC (GenIO, uniformR, uniformVector)


blowUp :: Text -> Text -> [Text] -> a
blowUp = U.blowUp "Mud.Data.State.Util.Random"


-- ==================================================


type Count = Int
type Range = (Int, Int)


getGen :: MudStack GenIO
getGen = view gen <$> ask


isSuccess :: Int -> MudStack Bool
isSuccess prob = inRange (1, adjust prob) <$> rndmPer
  where
    adjust p = p < 1 ? 1 :? p


percent :: Range
percent = (1, 100)


rndmDo :: Int -> MudStack () -> MudStack ()
rndmDo prob = mWhen (isSuccess prob)


rndmDos :: [(Int, MudStack ())] -> MudStack ()
rndmDos []               = unit
rndmDos ((prob, act):xs) = mIf (isSuccess prob) act . rndmDos $ xs


rndmElem :: [a] -> MudStack a
rndmElem xs = (xs !!) <$> rndmR (0, length xs - 1)


rndmIntToElem :: Int -> [a] -> a
rndmIntToElem r xs = xs !! rndmIntToRange r (0, length xs - 1)


rndmIntToPer :: Int -> Int
rndmIntToPer = flip rndmIntToRange percent


rndmIntToRange :: Int -> Range -> Int
rndmIntToRange = rndmIntToRangeHelper maxBound


rndmIntToRangeHelper :: Int -> Int -> Range -> Int
rndmIntToRangeHelper m (abs -> r) pair@(x, y)
  | x <  0    = oops
  | y <= x    = oops
  | otherwise = let steps   = y - x + 1
                    stepAmt = m `div` steps
                    helper step z | r <= z + stepAmt = step
                                  | otherwise        = helper (succ step) $ z + stepAmt
                in x + helper 0 0
  where
    oops = blowUp "rndmIntToRangeHelper" "bad range" . pure . showText $ pair


rndmPer :: MudStack Int
rndmPer = rndmR percent


rndmR :: Range -> MudStack Int
rndmR r = liftIO . uniformR r =<< getGen


rndmRs :: Count -> Range -> MudStack [Int]
rndmRs c = replicateM c . rndmR


rndmVector :: Count -> MudStack (V.Vector Int)
rndmVector c = flip uniformVector c =<< getGen
