module Mud.Data.State.Util.Random ( percent
                                  , rndmDo
                                  , rndmDos
                                  , rndmElem
                                  , rndmR
                                  , rndmRs ) where

import Mud.Data.State.MudData
import Mud.Util.Misc
import Mud.Util.Operators

import Control.Lens (view)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Ix (inRange)
import System.Random.MWC (GenIO, uniformR)


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


rndmPer :: MudStack Int
rndmPer = rndmR percent


rndmR :: Range -> MudStack Int
rndmR r = liftIO . uniformR r =<< getGen


rndmRs :: Count -> Range -> MudStack [Int]
rndmRs c = replicateM c . rndmR
