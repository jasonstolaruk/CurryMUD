module Mud.Data.State.Util.Random ( rndmDo
                                  , rndmR
                                  , rndmRs ) where

import Mud.Data.State.MudData

import Control.Applicative ((<$>))
import Control.Lens (view)
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Ix (inRange)
import System.Random.MWC (GenIO, uniformR)


type Count = Int
type Range = (Int, Int)


getGen :: MudStack GenIO
getGen = view gen <$> ask


probRange :: Range
probRange = (1, 100)


rndmDo :: Int -> MudStack () -> MudStack ()
rndmDo prob act = rndmR probRange >>= \res -> when (inRange (1, prob) res) act


rndmR :: Range -> MudStack Int
rndmR r = liftIO . uniformR r =<< getGen


rndmRs :: Count -> Range -> MudStack [Int]
rndmRs c = replicateM c . rndmR
