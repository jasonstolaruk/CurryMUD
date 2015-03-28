module Mud.Misc.Persist (persist) where

import Mud.Data.State.MudData
import Mud.TopLvlDefs.FilePaths

import Control.Lens.Operators ((^.))
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (encode, toJSON)
import Data.Conduit (($$), (=$), yield)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Conduit.Binary as CB (sinkFile)
import qualified Data.Conduit.List as CL (map)


persist :: MudState -> IO ()
persist ms = runResourceT $ do
    helper (ms^.armTbl  ) armTblFile
    helper (ms^.clothTbl) clothTblFile
    helper (ms^.coinsTbl) coinsTblFile
    helper (ms^.conTbl  ) conTblFile
    helper (ms^.entTbl  ) entTblFile
  where
    helper tbl file = yield (toJSON tbl) $$ CL.map (BL.toStrict . encode) =$ CB.sinkFile file
