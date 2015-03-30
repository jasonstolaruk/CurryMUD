{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Mud.Misc.Persist (persist) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.TopLvlDefs.FilePaths
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar)
import Control.Lens (views)
import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (encode, toJSON)
import Data.Conduit (($$), (=$), yield)
import Data.IORef (readIORef)
import Data.Tuple (swap)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Conduit.Binary as CB (sinkFile)
import qualified Data.Conduit.List as CL (map)
import qualified Data.IntMap.Lazy as IM (fromList, map)
import qualified Data.Map.Lazy as M (toList)
import qualified Data.Text as T


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Misc.Persist"


-- ==================================================


persist :: MudStack ()
persist = do
    logNotice "persist" "persisting the world."
    liftIO . uncurry persistHelper =<< onEnv mkBindings
  where
    mkBindings md = return . (md^.persisterTMVar, ) =<< (liftIO . readIORef $ md^.mudStateIORef)


persistHelper :: TMVar PersisterDone -> MudState -> IO ()
persistHelper persistTMVar ms = do
    atomically . void . takeTMVar $ persistTMVar
    flip withAsync wait $ mapM_ runResourceT [ helper (ms^.armTbl    ) armTblFile
                                             , helper (ms^.clothTbl  ) clothTblFile
                                             , helper (ms^.coinsTbl  ) coinsTblFile
                                             , helper (ms^.conTbl    ) conTblFile
                                             , helper (ms^.entTbl    ) entTblFile
                                             , helper (eqTblHelper ms) eqTblFile
                                             , helper (ms^.invTbl    ) invTblFile
                                             , helper (ms^.mobTbl    ) mobTblFile
                                             , helper (ms^.objTbl    ) objTblFile
                                             , helper (ms^.pcTbl     ) pcTblFile
                                             , helper (ms^.plaTbl    ) plaTblFile
                                             , helper (ms^.rmTbl     ) rmTblFile
                                             , helper (ms^.typeTbl   ) typeTblFile
                                             , helper (ms^.wpnTbl    ) wpnTblFile ]
    atomically . putTMVar persistTMVar $ PersisterDone
  where
    helper tbl file = yield (toJSON tbl) $$ CL.map (BL.toStrict . encode) =$ CB.sinkFile file
    eqTblHelper     = views eqTbl convertEqMaps
    convertEqMaps   = IM.map (IM.fromList . map swap . M.toList)
