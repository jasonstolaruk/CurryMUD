{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Mud.Misc.Persist (persist) where

import Mud.Cmds.Util.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import qualified Mud.Misc.Logging as L (logExMsg, logNotice)

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import Control.Exception (SomeException)
import Control.Exception.Lifted (catch)
import Control.Lens (views)
import Control.Lens.Operators ((^.))
import Control.Monad ((>=>), void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (encode, toJSON)
import Data.Conduit (($$), (=$), yield)
import Data.IORef (readIORef)
import Data.List (sort)
import Data.Tuple (swap)
import System.Directory (createDirectory, doesDirectoryExist, getDirectoryContents, removeDirectoryRecursive)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as B (toStrict)
import qualified Data.Conduit.Binary as CB (sinkFile)
import qualified Data.Conduit.List as CL (map)
import qualified Data.IntMap.Lazy as IM (fromList, map)
import qualified Data.Map.Lazy as M (toList)
import qualified Data.Text as T


logExMsg :: T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Misc.Persist"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Misc.Persist"


-- ==================================================


persist :: MudStack ()
persist = do
    logNotice "persist" "persisting the world."
    (mkBindings |&| onEnv >=> liftIO . uncurry persistHelper) `catch` persistExHandler
  where
    mkBindings md = md^.mudStateIORef |&| liftIO . readIORef >=> return . (md^.locks.persistLock, )


persistHelper :: Lock -> MudState -> IO ()
persistHelper l ms = do
    atomically . void . takeTMVar $ l
    path <- getNonExistingPath =<< (persistDir </>) . T.unpack . T.replace ":" "-" <$> mkTimestamp
    createDirectory path
    cont <- dropIrrelevantFilenames . sort <$> getDirectoryContents persistDir
    when (length cont > noOfPersistedWorlds) . removeDirectoryRecursive . (persistDir </>) . head $ cont
    flip withAsync wait $ mapM_ runResourceT [ helper (ms^.armTbl       ) $ path </> armTblFile
                                             , helper (ms^.clothTbl     ) $ path </> clothTblFile
                                             , helper (ms^.coinsTbl     ) $ path </> coinsTblFile
                                             , helper (ms^.conTbl       ) $ path </> conTblFile
                                             , helper (ms^.entTbl       ) $ path </> entTblFile
                                             , helper (eqTblHelper ms   ) $ path </> eqTblFile
                                             , helper (ms^.hostTbl      ) $ path </> hostTblFile
                                             , helper (ms^.invTbl       ) $ path </> invTblFile
                                             , helper (ms^.mobTbl       ) $ path </> mobTblFile
                                             , helper (ms^.objTbl       ) $ path </> objTblFile
                                             , helper (ms^.pcTbl        ) $ path </> pcTblFile
                                             , helper (ms^.plaTbl       ) $ path </> plaTblFile
                                             , helper (ms^.rmTbl        ) $ path </> rmTblFile
                                             , helper (ms^.rmTeleNameTbl) $ path </> rmTeleNameTblFile
                                             , helper (ms^.typeTbl      ) $ path </> typeTblFile
                                             , helper (ms^.wpnTbl       ) $ path </> wpnTblFile ]
    atomically . putTMVar l $ Done
  where
    getNonExistingPath path = mIf (doesDirectoryExist path)
                                  (getNonExistingPath $ path ++ "_")
                                  (return path)
    helper tbl file = yield (toJSON tbl) $$ CL.map (B.toStrict . encode) =$ CB.sinkFile file
    eqTblHelper     = views eqTbl convertEqMaps
    convertEqMaps   = IM.map (IM.fromList . map swap . M.toList)


persistExHandler :: SomeException -> MudStack ()
persistExHandler e = do
    logExMsg "persistExHandler" "exception caught while persisting the world; rethrowing to listen thread" e
    throwToListenThread e
