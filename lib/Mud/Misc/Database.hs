{-# LANGUAGE FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Mud.Misc.Database ( Prof
                         , ProfId
                         , dumpDbTbl
                         , insertDbTbl
                         , mkDbTbl ) where

-- TODO: Delete.
{-
import Mud.Data.State.MudData
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import Control.Exception (SomeException)
import Control.Exception.Lifted (catch)
import Control.Lens (views)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.IORef (readIORef)
import System.Directory (createDirectory, doesDirectoryExist, getDirectoryContents, removeDirectoryRecursive)
import qualified Data.ByteString.Lazy as B (toStrict)
import qualified Data.Conduit.Binary as CB (sinkFile)
import qualified Data.IntMap.Lazy as IM (fromList, map)
import qualified Data.Map.Lazy as M (toList)
-}

import Mud.TopLvlDefs.FilePaths

import Control.Monad (void)
import Data.Conduit (($$), (=$))
import Data.Time (UTCTime)
import Database.Persist.Sql (insert, rawQuery)
import Database.Persist.Sqlite (runMigrationSilent, runSqlite)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Types (PersistValue(..))
import qualified Data.Conduit.List as CL (consume, map)
import qualified Data.Text as T


dbFile :: T.Text
dbFile = T.pack profanitiesDbFile


-- Creates a "migrateTables" function.
share [ mkPersist sqlSettings, mkMigrate "migrateTables" ] [persistLowerCase|
BanHost
  timestamp UTCTime
  host      T.Text
  isBanned  Bool
  reason    T.Text
BanPla
  timestamp UTCTime
  name      T.Text
  isBanned  Bool
  reason    T.Text
Bug
  timestamp UTCTime
  name      T.Text
  loc       T.Text
  desc      T.Text
  isClosed  Bool
Prof
  timestamp UTCTime
  host      T.Text
  profanity T.Text
  deriving Show -- TODO: Remove.
Typo
  timestamp UTCTime
  name      T.Text
  loc       T.Text
  desc      T.Text
  isClosed  Bool
|]


-- Has no effect if the tables already exist.
mkDbTbls :: IO ()
mkDbTbls = runSqlite dbFile . runMigrationSilent $ migrateTables


dumpDbTbl tblName fromPersistent = runSqlite dbFile helper
  where
    helper = rawQuery ("select * from " ++ tblName) [] $$ CL.map fromPersistent =$ CL.consume
    -- fromPersistent []                                                  = Nothing
    -- fromPersistent [ _, PersistText ts, PersistText h, PersistText p ] = Just . Prof ts h $ p
    -- fromPersistent _                                                   = Nothing


insertDbTbl :: a -> IO ()
insertDbTbl = runSqlite dbFile . void . insert
