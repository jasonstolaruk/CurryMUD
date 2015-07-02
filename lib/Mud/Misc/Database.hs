{-# LANGUAGE FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Mud.Misc.Database ( Profanities
                         , ProfanitiesId
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
import Database.Persist.Sql (insert, rawQuery)
import Database.Persist.Sqlite (runMigration, runSqlite) -- TODO: Change to runMigrationSilent.
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Types (PersistValue(..))
import qualified Data.Conduit.List as CL (consume, map)
import qualified Data.Text as T


dbFile :: T.Text
dbFile = T.pack profanitiesDbFile


-- Creates a "migrateTables" function.
share [ mkPersist sqlSettings, mkMigrate "migrateTables" ] [persistLowerCase|
Profanities
   timestamp T.Text
   host      T.Text
   profanity T.Text
   deriving Show
|]


-- This does nothing if the table already exists.
mkDbTbl :: IO ()
mkDbTbl = runSqlite dbFile . runMigration $ migrateTables


dumpDbTbl :: IO ()
dumpDbTbl = runSqlite dbFile helper >>= mapM_ print
  where
    helper = rawQuery "select * from Profanities" [] $$ CL.map fromPersistent =$ CL.consume
    fromPersistent []                                                  = Nothing
    fromPersistent [ _, PersistText ts, PersistText h, PersistText p ] = Just . Profanities ts h $ p
    fromPersistent _                                                   = Nothing


insertDbTbl :: IO ()
insertDbTbl = runSqlite dbFile . void . insert . Profanities "2015-07-02 19:32:09" "somehost" $ "cunt"
