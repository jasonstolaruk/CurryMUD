{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}

module Mud.Misc.Database ( BanHost(..)
                         , BanHostId
                         , BanPla(..)
                         , BanPlaId
                         , Bug(..)
                         , BugId
                         , dumpDbTbl
                         , insertDbTbl
                         , mkDbTbls
                         , Prof(..)
                         , ProfId
                         , Typo(..)
                         , TypoId ) where

import Mud.TopLvlDefs.FilePaths

import Control.Monad (void)
import Data.Conduit (($$), (=$))
import Data.Monoid ((<>))
import Data.Time (UTCTime)
import Database.Persist.Class (fromPersistValues)
import Database.Persist.Sql (insert, rawQuery)
import Database.Persist.Sqlite (runMigrationSilent, runSqlite)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import qualified Data.Conduit.List as CL (consume, map)
import qualified Data.Text as T


-- ==================================================


-- Creates a "migrateAll" function.
share [ mkPersist sqlSettings, mkMigrate "migrateAll" ] [persistLowerCase|
BanHost
  timestamp T.Text
  host      T.Text
  isBanned  Bool
  reason    T.Text
  deriving Show
BanPla
  timestamp T.Text
  name      T.Text
  isBanned  Bool
  reason    T.Text
  deriving Show
Bug
  timestamp T.Text
  name      T.Text
  loc       T.Text
  desc      T.Text
  isClosed  Bool
  deriving Show
Prof
  timestamp T.Text
  host      T.Text
  profanity T.Text
  deriving Show
Typo
  timestamp T.Text
  name      T.Text
  loc       T.Text
  desc      T.Text
  isClosed  Bool
  deriving Show
|]


dbFile' :: T.Text
dbFile' = T.pack dbFile


-- Has no effect if the tables already exist.
mkDbTbls :: IO ()
mkDbTbls = runSqlite dbFile' . void . runMigrationSilent $ migrateAll


dumpDbTbl tblName = runSqlite dbFile' helper
  where
    helper = rawQuery ("select * from " <> tblName) [] $$ CL.map fromPersistValues =$ CL.consume


insertDbTbl x = runSqlite dbFile' . void . insert $ x
