{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}

module Mud.Misc.Database ( AdminChan(..)
                         , AdminChanId
                         , BanHost(..)
                         , BanHostId
                         , BanPla(..)
                         , BanPlaId
                         , Bug(..)
                         , BugId
                         , dumpDbTbl
                         , dumpDbTbl' -- TODO
                         , insertDbTbl
                         , migrateDbTbls
                         , Prof(..)
                         , ProfId
                         , Typo(..)
                         , TypoId ) where

import Mud.TopLvlDefs.FilePaths

import Control.Monad (void)
import Data.Conduit (($$), (=$))
import Data.Monoid ((<>))
import Database.Esqueleto -- TODO
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import qualified Data.Conduit.List as CL (consume, map)
import qualified Data.Text as T


-- ==================================================


share [ mkPersist sqlSettings, mkMigrate "migrateAll" ] [persistLowerCase|
AdminChan
  timestamp T.Text
  name      T.Text
  msg       T.Text
BanHost
  timestamp T.Text
  host      T.Text
  isBanned  Bool
  reason    T.Text
BanPla
  timestamp T.Text
  name      T.Text
  isBanned  Bool
  reason    T.Text
Bug
  timestamp T.Text
  name      T.Text
  loc       T.Text
  desc      T.Text
  isOpen    Bool
Prof
  timestamp T.Text
  host      T.Text
  profanity T.Text
Typo
  timestamp T.Text
  name      T.Text
  loc       T.Text
  desc      T.Text
  isOpen    Bool
|]


-- ==================================================


dbFile' :: T.Text
dbFile' = T.pack dbFile


dumpDbTbl tblName = runRawQuery $ "select * from " <> tblName


-- TODO: How can I get the AdminChans out of the stack?
dumpDbTbl' :: SqlPersistT IO [AdminChan]
dumpDbTbl' = map entityVal <$> (select . from $ return)


insertDbTbl x = runSqlite dbFile' . void . insert $ x


migrateDbTbls :: IO ()
migrateDbTbls = runSqlite dbFile' . void . runMigrationSilent $ migrateAll


runRawQuery query = runSqlite dbFile' helper
  where
    helper = rawQuery query [] $$ CL.map (fromPersistValues . tail) =$ CL.consume
