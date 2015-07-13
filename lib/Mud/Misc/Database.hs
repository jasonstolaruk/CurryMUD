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
                         , dumpDbTblAdmniChan
                         , dumpDbTblBanHost
                         , dumpDbTblBanPla
                         , dumpDbTblBug
                         , dumpDbTblProf
                         , dumpDbTblTypo
                         , insertDbTbl
                         , migrateDbTbls
                         , Prof(..)
                         , ProfId
                         , Typo(..)
                         , TypoId ) where

import Mud.Data.State.MudData
import Mud.TopLvlDefs.FilePaths

import Control.Exception (SomeException)
import Control.Exception.Lifted (catch)
import Control.Monad (void)
import Data.Conduit (($$), (=$))
import Data.Monoid ((<>))
import Database.Esqueleto -- TODO
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import qualified Data.Conduit.List as CL (consume, map)
import qualified Data.Text as T


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


migrateDbTbls :: IO ()
migrateDbTbls = runSqlite dbFile' . void . runMigrationSilent $ migrateAll


dumpDbTbl tblName = runRawQuery $ "select * from " <> tblName


runRawQuery query = runSqlite dbFile' helper
  where
    helper = rawQuery query [] $$ CL.map (fromPersistValues . tail) =$ CL.consume


insertDbTbl x = runSqlite dbFile' . void . insert $ x


-- ==================================================


dumpDbTblAdmniChan :: MudStack (Either SomeException [AdminChan])
dumpDbTblAdmniChan = (Right <$> runSqlite dbFile' helper) `catch` (return . Left)
  where
    helper = do
        xs <- select $
              from $ \x -> do
              return x
        let xs' = map entityVal xs
        return xs'


dumpDbTblBanHost :: MudStack [BanHost]
dumpDbTblBanHost = runSqlite dbFile' helper
  where
    helper = do
        xs <- select $
              from $ \x -> do
              return x
        let xs' = map entityVal xs
        return xs'


dumpDbTblBanPla :: MudStack [BanPla]
dumpDbTblBanPla = runSqlite dbFile' helper
  where
    helper = do
        xs <- select $
              from $ \x -> do
              return x
        let xs' = map entityVal xs
        return xs'


dumpDbTblBug :: MudStack [Bug]
dumpDbTblBug = runSqlite dbFile' helper
  where
    helper = do
        xs <- select $
              from $ \x -> do
              return x
        let xs' = map entityVal xs
        return xs'


dumpDbTblProf :: MudStack [Prof]
dumpDbTblProf = runSqlite dbFile' helper
  where
    helper = do
        xs <- select $
              from $ \x -> do
              return x
        let xs' = map entityVal xs
        return xs'


dumpDbTblTypo :: MudStack [Typo]
dumpDbTblTypo = runSqlite dbFile' helper
  where
    helper = do
        xs <- select $
              from $ \x -> do
              return x
        let xs' = map entityVal xs
        return xs'
