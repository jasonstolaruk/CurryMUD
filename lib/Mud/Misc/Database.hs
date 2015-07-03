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
                         , toProf
                         , Typo(..)
                         , TypoId ) where

import Mud.TopLvlDefs.FilePaths
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Monad (void)
import Data.Conduit (($$), (=$))
import Data.Monoid ((<>))
import Data.Time (UTCTime)
import Database.Persist.Class (fromPersistValue)
import Database.Persist.Sql (insert, rawQuery)
import Database.Persist.Sqlite (runMigrationSilent, runSqlite)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Types (PersistValue)
import qualified Data.Conduit.List as CL (consume, map)
import qualified Data.Text as T


-- ==================================================


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
  timestamp T.Text
  host      T.Text
  profanity T.Text
  deriving Show
Typo
  timestamp UTCTime
  name      T.Text
  loc       T.Text
  desc      T.Text
  isClosed  Bool
|]


dbFile' :: T.Text
dbFile' = T.pack dbFile


-- Has no effect if the tables already exist.
mkDbTbls :: IO ()
mkDbTbls = runSqlite dbFile' . void . runMigrationSilent $ migrateTables


dumpDbTbl tblName fromPersistent = runSqlite dbFile' helper
  where
    helper = rawQuery ("select * from " <> tblName) [] $$ CL.map fromPersistent =$ CL.consume


toProf :: [PersistValue] -> Either T.Text Prof
toProf [ _, timestampVal, hostVal, profanityVal ] = Prof <$> fromPersistValue timestampVal
                                                         <*> fromPersistValue hostVal
                                                         <*> fromPersistValue profanityVal
toProf vals = Left . T.concat $ [ "Could not convert a "
                                , dblQuote "Prof"
                                , " table field: "
                                , showText vals ]


insertDbTbl x = runSqlite dbFile' . void . insert $ x
