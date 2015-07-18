{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.Database ( AdminChanRec(..)
                         , BanHostRec(..)
                         , BanPlaRec(..)
                         , BugRec(..)
                         , countDbTblRecsAdminChan
                         , createDbTbls
                         , getDbTblRecs
                         , insertDbTblAdminChan
                         , insertDbTblBanHost
                         , insertDbTblBanPla
                         , insertDbTblBug
                         , insertDbTblProf
                         , insertDbTblTypo
                         , ProfRec(..)
                         , purgeDbTblAdminChan
                         , TypoRec(..)) where

import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc

import Control.Monad (forM_)
import Data.Monoid ((<>))
import Database.SQLite.Simple (FromRow, Only(..), Query(..), ToRow, execute, execute_, field, fromRow, query_, toRow, withConnection)
import Database.SQLite.Simple.FromRow (RowParser)
import qualified Data.Text as T


data AdminChanRec = AdminChanRec { adminChanTimestamp :: T.Text
                                 , adminChanName      :: T.Text
                                 , adminChanMsg       :: T.Text }
data BanHostRec   = BanHostRec   { banHostTimestamp   :: T.Text
                                 , banHostHost        :: T.Text
                                 , banHostIsBanned    :: Bool
                                 , banHostReason      :: T.Text }
data BanPlaRec    = BanPlaRec    { banPlaTimestamp    :: T.Text
                                 , banPlaName         :: T.Text
                                 , banPlaIsBanned     :: Bool
                                 , banPlaReason       :: T.Text }
data BugRec       = BugRec       { bugTimestamp       :: T.Text
                                 , bugName            :: T.Text
                                 , bugLoc             :: T.Text
                                 , bugDesc            :: T.Text
                                 , bugIsOpen          :: Bool   }
data ProfRec      = ProfRec      { profTimestamp      :: T.Text
                                 , profHost           :: T.Text
                                 , profProfanity      :: T.Text }
data TypoRec      = TypoRec      { typoTimestamp      :: T.Text
                                 , typoName           :: T.Text
                                 , typoLoc            :: T.Text
                                 , typoDesc           :: T.Text
                                 , typoIsOpen         :: Bool   }


instance FromRow AdminChanRec where
  fromRow = AdminChanRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow BanHostRec where
  fromRow = BanHostRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow BanPlaRec where
  fromRow = BanPlaRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow BugRec where
  fromRow = BugRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field <*> field


instance FromRow ProfRec where
  fromRow = ProfRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow TypoRec where
  fromRow = TypoRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field <*> field


instance ToRow AdminChanRec where
  toRow (AdminChanRec a b c) = toRow (a, b, c)


instance ToRow BanHostRec where
  toRow (BanHostRec a b c d) = toRow (a, b, c, d)


instance ToRow BanPlaRec where
  toRow (BanPlaRec a b c d) = toRow (a, b, c, d)


instance ToRow BugRec where
  toRow (BugRec a b c d e) = toRow (a, b, c, d, e)


instance ToRow ProfRec where
  toRow (ProfRec a b c) = toRow (a, b, c)


instance ToRow TypoRec where
  toRow (TypoRec a b c d e) = toRow (a, b, c, d, e)


createDbTbls :: IO ()
createDbTbls = forM_ qs $ \q -> withConnection dbFile (`execute_` q)
  where
    qs = [ "create table if not exists admin_chan (id integer primary key, timestamp text, name text, msg text)"
         , "create table if not exists ban_host   (id integer primary key, timestamp text, host text, is_banned integer, reason text)"
         , "create table if not exists ban_pla    (id integer primary key, timestamp text, name text, is_banned integer, reason text)"
         , "create table if not exists bug        (id integer primary key, timestamp text, name text, loc text, desc text, is_open integer)"
         , "create table if not exists profanity  (id integer primary key, timestamp text, host text, prof text)"
         , "create table if not exists typo       (id integer primary key, timestamp text, name text, loc text, desc text, is_open integer)" ]


getDbTblRecs :: (FromRow a) => T.Text -> IO [a]
getDbTblRecs tblName = withConnection dbFile helper
  where
    helper conn = query_ conn . Query $ "select * from " <> tblName


insertDbTblHelper :: (ToRow a) => Query -> a -> IO ()
insertDbTblHelper q x = withConnection dbFile helper
  where
    helper conn = execute conn q x


insertDbTblAdminChan :: AdminChanRec -> IO ()
insertDbTblAdminChan = insertDbTblHelper "insert into admin_chan (timestamp, name, msg) values (?, ?, ?)"


insertDbTblBanHost :: BanHostRec -> IO ()
insertDbTblBanHost = insertDbTblHelper "insert into ban_host (timestamp, host, is_banned, reason) values (?, ?, ?, ?)"


insertDbTblBanPla :: BanPlaRec -> IO ()
insertDbTblBanPla = insertDbTblHelper "insert into ban_pla (timestamp, name, is_banned, reason) values (?, ?, ?, ?)"


insertDbTblBug :: BugRec -> IO ()
insertDbTblBug = insertDbTblHelper "insert into bug (timestamp, name, loc, desc, is_open) values (?, ?, ?, ?, ?)"


insertDbTblProf :: ProfRec -> IO ()
insertDbTblProf = insertDbTblHelper "insert into profanity (timestamp, host, prof) values (?, ?, ?)"


insertDbTblTypo :: TypoRec -> IO ()
insertDbTblTypo = insertDbTblHelper "insert into typo (timestamp, name, loc, desc, is_open) values (?, ?, ?, ?, ?)"


countDbTblRecsAdminChan :: IO [Only Int]
countDbTblRecsAdminChan = withConnection dbFile helper
  where
    helper conn = query_ conn "select count(*) from admin_chan"


purgeDbTblAdminChan :: IO ()
purgeDbTblAdminChan = withConnection dbFile helper
  where
    helper conn = execute conn q x
    q           = "delete from admin_chan where id in (select id from admin_chan limit ?)"
    x           = Only noOfDbTblRecsToPurge
