{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.Database ( AdminChanRec(..)
                         , BanHostRec(..)
                         , BanPlaRec(..)
                         , BugRec(..)
                         , ProfRec(..)
                         , TypoRec(..)
                         , createDbTbls
                         , getDbTblRecs
                         , insertDbTblAdminChan
                         , insertDbTblBanHost
                         , insertDbTblBanPla
                         , insertDbTblBug
                         , insertDbTblProf
                         , insertDbTblTypo ) where

import Mud.TopLvlDefs.FilePaths

import Control.Monad (forM_)
import Data.Monoid ((<>))
import Database.SQLite.Simple (FromRow, Query(..), ToRow, execute, execute_, field, fromRow, query_, toRow, withConnection)
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
    qs = [ "CREATE TABLE IF NOT EXISTS admin_chan (id INTEGER PRIMARY KEY, timestamp TEXT, name TEXT, msg TEXT)"
         , "CREATE TABLE IF NOT EXISTS ban_host   (id INTEGER PRIMARY KEY, timestamp TEXT, host TEXT, is_banned INTEGER, reason TEXT)"
         , "CREATE TABLE IF NOT EXISTS ban_pla    (id INTEGER PRIMARY KEY, timestamp TEXT, name TEXT, is_banned INTEGER, reason TEXT)"
         , "CREATE TABLE IF NOT EXISTS bug        (id INTEGER PRIMARY KEY, timestamp TEXT, name TEXT, loc TEXT, desc TEXT, is_open INTEGER)"
         , "CREATE TABLE IF NOT EXISTS profanity  (id INTEGER PRIMARY KEY, timestamp TEXT, host TEXT, prof TEXT)"
         , "CREATE TABLE IF NOT EXISTS typo       (id INTEGER PRIMARY KEY, timestamp TEXT, name TEXT, loc TEXT, desc TEXT, is_open INTEGER)" ]


getDbTblRecs :: (FromRow a) => T.Text -> IO [a]
getDbTblRecs tblName = withConnection dbFile helper
  where
    helper conn = query_ conn . Query $ "SELECT * FROM " <> tblName


insertDbTblHelper :: (ToRow a) => Query -> a -> IO ()
insertDbTblHelper q x = withConnection dbFile f
  where
    f conn = execute conn q x


insertDbTblAdminChan :: AdminChanRec -> IO ()
insertDbTblAdminChan = insertDbTblHelper "INSERT INTO admin_chan (timestamp, name, msg) VALUES (?, ?, ?)"


insertDbTblBanHost :: BanHostRec -> IO ()
insertDbTblBanHost = insertDbTblHelper "INSERT INTO ban_host (timestamp, host, is_banned, reason) VALUES (?, ?, ?, ?)"


insertDbTblBanPla :: BanPlaRec -> IO ()
insertDbTblBanPla = insertDbTblHelper "INSERT INTO ban_pla (timestamp, name, is_banned, reason) VALUES (?, ?, ?, ?)"


insertDbTblBug :: BugRec -> IO ()
insertDbTblBug = insertDbTblHelper "INSERT INTO bug (timestamp, name, loc, desc, is_open) VALUES (?, ?, ?, ?, ?)"


insertDbTblProf :: ProfRec -> IO ()
insertDbTblProf = insertDbTblHelper "INSERT INTO profanity (timestamp, host, prof) VALUES (?, ?, ?)"


insertDbTblTypo :: TypoRec -> IO ()
insertDbTblTypo = insertDbTblHelper "INSERT INTO typo (timestamp, name, loc, desc, is_open) VALUES (?, ?, ?, ?, ?)"
