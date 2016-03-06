{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.Database ( AdminChanRec(..)
                         , AdminMsgRec(..)
                         , BanHostRec(..)
                         , BanPlaRec(..)
                         , BugRec(..)
                         , ChanRec(..)
                         , countDbTblRecsAdminChan
                         , countDbTblRecsAdminMsg
                         , countDbTblRecsChan
                         , countDbTblRecsQuestion
                         , countDbTblRecsTele
                         , createDbTbls
                         , getDbTblRecs
                         , insertDbTblAdminChan
                         , insertDbTblAdminMsg
                         , insertDbTblBanHost
                         , insertDbTblBanPla
                         , insertDbTblBug
                         , insertDbTblChan
                         , insertDbTblProf
                         , insertDbTblQuestion
                         , insertDbTblTele
                         , insertDbTblTypo
                         , insertDbTblUnPwRec
                         , lookupPW
                         , ProfRec(..)
                         , purgeDbTblAdminChan
                         , purgeDbTblAdminMsg
                         , purgeDbTblChan
                         , purgeDbTblQuestion
                         , purgeDbTblTele
                         , QuestionRec(..)
                         , TeleRec(..)
                         , TypoRec(..)
                         , UnPwRec(..) ) where

import Mud.Data.State.MudData
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc

import Control.Monad (forM_)
import Crypto.BCrypt (fastBcryptHashingPolicy, hashPasswordUsingPolicy)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.SQLite.Simple (FromRow, Only(..), Query(..), ToRow, execute, execute_, field, fromRow, query, query_, toRow, withConnection)
import Database.SQLite.Simple.FromRow (RowParser)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


data AdminChanRec = AdminChanRec { adminChanTimestamp :: Text
                                 , adminChanName      :: Text
                                 , adminChanMsg       :: Text }
data AdminMsgRec  = AdminMsgRec  { adminMsgTimestamp  :: Text
                                 , adminMsgFromName   :: Text
                                 , adminMsgToName     :: Text
                                 , adminMsgMsg        :: Text }
data BanHostRec   = BanHostRec   { banHostTimestamp   :: Text
                                 , banHostHost        :: Text
                                 , banHostIsBanned    :: Bool
                                 , banHostReason      :: Text }
data BanPlaRec    = BanPlaRec    { banPlaTimestamp    :: Text
                                 , banPlaName         :: Text
                                 , banPlaIsBanned     :: Bool
                                 , banPlaReason       :: Text }
data BugRec       = BugRec       { bugTimestamp       :: Text
                                 , bugName            :: Text
                                 , bugLoc             :: Text
                                 , bugDesc            :: Text
                                 , bugIsOpen          :: Bool }
data ChanRec      = ChanRec      { chanTimestamp      :: Text
                                 , chanChanId         :: Int
                                 , chanChanName       :: Text
                                 , chanPCName         :: Text
                                 , chanMsg            :: Text }
data ProfRec      = ProfRec      { profTimestamp      :: Text
                                 , profHost           :: Text
                                 , profProfanity      :: Text }
data QuestionRec  = QuestionRec  { questionTimestamp  :: Text
                                 , questionName       :: Text
                                 , questionMsg        :: Text }
data TeleRec      = TeleRec      { teleTimestamp      :: Text
                                 , teleFromName       :: Text
                                 , teleToName         :: Text
                                 , teleMsg            :: Text }
data TypoRec      = TypoRec      { typoTimestamp      :: Text
                                 , typoName           :: Text
                                 , typoLoc            :: Text
                                 , typoDesc           :: Text
                                 , typoIsOpen         :: Bool }
data UnPwRec      = UnPwRec      { un                 :: Text
                                 , pw                 :: Text }


-----


instance FromRow AdminChanRec where
  fromRow = AdminChanRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow AdminMsgRec where
  fromRow = AdminMsgRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow BanHostRec where
  fromRow = BanHostRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow BanPlaRec where
  fromRow = BanPlaRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow BugRec where
  fromRow = BugRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field <*> field


instance FromRow ChanRec where
  fromRow = ChanRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field <*> field


instance FromRow ProfRec where
  fromRow = ProfRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow QuestionRec where
  fromRow = QuestionRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow TeleRec where
  fromRow = TeleRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow TypoRec where
  fromRow = TypoRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field <*> field


instance FromRow UnPwRec where
  fromRow = UnPwRec <$ (field :: RowParser Int) <*> field <*> field


-----


instance ToRow AdminChanRec where
  toRow (AdminChanRec a b c) = toRow (a, b, c)


instance ToRow AdminMsgRec where
  toRow (AdminMsgRec a b c d) = toRow (a, b, c, d)


instance ToRow BanHostRec where
  toRow (BanHostRec a b c d) = toRow (a, b, c, d)


instance ToRow BanPlaRec where
  toRow (BanPlaRec a b c d) = toRow (a, b, c, d)


instance ToRow BugRec where
  toRow (BugRec a b c d e) = toRow (a, b, c, d, e)


instance ToRow ChanRec where
  toRow (ChanRec a b c d e) = toRow (a, b, c, d, e)


instance ToRow ProfRec where
  toRow (ProfRec a b c) = toRow (a, b, c)


instance ToRow QuestionRec where
  toRow (QuestionRec a b c) = toRow (a, b, c)


instance ToRow TeleRec where
  toRow (TeleRec a b c d) = toRow (a, b, c, d)


instance ToRow TypoRec where
  toRow (TypoRec a b c d e) = toRow (a, b, c, d, e)


instance ToRow UnPwRec where
  toRow (UnPwRec a b) = toRow (a, b)


-----


createDbTbls :: IO ()
createDbTbls = withConnection dbFile $ \conn -> do
    forM_ qs $ execute_ conn . Query
    execute conn (Query "insert or ignore into unpw (id, un, pw) values (1, 'Root',  ?)") . Only =<< mkPW "root"
    execute conn (Query "insert or ignore into unpw (id, un, pw) values (2, 'Curry', ?)") . Only =<< mkPW "curry"
  where
    qs = [ "create table if not exists admin_chan (id integer primary key, timestamp text, name text, msg text)"
         , "create table if not exists admin_msg  (id integer primary key, timestamp text, fromName text, toName text, msg text)"
         , "create table if not exists ban_host   (id integer primary key, timestamp text, host text, is_banned integer, reason text)"
         , "create table if not exists ban_pla    (id integer primary key, timestamp text, name text, is_banned integer, reason text)"
         , "create table if not exists bug        (id integer primary key, timestamp text, name text, loc text, desc text, is_open integer)"
         , "create table if not exists chan       (id integer primary key, timestamp text, chan_id integer, chan_name text, name text, msg text)"
         , "create table if not exists profanity  (id integer primary key, timestamp text, host text, prof text)"
         , "create table if not exists question   (id integer primary key, timestamp text, name text, msg text)"
         , "create table if not exists tele       (id integer primary key, timestamp text, fromName text, toName text, msg text)"
         , "create table if not exists typo       (id integer primary key, timestamp text, name text, loc text, desc text, is_open integer)"
         , "create table if not exists unpw       (id integer primary key, un text, pw text)" ]
    mkPW = maybe "" T.decodeUtf8 `fmap2` (hashPasswordUsingPolicy fastBcryptHashingPolicy . B.pack)


-----


getDbTblRecs :: (FromRow a) => Text -> IO [a]
getDbTblRecs tblName = withConnection dbFile helper
  where
    helper conn = query_ conn . Query $ "select * from " <> tblName


-----


insertDbTblHelper :: (ToRow a) => Query -> a -> IO ()
insertDbTblHelper q x = withConnection dbFile helper
  where
    helper conn = execute conn q x


insertDbTblAdminChan :: AdminChanRec -> IO ()
insertDbTblAdminChan = insertDbTblHelper "insert into admin_chan (timestamp, name, msg) values (?, ?, ?)"


insertDbTblAdminMsg :: AdminMsgRec -> IO ()
insertDbTblAdminMsg = insertDbTblHelper "insert into admin_msg (timestamp, fromName, toName, msg) values (?, ?, ?, ?)"


insertDbTblBanHost :: BanHostRec -> IO ()
insertDbTblBanHost = insertDbTblHelper "insert into ban_host (timestamp, host, is_banned, reason) values (?, ?, ?, ?)"


insertDbTblBanPla :: BanPlaRec -> IO ()
insertDbTblBanPla = insertDbTblHelper "insert into ban_pla (timestamp, name, is_banned, reason) values (?, ?, ?, ?)"


insertDbTblBug :: BugRec -> IO ()
insertDbTblBug = insertDbTblHelper "insert into bug (timestamp, name, loc, desc, is_open) values (?, ?, ?, ?, ?)"


insertDbTblChan :: ChanRec -> IO ()
insertDbTblChan = insertDbTblHelper "insert into chan (timestamp, chan_id, chan_name, name, msg) values (?, ?, ?, ?, ?)"


insertDbTblProf :: ProfRec -> IO ()
insertDbTblProf = insertDbTblHelper "insert into profanity (timestamp, host, prof) values (?, ?, ?)"


insertDbTblQuestion :: QuestionRec -> IO ()
insertDbTblQuestion = insertDbTblHelper "insert into question (timestamp, name, msg) values (?, ?, ?)"


insertDbTblTele :: TeleRec -> IO ()
insertDbTblTele = insertDbTblHelper "insert into tele (timestamp, fromName, toName, msg) values (?, ?, ?, ?)"


insertDbTblTypo :: TypoRec -> IO ()
insertDbTblTypo = insertDbTblHelper "insert into typo (timestamp, name, loc, desc, is_open) values (?, ?, ?, ?, ?)"


insertDbTblUnPwRec :: UnPwRec -> IO ()
insertDbTblUnPwRec = insertDbTblHelper "insert into unpw (un, pw) values (?, ?)"


-----


countDbTblRecsAdminChan :: IO [Only Int]
countDbTblRecsAdminChan = countHelper "admin_chan"


countDbTblRecsAdminMsg :: IO [Only Int]
countDbTblRecsAdminMsg = countHelper "admin_msg"


countDbTblRecsChan :: IO [Only Int]
countDbTblRecsChan = countHelper "chan"


countDbTblRecsQuestion :: IO [Only Int]
countDbTblRecsQuestion = countHelper "question"


countDbTblRecsTele :: IO [Only Int]
countDbTblRecsTele = countHelper "tele"


countHelper :: Text -> IO [Only Int]
countHelper tblName = withConnection dbFile helper
  where
    helper conn = query_ conn . Query $ "select count(*) from " <> tblName


-----


purgeDbTblAdminChan :: IO ()
purgeDbTblAdminChan = purgeHelper "admin_chan"


purgeDbTblAdminMsg :: IO ()
purgeDbTblAdminMsg = purgeHelper "admin_msg"


purgeDbTblChan :: IO ()
purgeDbTblChan = purgeHelper "chan"


purgeDbTblQuestion :: IO ()
purgeDbTblQuestion = purgeHelper "question"


purgeDbTblTele :: IO ()
purgeDbTblTele = purgeHelper "tele"


purgeHelper :: Text -> IO ()
purgeHelper tblName = withConnection dbFile helper
  where
    helper conn = execute conn q x
    q           = Query . T.concat $ [ "delete from "
                                     , tblName
                                     , " where id in (select id from "
                                     , tblName
                                     , " limit ?)" ]
    x           = Only noOfDbTblRecsToPurge


-----


lookupPW :: Sing -> IO (Maybe Text)
lookupPW s = withConnection dbFile helper
  where
    helper conn = f <$> query conn (Query "select pw from unpw where un = ?") (Only s)
    f :: [Only Text] -> Maybe Text
    f [] = Nothing
    f xs = Just . fromOnly . head $ xs
