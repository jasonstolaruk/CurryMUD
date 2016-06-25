{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, RecordWildCards, TupleSections #-}

module Mud.Misc.Database ( AdminChanRec(..)
                         , AdminMsgRec(..)
                         , AlertExecRec(..)
                         , AlertMsgRec(..)
                         , BanHostRec(..)
                         , BanPCRec(..)
                         , BonusRec(..)
                         , BugRec(..)
                         , ChanRec(..)
                         , countDbTblRecsAdminChan
                         , countDbTblRecsAdminMsg
                         , countDbTblRecsBonus
                         , countDbTblRecsChan
                         , countDbTblRecsQuestion
                         , countDbTblRecsTele
                         , createDbTbls
                         , getDbTblRecs
                         , insertDbTblAdminChan
                         , insertDbTblAdminMsg
                         , insertDbTblAlertExec
                         , insertDbTblAlertMsg
                         , insertDbTblBanHost
                         , insertDbTblBanPC
                         , insertDbTblBonus
                         , insertDbTblBug
                         , insertDbTblChan
                         , insertDbTblProf
                         , insertDbTblQuestion
                         , insertDbTblSec
                         , insertDbTblTele
                         , insertDbTblTypo
                         , insertDbTblUnPw
                         , lookupPW
                         , ProfRec(..)
                         , purgeDbTblAdminChan
                         , purgeDbTblAdminMsg
                         , purgeDbTblBonus
                         , purgeDbTblChan
                         , purgeDbTblQuestion
                         , purgeDbTblTele
                         , QuestionRec(..)
                         , SecRec(..)
                         , TeleRec(..)
                         , TypoRec(..)
                         , UnPwRec(..) ) where

import Mud.Data.State.MudData
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc

import Control.Monad (forM_, when)
import Crypto.BCrypt (fastBcryptHashingPolicy, hashPasswordUsingPolicy)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.SQLite.Simple (FromRow, Only(..), Query(..), ToRow, execute, execute_, field, fromRow, query, query_, toRow, withConnection)
import Database.SQLite.Simple.FromRow (RowParser)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


data AdminChanRec = AdminChanRec { dbTimestamp :: Text
                                 , dbName      :: Text
                                 , dbMsg       :: Text }
data AdminMsgRec  = AdminMsgRec  { dbTimestamp :: Text
                                 , dbFromName  :: Text
                                 , dbToName    :: Text
                                 , dbMsg       :: Text }
data AlertExecRec = AlertExecRec { dbTimestamp :: Text
                                 , dbName      :: Text
                                 , dbCmdName   :: Text
                                 , dbTarget    :: Text
                                 , dbArgs      :: Text }
data AlertMsgRec  = AlertMsgRec  { dbTimestamp :: Text
                                 , dbName      :: Text
                                 , dbCmdName   :: Text
                                 , dbTrigger   :: Text
                                 , dbMsg       :: Text }
data BanHostRec   = BanHostRec   { dbTimestamp :: Text
                                 , dbHost      :: Text
                                 , dbIsBanned  :: Bool
                                 , dbReason    :: Text }
data BanPCRec     = BanPCRec     { dbTimestamp :: Text
                                 , dbName      :: Text
                                 , dbIsBanned  :: Bool
                                 , dbReason    :: Text }
data BonusRec     = BonusRec     { dbTimestamp :: Text
                                 , dbFromName  :: Text
                                 , dbToName    :: Text
                                 , dbAmt       :: Int }
data BugRec       = BugRec       { dbTimestamp :: Text
                                 , dbName      :: Text
                                 , dbLoc       :: Text
                                 , dbDesc      :: Text
                                 , dbIsOpen    :: Bool } -- TODO: Get rid of IsOpen fields?
data ChanRec      = ChanRec      { dbTimestamp :: Text
                                 , dbChanId    :: Int
                                 , dbChanName  :: Text
                                 , dbName      :: Text
                                 , dbMsg       :: Text }
data ProfRec      = ProfRec      { dbTimestamp :: Text
                                 , dbHost      :: Text
                                 , dbProfanity :: Text }
data QuestionRec  = QuestionRec  { dbTimestamp :: Text
                                 , dbName      :: Text
                                 , dbMsg       :: Text }
data SecRec       = SecRec       { dbName      :: Text
                                 , dbQ         :: Text
                                 , dbA         :: Text } deriving Eq
data TeleRec      = TeleRec      { dbTimestamp :: Text
                                 , dbFromName  :: Text
                                 , dbToName    :: Text
                                 , dbMsg       :: Text }
data TypoRec      = TypoRec      { dbTimestamp :: Text
                                 , dbName      :: Text
                                 , dbLoc       :: Text
                                 , dbDesc      :: Text
                                 , dbIsOpen    :: Bool }
data UnPwRec      = UnPwRec      { dbUn        :: Text
                                 , dbPw        :: Text }


-----


instance FromRow AdminChanRec where
  fromRow = AdminChanRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow AdminMsgRec where
  fromRow = AdminMsgRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow AlertExecRec where
  fromRow = AlertExecRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field <*> field


instance FromRow AlertMsgRec where
  fromRow = AlertMsgRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field <*> field


instance FromRow BanHostRec where
  fromRow = BanHostRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow BanPCRec where
  fromRow = BanPCRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow BonusRec where
  fromRow = BonusRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow BugRec where
  fromRow = BugRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field <*> field


instance FromRow ChanRec where
  fromRow = ChanRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field <*> field


instance FromRow ProfRec where
  fromRow = ProfRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow QuestionRec where
  fromRow = QuestionRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow SecRec where
  fromRow = SecRec <$ (field :: RowParser Int) <*> field <*> field <*> field


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


instance ToRow AlertExecRec where
  toRow (AlertExecRec a b c d e) = toRow (a, b, c, d, e)


instance ToRow AlertMsgRec where
  toRow (AlertMsgRec a b c d e) = toRow (a, b, c, d, e)


instance ToRow BanHostRec where
  toRow (BanHostRec a b c d) = toRow (a, b, c, d)


instance ToRow BanPCRec where
  toRow (BanPCRec a b c d) = toRow (a, b, c, d)


instance ToRow BonusRec where
  toRow (BonusRec a b c d) = toRow (a, b, c, d)


instance ToRow BugRec where
  toRow (BugRec a b c d e) = toRow (a, b, c, d, e)


instance ToRow ChanRec where
  toRow (ChanRec a b c d e) = toRow (a, b, c, d, e)


instance ToRow ProfRec where
  toRow (ProfRec a b c) = toRow (a, b, c)


instance ToRow QuestionRec where
  toRow (QuestionRec a b c) = toRow (a, b, c)


instance ToRow SecRec where
  toRow (SecRec a b c) = toRow (a, b, c)


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
    [Only x] <- query_ conn . Query $ "select count(*) from unpw" :: IO [Only Int]
    when (x == 0) $ execute conn (Query "insert into unpw (id, un, pw) values (2, 'Curry', ?)") . Only =<< hashPW "curry"
    execute conn (Query "insert or ignore into unpw (id, un, pw) values (1, 'Root',  ?)") . Only =<< hashPW "root"
  where
    qs = [ "create table if not exists admin_chan (id integer primary key, timestamp text, name text, msg text)"
         , "create table if not exists admin_msg  (id integer primary key, timestamp text, fromName text, toName text, msg text)"
         , "create table if not exists alert_exec (id integer primary key, timestamp text, name text, cmd_name text, target text, args text)"
         , "create table if not exists alert_msg  (id integer primary key, timestamp text, name text, cmd_name text, trigger text, msg text)"
         , "create table if not exists ban_host   (id integer primary key, timestamp text, host text, is_banned integer, reason text)"
         , "create table if not exists ban_pc     (id integer primary key, timestamp text, name text, is_banned integer, reason text)"
         , "create table if not exists bonus      (id integer primary key, timestamp text, fromName text, ToName text, amt integer)"
         , "create table if not exists bug        (id integer primary key, timestamp text, name text, loc text, desc text, is_open integer)"
         , "create table if not exists chan       (id integer primary key, timestamp text, chan_id integer, chan_name text, name text, msg text)"
         , "create table if not exists profanity  (id integer primary key, timestamp text, host text, prof text)"
         , "create table if not exists question   (id integer primary key, timestamp text, name text, msg text)"
         , "create table if not exists sec        (id integer primary key, name text, question text, answer text)"
         , "create table if not exists tele       (id integer primary key, timestamp text, fromName text, toName text, msg text)"
         , "create table if not exists typo       (id integer primary key, timestamp text, name text, loc text, desc text, is_open integer)"
         , "create table if not exists unpw       (id integer primary key, un text, pw text)" ]


hashPW :: String -> IO Text
hashPW = maybe "" T.decodeUtf8 `fmap2` (hashPasswordUsingPolicy fastBcryptHashingPolicy . B.pack)


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


insertDbTblAlertExec :: AlertExecRec -> IO ()
insertDbTblAlertExec = insertDbTblHelper "insert into alert_exec (timestamp, name, cmd_name, target, args) values (?, ?, ?, ?, ?)"


insertDbTblAlertMsg :: AlertMsgRec -> IO ()
insertDbTblAlertMsg = insertDbTblHelper "insert into alert_msg (timestamp, name, cmd_name, trigger, msg) values (?, ?, ?, ?, ?)"


insertDbTblBanHost :: BanHostRec -> IO ()
insertDbTblBanHost = insertDbTblHelper "insert into ban_host (timestamp, host, is_banned, reason) values (?, ?, ?, ?)"


insertDbTblBanPC :: BanPCRec -> IO ()
insertDbTblBanPC = insertDbTblHelper "insert into ban_pc (timestamp, name, is_banned, reason) values (?, ?, ?, ?)"


insertDbTblBonus :: BonusRec -> IO ()
insertDbTblBonus = insertDbTblHelper "insert into bonus (timestamp, fromName, toName, amt) values (?, ?, ?, ?)"


insertDbTblBug :: BugRec -> IO ()
insertDbTblBug = insertDbTblHelper "insert into bug (timestamp, name, loc, desc, is_open) values (?, ?, ?, ?, ?)"


insertDbTblChan :: ChanRec -> IO ()
insertDbTblChan = insertDbTblHelper "insert into chan (timestamp, chan_id, chan_name, name, msg) values (?, ?, ?, ?, ?)"


insertDbTblProf :: ProfRec -> IO ()
insertDbTblProf = insertDbTblHelper "insert into profanity (timestamp, host, prof) values (?, ?, ?)"


insertDbTblQuestion :: QuestionRec -> IO ()
insertDbTblQuestion = insertDbTblHelper "insert into question (timestamp, name, msg) values (?, ?, ?)"


insertDbTblSec :: SecRec -> IO ()
insertDbTblSec = insertDbTblHelper "insert into sec (name, question, answer) values (?, ?, ?)"


insertDbTblTele :: TeleRec -> IO ()
insertDbTblTele = insertDbTblHelper "insert into tele (timestamp, fromName, toName, msg) values (?, ?, ?, ?)"


insertDbTblTypo :: TypoRec -> IO ()
insertDbTblTypo = insertDbTblHelper "insert into typo (timestamp, name, loc, desc, is_open) values (?, ?, ?, ?, ?)"


insertDbTblUnPw :: UnPwRec -> IO ()
insertDbTblUnPw rec@UnPwRec { .. } = hashPW (T.unpack dbPw) >>= withConnection dbFile . helper
  where
    helper pw conn = do
        execute conn "delete from unpw where un=?" . Only $ dbUn
        execute conn "insert into unpw (un, pw) values (?, ?)" rec { dbPw = pw }


-----


countDbTblRecsAdminChan :: IO [Only Int]
countDbTblRecsAdminChan = countHelper "admin_chan"


countDbTblRecsAdminMsg :: IO [Only Int]
countDbTblRecsAdminMsg = countHelper "admin_msg"


countDbTblRecsBonus :: IO [Only Int] -- TODO: Use this.
countDbTblRecsBonus = countHelper "bonus"


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


purgeDbTblBonus :: IO () -- TODO: Use this.
purgeDbTblBonus = purgeHelper "bonus"


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
