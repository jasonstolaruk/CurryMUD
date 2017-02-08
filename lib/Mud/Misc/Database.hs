{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TupleSections #-}

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
                         , countDbTblRecsChan
                         , countDbTblRecsQuestion
                         , countDbTblRecsTele
                         , createDbTbls
                         , dbOperation
                         , DiscoverRec(..)
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
                         , insertDbTblDiscover
                         , insertDbTblProf
                         , insertDbTblQuestion
                         , insertDbTblSacBonus
                         , insertDbTblSec
                         , insertDbTblTele
                         , insertDbTblTelnetChars
                         , insertDbTblTType
                         , insertDbTblTypo
                         , insertDbTblUnPw
                         , insertPropNames
                         , insertWords
                         , lookupPropName
                         , lookupPW
                         , lookupSacBonuses
                         , lookupTeleNames
                         , lookupWord
                         , ProfRec(..)
                         , PropNameRec(..)
                         , purgeDbTblAdminChan
                         , purgeDbTblAdminMsg
                         , purgeDbTblChan
                         , purgeDbTblQuestion
                         , purgeDbTblTele
                         , QuestionRec(..)
                         , SacBonusRec(..)
                         , SecRec(..)
                         , TeleRec(..)
                         , TelnetCharsRec(..)
                         , TTypeRec(..)
                         , TypoRec(..)
                         , UnPwRec(..)
                         , WordRec(..) ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Locks
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Arrow ((***))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Crypto.BCrypt (fastBcryptHashingPolicy, hashPasswordUsingPolicy)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.SQLite.Simple (Connection, FromRow, Only(..), Query(..), ToRow, execute, execute_, field, fromRow, query, query_, toRow, withConnection)
import Database.SQLite.Simple.FromRow (RowParser)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


data AdminChanRec   = AdminChanRec   { dbTimestamp   :: Text
                                     , dbName        :: Text
                                     , dbMsg         :: Text }
data AdminMsgRec    = AdminMsgRec    { dbTimestamp   :: Text
                                     , dbFromName    :: Text
                                     , dbToName      :: Text
                                     , dbMsg         :: Text }
data AlertExecRec   = AlertExecRec   { dbTimestamp   :: Text
                                     , dbName        :: Text
                                     , dbCmdName     :: Text
                                     , dbTarget      :: Text
                                     , dbArgs        :: Text }
data AlertMsgRec    = AlertMsgRec    { dbTimestamp   :: Text
                                     , dbName        :: Text
                                     , dbCmdName     :: Text
                                     , dbTrigger     :: Text
                                     , dbMsg         :: Text }
data BanHostRec     = BanHostRec     { dbTimestamp   :: Text
                                     , dbHost        :: Text
                                     , dbIsBanned    :: Bool
                                     , dbReason      :: Text }
data BanPCRec       = BanPCRec       { dbTimestamp   :: Text
                                     , dbName        :: Text
                                     , dbIsBanned    :: Bool
                                     , dbReason      :: Text }
data BonusRec       = BonusRec       { dbTimestamp   :: Text
                                     , dbFromName    :: Text
                                     , dbToName      :: Text
                                     , dbAmt         :: Int }
data BugRec         = BugRec         { dbTimestamp   :: Text
                                     , dbName        :: Text
                                     , dbLoc         :: Text
                                     , dbDesc        :: Text }
data ChanRec        = ChanRec        { dbTimestamp   :: Text
                                     , dbChanId      :: Int
                                     , dbChanName    :: Text
                                     , dbName        :: Text
                                     , dbMsg         :: Text }
data DiscoverRec    = DiscoverRec    { dbTimestamp   :: Text
                                     , dbHost        :: Text
                                     , dbMsg         :: Text }
data ProfRec        = ProfRec        { dbTimestamp   :: Text
                                     , dbHost        :: Text
                                     , dbProfanity   :: Text }
data PropNameRec    = PropNameRec    { dbWord        :: Text }
data QuestionRec    = QuestionRec    { dbTimestamp   :: Text
                                     , dbName        :: Text
                                     , dbMsg         :: Text }
data SacBonusRec    = SacBonusRec    { dbUTCTime     :: Text
                                     , dbName        :: Text
                                     , dbGodName     :: Text }
data SecRec         = SecRec         { dbName        :: Text
                                     , dbQ           :: Text
                                     , dbA           :: Text } deriving Eq
data TeleRec        = TeleRec        { dbTimestamp   :: Text
                                     , dbFromName    :: Text
                                     , dbToName      :: Text
                                     , dbMsg         :: Text }
data TelnetCharsRec = TelnetCharsRec { dbTimestamp   :: Text
                                     , dbHost        :: Text
                                     , dbTelnetChars :: Text }
data TTypeRec       = TTypeRec       { dbTimestamp   :: Text
                                     , dbHost        :: Text
                                     , dbTType       :: Text }
data TypoRec        = TypoRec        { dbTimestamp   :: Text
                                     , dbName        :: Text
                                     , dbLoc         :: Text
                                     , dbDesc        :: Text }
data UnPwRec        = UnPwRec        { dbUn          :: Text
                                     , dbPw          :: Text }
data WordRec        = WordRec        { dbWord        :: Text }


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
  fromRow = BugRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow ChanRec where
  fromRow = ChanRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field <*> field


instance FromRow DiscoverRec where
  fromRow = DiscoverRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow ProfRec where
  fromRow = ProfRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow PropNameRec where
  fromRow = PropNameRec <$ (field :: RowParser Int) <*> field


instance FromRow QuestionRec where
  fromRow = QuestionRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow SacBonusRec where
  fromRow = SacBonusRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow SecRec where
  fromRow = SecRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow TeleRec where
  fromRow = TeleRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow TelnetCharsRec where
  fromRow = TelnetCharsRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow TTypeRec where
  fromRow = TTypeRec <$ (field :: RowParser Int) <*> field <*> field <*> field


instance FromRow TypoRec where
  fromRow = TypoRec <$ (field :: RowParser Int) <*> field <*> field <*> field <*> field


instance FromRow UnPwRec where
  fromRow = UnPwRec <$ (field :: RowParser Int) <*> field <*> field


instance FromRow WordRec where
  fromRow = WordRec <$ (field :: RowParser Int) <*> field


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
  toRow (BugRec a b c d) = toRow (a, b, c, d)


instance ToRow ChanRec where
  toRow (ChanRec a b c d e) = toRow (a, b, c, d, e)


instance ToRow DiscoverRec where
  toRow (DiscoverRec a b c) = toRow (a, b, c)


instance ToRow ProfRec where
  toRow (ProfRec a b c) = toRow (a, b, c)


instance ToRow PropNameRec where
  toRow (PropNameRec a) = toRow . Only $ a


instance ToRow QuestionRec where
  toRow (QuestionRec a b c) = toRow (a, b, c)


instance ToRow SacBonusRec where
  toRow (SacBonusRec a b c) = toRow (a, b, c)


instance ToRow SecRec where
  toRow (SecRec a b c) = toRow (a, b, c)


instance ToRow TeleRec where
  toRow (TeleRec a b c d) = toRow (a, b, c, d)


instance ToRow TelnetCharsRec where
  toRow (TelnetCharsRec a b c) = toRow (a, b, c)


instance ToRow TTypeRec where
  toRow (TTypeRec a b c) = toRow (a, b, c)


instance ToRow TypoRec where
  toRow (TypoRec a b c d) = toRow (a, b, c, d)


instance ToRow UnPwRec where
  toRow (UnPwRec a b) = toRow (a, b)


instance ToRow WordRec where
  toRow (WordRec a) = toRow . Only $ a


-----


dbOperation :: IO a -> MudStack a
dbOperation f = liftIO . flip withLock f =<< getLock dbLock


onDbFile :: (Connection -> IO a) -> IO a
onDbFile f = flip withConnection f =<< mkMudFilePath dbFileFun


createDbTbls :: IO ()
createDbTbls = onDbFile $ \conn -> do
    forM_ qs $ execute_ conn . Query
    [Only x] <- query_ conn . Query $ "select count(*) from unpw" :: IO [Only Int]
    when (isZero x) . execute conn (Query "insert into unpw (id, un, pw) values (2, 'Curry', ?)") . Only =<< hashPW "curry"
    execute conn (Query "insert or ignore into unpw (id, un, pw) values (1, 'Root',  ?)") . Only =<< hashPW "root"
  where
    qs = [ "create table if not exists admin_chan   (id integer primary key, timestamp text, name text, msg text)"
         , "create table if not exists admin_msg    (id integer primary key, timestamp text, from_name text, to_name text, \
           \msg text)"
         , "create table if not exists alert_exec   (id integer primary key, timestamp text, name text, cmd_name text, \
           \target text, args text)"
         , "create table if not exists alert_msg    (id integer primary key, timestamp text, name text, cmd_name text, \
           \trigger text, msg text)"
         , "create table if not exists ban_host     (id integer primary key, timestamp text, host text, is_banned \
           \integer, reason text)"
         , "create table if not exists ban_pc       (id integer primary key, timestamp text, name text, is_banned \
           \integer, reason text)"
         , "create table if not exists bonus        (id integer primary key, timestamp text, from_name text, to_name text, \
           \amt integer)"
         , "create table if not exists bug          (id integer primary key, timestamp text, name text, loc text, desc text)"
         , "create table if not exists chan         (id integer primary key, timestamp text, chan_id integer, chan_name \
           \text, name text, msg text)"
         , "create table if not exists discover     (id integer primary key, timestamp text, host text, msg text)"
         , "create table if not exists profanity    (id integer primary key, timestamp text, host text, prof text)"
         , "create table if not exists prop_names   (id integer primary key, prop_name text)"
         , "create table if not exists question     (id integer primary key, timestamp text, name text, msg text)"
         , "create table if not exists sac_bonus    (id integer primary key, utc_time text, name text, god_name text)"
         , "create table if not exists sec          (id integer primary key, name text, question text, answer text)"
         , "create table if not exists tele         (id integer primary key, timestamp text, from_name text, to_name text, \
           \msg text)"
         , "create table if not exists telnet_chars (id integer primary key, timestamp text, host text, telnet_chars text)"
         , "create table if not exists ttype        (id integer primary key, timestamp text, host text, ttype text)"
         , "create table if not exists typo         (id integer primary key, timestamp text, name text, loc text, desc text)"
         , "create table if not exists unpw         (id integer primary key, un text, pw text)"
         , "create table if not exists words        (id integer primary key, word text)" ]


hashPW :: String -> IO Text
hashPW = maybeEmp T.decodeUtf8 `fmap2` (hashPasswordUsingPolicy fastBcryptHashingPolicy . B.pack)


-----


getDbTblRecs :: (FromRow a) => Text -> IO [a]
getDbTblRecs tblName = onDbFile (\conn -> query_ conn . Query $ "select * from " <> tblName)


-----


insertDbTblHelper :: (ToRow a) => Query -> a -> IO ()
insertDbTblHelper q x = onDbFile $ \conn -> execute conn q x


insertDbTblAdminChan :: AdminChanRec -> IO ()
insertDbTblAdminChan = insertDbTblHelper "insert into admin_chan (timestamp, name, msg) values (?, ?, ?)"


insertDbTblAdminMsg :: AdminMsgRec -> IO ()
insertDbTblAdminMsg = insertDbTblHelper "insert into admin_msg (timestamp, from_name, to_name, msg) values (?, ?, ?, ?)"


insertDbTblAlertExec :: AlertExecRec -> IO ()
insertDbTblAlertExec = insertDbTblHelper "insert into alert_exec (timestamp, name, cmd_name, target, args) values \
                                         \(?, ?, ?, ?, ?)"


insertDbTblAlertMsg :: AlertMsgRec -> IO ()
insertDbTblAlertMsg = insertDbTblHelper "insert into alert_msg (timestamp, name, cmd_name, trigger, msg) values \
                                        \(?, ?, ?, ?, ?)"


insertDbTblBanHost :: BanHostRec -> IO ()
insertDbTblBanHost = insertDbTblHelper "insert into ban_host (timestamp, host, is_banned, reason) values (?, ?, ?, ?)"


insertDbTblBanPC :: BanPCRec -> IO ()
insertDbTblBanPC = insertDbTblHelper "insert into ban_pc (timestamp, name, is_banned, reason) values (?, ?, ?, ?)"


insertDbTblBonus :: BonusRec -> IO ()
insertDbTblBonus = insertDbTblHelper "insert into bonus (timestamp, from_name, to_name, amt) values (?, ?, ?, ?)"


insertDbTblBug :: BugRec -> IO ()
insertDbTblBug = insertDbTblHelper "insert into bug (timestamp, name, loc, desc) values (?, ?, ?, ?)"


insertDbTblChan :: ChanRec -> IO ()
insertDbTblChan = insertDbTblHelper "insert into chan (timestamp, chan_id, chan_name, name, msg) values (?, ?, ?, ?, ?)"


insertDbTblDiscover :: DiscoverRec -> IO ()
insertDbTblDiscover = insertDbTblHelper "insert into discover (timestamp, host, msg) values (?, ?, ?)"


insertDbTblProf :: ProfRec -> IO ()
insertDbTblProf = insertDbTblHelper "insert into profanity (timestamp, host, prof) values (?, ?, ?)"


insertDbTblQuestion :: QuestionRec -> IO ()
insertDbTblQuestion = insertDbTblHelper "insert into question (timestamp, name, msg) values (?, ?, ?)"


insertDbTblSacBonus :: SacBonusRec -> IO ()
insertDbTblSacBonus = insertDbTblHelper "insert into sac_bonus (utc_time, name, god_name) values (?, ?, ?)"


insertDbTblSec :: SecRec -> IO ()
insertDbTblSec = insertDbTblHelper "insert into sec (name, question, answer) values (?, ?, ?)"


insertDbTblTele :: TeleRec -> IO ()
insertDbTblTele = insertDbTblHelper "insert into tele (timestamp, from_name, to_name, msg) values (?, ?, ?, ?)"


insertDbTblTelnetChars :: TelnetCharsRec -> IO ()
insertDbTblTelnetChars = insertDbTblHelper "insert into telnet_chars (timestamp, host, telnet_chars) values (?, ?, ?)"


insertDbTblTType :: TTypeRec -> IO ()
insertDbTblTType = insertDbTblHelper "insert into ttype (timestamp, host, ttype) values (?, ?, ?)"


insertDbTblTypo :: TypoRec -> IO ()
insertDbTblTypo = insertDbTblHelper "insert into typo (timestamp, name, loc, desc) values (?, ?, ?, ?)"


insertDbTblUnPw :: UnPwRec -> IO ()
insertDbTblUnPw rec@UnPwRec { .. } = hashPW (T.unpack dbPw) >>= \pw -> onDbFile $ \conn -> do
    execute conn "delete from unpw where un=?" . Only $ dbUn
    execute conn "insert into unpw (un, pw) values (?, ?)" rec { dbPw = pw }


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
countHelper tblName = onDbFile $ \conn -> query_ conn . Query $ "select count(*) from " <> tblName


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
purgeHelper tblName = onDbFile $ \conn -> execute conn q x
  where
    q = Query . T.concat $ [ "delete from ", tblName, " where id in (select id from ", tblName, " limit ?)" ]
    x = Only noOfDbTblRecsToPurge


-----


lookupPropName :: Text -> IO (Maybe Text)
lookupPropName t = onDbFile $ \conn -> f <$> query conn (Query "select prop_name from prop_names where prop_name = ?") (Only t)
  where
    f :: [Only Text] -> Maybe Text
    f (x:_) = Just . fromOnly $ x
    f _     = Nothing


lookupPW :: Sing -> IO (Maybe Text)
lookupPW s = onDbFile $ \conn -> f <$> query conn (Query "select pw from unpw where un = ?") (Only s)
  where
    f :: [Only Text] -> Maybe Text
    f (x:_) = Just . fromOnly $ x
    f _     = Nothing


lookupSacBonuses :: Sing -> IO [(UTCTime, GodName)]
lookupSacBonuses s = onDbFile $ \conn -> f <$> query conn (Query "select utc_time, god_name from sac_bonus where name = ?") (Only s)
  where
    f = let helper pair acc = case (reads . T.unpack *** reads . T.unpack) pair of
              ([(now :: UTCTime, "")], [(gn :: GodName, "")]) -> (now, gn) : acc
              _                                               -> acc
        in foldr helper []


lookupTeleNames :: Sing -> IO [Text]
lookupTeleNames s = onDbFile $ \conn -> f <$> query conn (Query t) (dup4 s)
  where
    f :: [Only Text] -> [Text]
    f = map fromOnly
    t = "select case\
        \  when from_name != ? then from_name\
        \  when to_name   != ? then to_name\
        \  end as name \
        \from (select from_name, to_name from tele where from_name = ? or to_name = ?)"


lookupWord :: Text -> IO (Maybe Text)
lookupWord t = onDbFile $ \conn -> f <$> query conn (Query "select word from words where word = ?") (Only t)
  where
    f :: [Only Text] -> Maybe Text
    f (x:_) = Just . fromOnly $ x
    f _     = Nothing


-----


insertPropNames :: Text -> IO ()
insertPropNames = procSrcFileTxt "prop_names" "prop_name"


procSrcFileTxt :: Text -> Text -> Text -> IO ()
procSrcFileTxt tblName colName = onDbFile . flip execute_ . buildQuery
  where
    buildQuery = Query . (T.concat [ "insert into ", tblName, " ", parensQuote colName, " values " ] <>) . buildVals
    buildVals  = commas . map (parensQuote . dblQuote) . T.lines . T.toLower


insertWords :: Text -> IO ()
insertWords = procSrcFileTxt "words" "word"
