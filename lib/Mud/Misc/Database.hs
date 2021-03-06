{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedStrings, RecordWildCards, TypeApplications, ViewPatterns #-}

module Mud.Misc.Database ( AdminChanRec(..)
                         , AdminMsgRec(..)
                         , AlertExecRec(..)
                         , AlertMsgRec(..)
                         , BanHostRec(..)
                         , BanPCRec(..)
                         , BonusRec(..)
                         , BugRec(..)
                         , ChanRec(..)
                         , CountDbTblRecsFun
                         , DiscoverRec(..)
                         , DbTblName
                         , ProfRec(..)
                         , PropNameRec(..)
                         , PurgeDbTblFun
                         , QuestionRec(..)
                         , SacBonusRec(..)
                         , SacrificeRec(..)
                         , SecRec(..)
                         , TTypeRec(..)
                         , TeleRec(..)
                         , TelnetCharsRec(..)
                         , TypoRec(..)
                         , UnPwRec(..)
                         , WordRec(..)
                         , countDbTblRecsAdminChan
                         , countDbTblRecsAdminMsg
                         , countDbTblRecsChan
                         , countDbTblRecsPropNames
                         , countDbTblRecsQuestion
                         , countDbTblRecsTele
                         , countDbTblRecsWords
                         , createDbTbls
                         , dbOperation
                         , deleteDbTblRec
                         , deleteDbTblRecs
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
                         , insertDbTblSacrifice
                         , insertDbTblSec
                         , insertDbTblTType
                         , insertDbTblTele
                         , insertDbTblTelnetChars
                         , insertDbTblTypo
                         , insertDbTblUnPw
                         , insertPropNames
                         , insertWords
                         , lookupBonuses
                         , lookupBonusesFromTo
                         , lookupPW
                         , lookupPropName
                         , lookupSacBonusTime
                         , lookupSacrifices
                         , lookupSec
                         , lookupTeleNames
                         , lookupWord
                         , purgeDbTblAdminChan
                         , purgeDbTblAdminMsg
                         , purgeDbTblChan
                         , purgeDbTblQuestion
                         , purgeDbTblTele ) where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Locks
import           Mud.TopLvlDefs.FilePaths
import           Mud.TopLvlDefs.Misc
import           Mud.Util.Misc
import           Mud.Util.Quoting

import           Control.Monad (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Crypto.BCrypt (fastBcryptHashingPolicy, hashPasswordUsingPolicy)
import           Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.ByteString.Char8 as B
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import           Data.Time (UTCTime)
import           Database.SQLite.Simple (Connection, FromRow, Only(..), Query(..), ToRow, execute, execute_, field, fromRow, query, query_, toRow, withConnection)
import           GHC.Generics (Generic)

data    AdminChanRec   = AdminChanRec   { dbTimestamp   :: Text
                                        , dbName        :: Text
                                        , dbMsg         :: Text }
data    AdminMsgRec    = AdminMsgRec    { dbTimestamp   :: Text
                                        , dbFromName    :: Text
                                        , dbToName      :: Text
                                        , dbMsg         :: Text }
data    AlertExecRec   = AlertExecRec   { dbId          :: Maybe Int
                                        , dbTimestamp   :: Text
                                        , dbName        :: Text
                                        , dbCmdName     :: Text
                                        , dbTarget      :: Text
                                        , dbArgs        :: Text } deriving Generic
data    AlertMsgRec    = AlertMsgRec    { dbId          :: Maybe Int
                                        , dbTimestamp   :: Text
                                        , dbName        :: Text
                                        , dbCmdName     :: Text
                                        , dbTrigger     :: Text
                                        , dbMsg         :: Text } deriving Generic
data    BanHostRec     = BanHostRec     { dbId          :: Maybe Int
                                        , dbTimestamp   :: Text
                                        , dbHost        :: Text
                                        , dbIsBanned    :: Bool
                                        , dbReason      :: Text } deriving Generic
data    BanPCRec       = BanPCRec       { dbId          :: Maybe Int
                                        , dbTimestamp   :: Text
                                        , dbName        :: Text
                                        , dbIsBanned    :: Bool
                                        , dbReason      :: Text } deriving Generic
data    BonusRec       = BonusRec       { dbTimestamp   :: Text
                                        , dbFromName    :: Text
                                        , dbToName      :: Text
                                        , dbAmt         :: Int }
data    BugRec         = BugRec         { dbTimestamp   :: Text
                                        , dbName        :: Text
                                        , dbLoc         :: Text
                                        , dbDesc        :: Text } deriving Generic
data    ChanRec        = ChanRec        { dbTimestamp   :: Text
                                        , dbChanId      :: Int
                                        , dbChanName    :: Text
                                        , dbName        :: Text
                                        , dbMsg         :: Text }
data    DiscoverRec    = DiscoverRec    { dbTimestamp   :: Text
                                        , dbHost        :: Text
                                        , dbMsg         :: Text } deriving Generic
data    ProfRec        = ProfRec        { dbTimestamp   :: Text
                                        , dbHost        :: Text
                                        , dbProfanity   :: Text } deriving Generic
newtype PropNameRec    = PropNameRec    { dbWord        :: Text } deriving Generic
data    QuestionRec    = QuestionRec    { dbTimestamp   :: Text
                                        , dbName        :: Text
                                        , dbMsg         :: Text }
data    SacBonusRec    = SacBonusRec    { dbUTCTime     :: Text
                                        , dbName        :: Text
                                        , dbGodName     :: Text }
data    SacrificeRec   = SacrificeRec   { dbUTCTime     :: Text
                                        , dbName        :: Text
                                        , dbGodName     :: Text }
data    SecRec         = SecRec         { dbName        :: Text
                                        , dbQ           :: Text
                                        , dbA           :: Text } deriving Eq
data    TeleRec        = TeleRec        { dbTimestamp   :: Text
                                        , dbFromName    :: Text
                                        , dbToName      :: Text
                                        , dbMsg         :: Text }
data    TelnetCharsRec = TelnetCharsRec { dbTimestamp   :: Text
                                        , dbHost        :: Text
                                        , dbTelnetChars :: Text } deriving Generic
data    TTypeRec       = TTypeRec       { dbTimestamp   :: Text
                                        , dbHost        :: Text
                                        , dbTType       :: Text } deriving Generic
data    TypoRec        = TypoRec        { dbTimestamp   :: Text
                                        , dbName        :: Text
                                        , dbLoc         :: Text
                                        , dbDesc        :: Text } deriving Generic
data    UnPwRec        = UnPwRec        { dbUn          :: Text
                                        , dbPw          :: Text }
newtype WordRec        = WordRec        { dbWord        :: Text } deriving Generic

-----

instance FromRow AdminChanRec where
  fromRow = AdminChanRec <$ field @Int <*> field <*> field <*> field

instance FromRow AdminMsgRec where
  fromRow = AdminMsgRec <$ field @Int <*> field <*> field <*> field <*> field

instance FromRow AlertExecRec where
  fromRow = AlertExecRec <$> field @(Maybe Int) <*> field <*> field <*> field <*> field <*> field

instance FromRow AlertMsgRec where
  fromRow = AlertMsgRec <$> field @(Maybe Int) <*> field <*> field <*> field <*> field <*> field

instance FromRow BanHostRec where
  fromRow = BanHostRec <$> field @(Maybe Int) <*> field <*> field <*> field <*> field

instance FromRow BanPCRec where
  fromRow = BanPCRec <$> field @(Maybe Int) <*> field <*> field <*> field <*> field

instance FromRow BonusRec where
  fromRow = BonusRec <$ field @Int <*> field <*> field <*> field <*> field

instance FromRow BugRec where
  fromRow = BugRec <$ field @Int <*> field <*> field <*> field <*> field

instance FromRow ChanRec where
  fromRow = ChanRec <$ field @Int <*> field <*> field <*> field <*> field <*> field

instance FromRow DiscoverRec where
  fromRow = DiscoverRec <$ field @Int <*> field <*> field <*> field

instance FromRow ProfRec where
  fromRow = ProfRec <$ field @Int <*> field <*> field <*> field

instance FromRow PropNameRec where
  fromRow = PropNameRec <$ field @Int <*> field

instance FromRow QuestionRec where
  fromRow = QuestionRec <$ field @Int <*> field <*> field <*> field

instance FromRow SacBonusRec where
  fromRow = SacBonusRec <$ field @Int <*> field <*> field <*> field

instance FromRow SacrificeRec where
  fromRow = SacrificeRec <$ field @Int <*> field <*> field <*> field

instance FromRow SecRec where
  fromRow = SecRec <$ field @Int <*> field <*> field <*> field

instance FromRow TeleRec where
  fromRow = TeleRec <$ field @Int <*> field <*> field <*> field <*> field

instance FromRow TelnetCharsRec where
  fromRow = TelnetCharsRec <$ field @Int <*> field <*> field <*> field

instance FromRow TTypeRec where
  fromRow = TTypeRec <$ field @Int <*> field <*> field <*> field

instance FromRow TypoRec where
  fromRow = TypoRec <$ field @Int <*> field <*> field <*> field <*> field

instance FromRow UnPwRec where
  fromRow = UnPwRec <$ field @Int <*> field <*> field

instance FromRow WordRec where
  fromRow = WordRec <$ field @Int <*> field

-----

instance ToRow AdminChanRec where
  toRow (AdminChanRec a b c) = toRow (a, b, c)

instance ToRow AdminMsgRec where
  toRow (AdminMsgRec a b c d) = toRow (a, b, c, d)

instance ToRow AlertExecRec where
  toRow (AlertExecRec _ a b c d e) = toRow (a, b, c, d, e)

instance ToRow AlertMsgRec where
  toRow (AlertMsgRec _ a b c d e) = toRow (a, b, c, d, e)

instance ToRow BanHostRec where
  toRow (BanHostRec _ a b c d) = toRow (a, b, c, d)

instance ToRow BanPCRec where
  toRow (BanPCRec _ a b c d) = toRow (a, b, c, d)

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

instance ToRow SacrificeRec where
  toRow (SacrificeRec a b c) = toRow (a, b, c)

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

instance FromJSON AlertExecRec
instance FromJSON AlertMsgRec
instance FromJSON BanHostRec
instance FromJSON BanPCRec
instance FromJSON BugRec
instance FromJSON DiscoverRec
instance FromJSON ProfRec
instance FromJSON PropNameRec
instance FromJSON TelnetCharsRec
instance FromJSON TTypeRec
instance FromJSON TypoRec
instance FromJSON WordRec
instance ToJSON   AlertExecRec
instance ToJSON   AlertMsgRec
instance ToJSON   BanHostRec
instance ToJSON   BanPCRec
instance ToJSON   BugRec
instance ToJSON   DiscoverRec
instance ToJSON   ProfRec
instance ToJSON   PropNameRec
instance ToJSON   TelnetCharsRec
instance ToJSON   TTypeRec
instance ToJSON   TypoRec
instance ToJSON   WordRec

-----

dbOperation :: IO a -> MudStack a
dbOperation f = liftIO . flip withLock f =<< getLock dbLock

onDbFile :: (Connection -> IO a) -> IO a
onDbFile f = flip withConnection f =<< mkMudFilePath dbFileFun

createDbTbls :: IO ()
createDbTbls = onDbFile $ \conn -> do
    forM_ qs $ execute_ conn . Query
    x <- onlyIntsHelper <$> query_ conn (Query "select count(*) from unpw")
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
         , "create table if not exists sacrifice    (id integer primary key, utc_time text, name text, god_name text)"
         , "create table if not exists sec          (id integer primary key, name text, question text, answer text)"
         , "create table if not exists tele         (id integer primary key, timestamp text, from_name text, to_name text, \
           \msg text)"
         , "create table if not exists telnet_chars (id integer primary key, timestamp text, host text, telnet_chars text)"
         , "create table if not exists ttype        (id integer primary key, timestamp text, host text, ttype text)"
         , "create table if not exists typo         (id integer primary key, timestamp text, name text, loc text, desc text)"
         , "create table if not exists unpw         (id integer primary key, un text, pw text)"
         , "create table if not exists words        (id integer primary key, word text)" ]

onlyIntsHelper :: [Only Int] -> Int
onlyIntsHelper []         = 0
onlyIntsHelper (Only x:_) = x

hashPW :: String -> IO Text
hashPW = maybeEmp TE.decodeUtf8 `fmap2` (hashPasswordUsingPolicy fastBcryptHashingPolicy . B.pack)

-----

type DbTblName = Text

getDbTblRecs :: (FromRow a) => DbTblName -> IO [a]
getDbTblRecs tblName = onDbFile (\conn -> query_ conn . Query $ "select * from " <> tblName)

deleteDbTblRec :: DbTblName -> Int -> IO ()
deleteDbTblRec tblName i = onDbFile $ \conn -> execute conn (Query $ "delete from " <> tblName <> " where id=?") . Only $ i

deleteDbTblRecs :: DbTblName -> IO ()
deleteDbTblRecs tblName = onDbFile (\conn -> execute_ conn . Query $ "delete from " <> tblName)

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

insertDbTblSacrifice :: SacrificeRec -> IO ()
insertDbTblSacrifice = insertDbTblHelper "insert into sacrifice (utc_time, name, god_name) values (?, ?, ?)"

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

type CountDbTblRecsFun = IO Int

countDbTblRecsAdminChan :: CountDbTblRecsFun
countDbTblRecsAdminChan = countHelper "admin_chan"

countDbTblRecsAdminMsg :: CountDbTblRecsFun
countDbTblRecsAdminMsg = countHelper "admin_msg"

countDbTblRecsChan :: CountDbTblRecsFun
countDbTblRecsChan = countHelper "chan"

countDbTblRecsPropNames :: CountDbTblRecsFun
countDbTblRecsPropNames = countHelper "prop_names"

countDbTblRecsQuestion :: CountDbTblRecsFun
countDbTblRecsQuestion = countHelper "question"

countDbTblRecsTele :: CountDbTblRecsFun
countDbTblRecsTele = countHelper "tele"

countDbTblRecsWords :: CountDbTblRecsFun
countDbTblRecsWords = countHelper "words"

countHelper :: DbTblName -> IO Int
countHelper tblName = let q = Query $ "select count(*) from " <> tblName
                      in onDbFile $ \conn -> onlyIntsHelper <$> query_ conn q

-----

type PurgeDbTblFun = IO ()

purgeDbTblAdminChan :: PurgeDbTblFun
purgeDbTblAdminChan = purgeHelper "admin_chan"

purgeDbTblAdminMsg :: PurgeDbTblFun
purgeDbTblAdminMsg = purgeHelper "admin_msg"

purgeDbTblChan :: PurgeDbTblFun
purgeDbTblChan = purgeHelper "chan"

purgeDbTblQuestion :: PurgeDbTblFun
purgeDbTblQuestion = purgeHelper "question"

purgeDbTblTele :: PurgeDbTblFun
purgeDbTblTele = purgeHelper "tele"

purgeHelper :: DbTblName -> IO ()
purgeHelper tblName = onDbFile $ \conn -> execute conn q (Only noOfDbTblRecsToPurge)
  where
    q = Query . T.concat $ [ "delete from ", tblName, " where id in (select id from ", tblName, " limit ?)" ]

-----

lookupPropName :: Text -> IO (Maybe Text)
lookupPropName txt = let q = Query "select prop_name from prop_names where prop_name = ? collate nocase"
                     in onDbFile $ \conn -> onlyTxtsHelper <$> query conn q (Only txt)

onlyTxtsHelper :: [Only Text] -> Maybe Text
onlyTxtsHelper = listToMaybe . map fromOnly

lookupBonuses :: Sing -> IO [BonusRec]
lookupBonuses s = let q = Query "select * from bonus where from_name = ? or to_name = ?"
                  in onDbFile $ \conn -> query conn q . dup $ s

lookupBonusesFromTo :: Sing -> Sing -> IO Int
lookupBonusesFromTo fromSing toSing = let q = Query "select count(*) from bonus where from_name = ? and to_name = ?"
                                      in onDbFile $ \conn -> onlyIntsHelper <$> query conn q (fromSing, toSing)

lookupPW :: Sing -> IO (Maybe Text)
lookupPW s = onDbFile $ \conn -> onlyTxtsHelper <$> query conn (Query "select pw from unpw where un = ?") (Only s)

lookupSacBonusTime :: Sing -> Text -> IO (Maybe UTCTime) -- When was the last sacrifice bonus given?
lookupSacBonusTime s gn = let q = Query "select utc_time from sac_bonus where name = ? and god_name = ?"
                          in onDbFile $ \conn -> f <$> query conn q (s, gn)
  where
    f (reverse -> (Only t:_)) = case reads t of [(time, "")] -> Just time
                                                _            -> Nothing
    f _                       = Nothing

lookupSacrifices :: Sing -> Text -> IO Int -- How many sacrifices have been made?
lookupSacrifices s gn = let q = Query "select count(*) from sacrifice where name = ? and god_name = ?"
                        in onDbFile $ \conn -> onlyIntsHelper <$> query conn q (s, gn)

lookupSec :: Sing -> IO [SecRec]
lookupSec s = onDbFile $ \conn -> query conn (Query "select * from sec where name = ?") (Only s)

lookupTeleNames :: Sing -> IO [Text]
lookupTeleNames s = onDbFile $ \conn -> map fromOnly <$> query conn (Query t) (dup4 s)
  where
    t = "select case\
        \  when from_name != ? then from_name\
        \  when to_name   != ? then to_name\
        \  end as name \
        \from (select from_name, to_name from tele where from_name = ? or to_name = ?)"

lookupWord :: Text -> IO (Maybe Text)
lookupWord txt = onDbFile $ \conn -> let t = "select word from words where word = ? collate nocase"
                                     in onlyTxtsHelper <$> query conn (Query t) (Only txt)

-----

insertPropNames :: LT.Text -> IO ()
insertPropNames = procSrcFileTxt "prop_names" "prop_name"

procSrcFileTxt :: Text -> Text -> LT.Text -> IO ()
procSrcFileTxt tblName colName = onDbFile . flip execute_ . buildQuery
  where
    buildQuery    = let txt = T.concat [ "insert into ", tblName, " ", parensQuote colName, " values " ]
                    in Query . (txt <>) . LT.toStrict . buildVals
    buildVals txt = LT.intercalate ", " [ "(\"" <> t <> "\")" | t <- LT.lines txt ]

insertWords :: LT.Text -> IO ()
insertWords = procSrcFileTxt "words" "word"
