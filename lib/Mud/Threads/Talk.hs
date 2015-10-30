{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.Threads.Talk ( runTalkAsync
                        , threadTalk ) where

import Mud.Cmds.Util.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Interp.Login
import Mud.TheWorld.AdminZoneIds (iWelcome)
import Mud.Threads.InacTimer
import Mud.Threads.Misc
import Mud.Threads.Receive
import Mud.Threads.Server
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (asyncThreadId, race_)
import Control.Concurrent.STM.TMQueue (newTMQueueIO)
import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Exception.Lifted (finally, handle, try)
import Control.Lens (at, views)
import Control.Lens.Operators ((&), (.~), (?~))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Bits (zeroBits)
import Data.List ((\\))
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import Network (HostName)
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (keys)
import qualified Data.Map.Lazy as M (empty)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import System.FilePath ((</>))
import System.IO (BufferMode(..), Handle, Newline(..), NewlineMode(..), hClose, hSetBuffering, hSetEncoding, hSetNewlineMode, latin1)
import System.Random (randomIO, randomRIO)


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Talk"


-- ==================================================


runTalkAsync :: Handle -> HostName -> MudStack ()
runTalkAsync h host = runAsync (threadTalk h host) >>= \a@(asyncThreadId -> ti) ->
    modifyState $ (, ()) . (talkAsyncTbl.at ti ?~ a)


-----


threadTalk :: Handle -> HostName -> MudStack ()
threadTalk h host = helper `finally` cleanUp
  where
    helper = do
        (mq, tq)           <- liftIO $ (,) <$> newTQueueIO <*> newTMQueueIO
        (i, dblQuote -> s) <- adHoc mq host
        setThreadType . Talk $ i
        handle (plaThreadExHandler "talk" i) $ onEnv $ \md -> do
            liftIO configBuffer
            dumpTitle mq
            prompt    mq "By what name are you known?"
            bcastAdmins $ "A new player has connected: " <> s <> "."
            logNotice "threadTalk helper" $ "new PC name for incoming player: " <> s <> "."
            liftIO . void . forkIO . runReaderT (threadInacTimer i mq tq) $ md
            liftIO $ race_ (runReaderT (threadServer  h i mq tq) md)
                           (runReaderT (threadReceive h i mq)    md)
    configBuffer = hSetBuffering h LineBuffering >> hSetNewlineMode h nlMode >> hSetEncoding h latin1
    nlMode       = NewlineMode { inputNL = CRLF, outputNL = CRLF }
    cleanUp      = do
        logNotice "threadTalk cleanUp" $ "closing the handle for " <> T.pack host <> "."
        liftIO . hClose $ h


adHoc :: MsgQueue -> HostName -> MudStack (Id, Sing)
adHoc mq host = do
    (sexy, r) <- liftIO $ (,) <$> randomSex <*> randomRace
    ct        <- liftIO getCurrentTime
    modifyState $ \ms ->
        let i    = getUnusedId ms
            s    = showText r <> showText i
            e    = Ent { _entId    = i
                       , _entName  = Nothing
                       , _sing     = s
                       , _plur     = ""
                       , _entDesc  = capitalize $ mkThrPerPro sexy <> " is an ad-hoc player character."
                       , _entFlags = zeroBits }
            m    = Mob { _sex   = sexy
                       , _st    = 50
                       , _dx    = 50
                       , _iq    = 50
                       , _ht    = 50
                       , _maxHp = 100, _curHp = 100
                       , _maxMp = 100, _curMp = 100
                       , _maxPp = 100, _curPp = 100
                       , _maxFp = 100, _curFp = 100
                       , _xp    = 0
                       , _hand  = RHand }
            pc   = PC  { _rmId       = iWelcome
                       , _race       = r
                       , _introduced = []
                       , _linked     = [] }
            pla  = Pla { _currHostName = host
                       , _connectTime  = Just ct
                       , _plaFlags     = zeroBits
                       , _columns      = 80
                       , _pageLines    = 24
                       , _interp       = Just interpName
                       , _peepers      = []
                       , _peeping      = []
                       , _regenAsync   = Nothing
                       , _retainedMsgs = []
                       , _lastRmId     = Nothing }
            ms'  = ms  & coinsTbl        .ind i        .~ mempty
                       & entTbl          .ind i        .~ e
                       & eqTbl           .ind i        .~ M.empty
                       & invTbl          .ind i        .~ []
                       & invTbl          .ind iWelcome .~ getInv iWelcome ms ++ pure i
                       & mobTbl          .ind i        .~ m
                       & msgQueueTbl     .ind i        .~ mq
                       & pcTbl           .ind i        .~ pc
                       & plaTbl          .ind i        .~ pla
                       & rndmNamesMstrTbl.ind i        .~ M.empty
                       & teleLinkMstrTbl .ind i        .~ M.empty
                       & typeTbl         .ind i        .~ PCType
        in (ms', (i, s))


randomSex :: IO Sex
randomSex = ([ Male, Female ] !!) . fromEnum <$> (randomIO :: IO Bool)


randomRace :: IO Race
randomRace = randomIO


getUnusedId :: MudState -> Id
getUnusedId = views typeTbl (head . ([0..] \\) . IM.keys)


dumpTitle :: MsgQueue -> MudStack ()
dumpTitle mq = liftIO mkFilename >>= try . takeADump >>= eitherRet (fileIOExHandler "dumpTitle")
  where
    mkFilename   = ("title" ++) . show <$> randomRIO (1, noOfTitles)
    takeADump fn = send mq . nlPrefix . nl =<< (liftIO . T.readFile $ titleDir </> fn)
