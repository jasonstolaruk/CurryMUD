{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.Threads.Talk ( runTalkAsync
                        , threadTalk ) where

import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Util.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Interp.Login
import Mud.Misc.Misc
import Mud.TheWorld.Zones.AdminZoneIds (iWelcome)
import Mud.Threads.InacTimer
import Mud.Threads.Misc
import Mud.Threads.Receive
import Mud.Threads.Server
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Telnet.Chars
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent.Async (asyncThreadId, cancel, wait)
import Control.Concurrent.STM.TMQueue (newTMQueueIO)
import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Exception.Lifted (finally, handle, try)
import Control.Lens (at)
import Control.Lens.Operators ((%~), (&), (.~), (?~))
import Control.Monad.IO.Class (liftIO)
import Data.Bits (setBit, zeroBits)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Network (HostName)
import Prelude hiding (pi)
import qualified Data.Map.Lazy as M (empty)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import System.FilePath ((</>))
import System.IO (BufferMode(..), Handle, Newline(..), NewlineMode(..), hClose, hSetBuffering, hSetEncoding, hSetNewlineMode, latin1)
import System.Random (randomIO, randomRIO)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Talk"


-- ==================================================


runTalkAsync :: Handle -> HostName -> MudStack ()
runTalkAsync h host = runAsync (threadTalk h host) >>= \a@(asyncThreadId -> ti) -> tweak $ talkAsyncTbl.at ti ?~ a


-----


threadTalk :: Handle -> HostName -> MudStack ()
threadTalk h host = helper `finally` cleanUp
  where
    helper = do
        (mq, tq) <- liftIO $ (,) <$> newTQueueIO <*> newTMQueueIO
        (i, s  ) <- adHoc mq host
        setThreadType . Talk $ i
        handle (threadExHandler (Just i) "talk") $ do
            liftIO configBuffer
            telnetHelper mq
            send         mq . nl       $ "CurryMUD " <> ver
            dumpTitle    mq
            send         mq . nlnl     $ helloRulesMsg
            send         mq . nl . prd $ "If you are new to CurryMUD, please enter " <> dblQuote "new"
            sendPrompt   mq "What is your character's name?"
            bcastAdmins . prd $ "A new player has connected: " <> s
            logNotice "threadTalk helper" . prd $ "new PC name for incoming player: " <> s
            onNewThread   . threadInacTimer i   mq $ tq
            a <- runAsync . threadReceive h i $ mq
            b <- runAsync . threadServer  h i   mq $ tq
            liftIO $ wait b >> cancel a
    telnetHelper mq = mapM_ (send mq) [ telnetWillTType, telnetTTypeRequest, telnetWillGMCP ]
    configBuffer    = hSetBuffering h LineBuffering >> hSetNewlineMode h nlMode >> hSetEncoding h latin1
    nlMode          = NewlineMode { inputNL = CRLF, outputNL = CRLF }
    cleanUp         = do
        logNotice "threadTalk cleanUp" . prd $ "closing the handle for " <> T.pack host
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
                       , _entSmell = Nothing
                       , _entFlags = zeroBits }
            m    = Mob { _sex            = sexy
                       , _st             = 10
                       , _dx             = 10
                       , _ht             = 10
                       , _ma             = 10
                       , _ps             = 10
                       , _maxHp          = 50, _curHp = 50
                       , _maxMp          = 50, _curMp = 50
                       , _maxPp          = 50, _curPp = 50
                       , _maxFp          = 50, _curFp = 50
                       , _exp            = 0
                       , _lvl            = 0
                       , _hand           = RHand
                       , _knownLangs     = pure . raceToLang $ r
                       , _rmId           = iWelcome
                       , _lastRmId       = iWelcome
                       , _mobRmDesc      = Nothing
                       , _tempDesc       = Nothing
                       , _mobSize        = Nothing
                       , _corpseWeight   = calcCorpseWeight   r
                       , _corpseVol      = calcCorpseVol      r
                       , _corpseCapacity = calcCorpseCapacity r
                       , _party          = dfltParty
                       , _stomach        = []
                       , _digesterAsync  = Nothing
                       , _feelingMap     = M.empty
                       , _actMap         = M.empty
                       , _nowEating      = Nothing
                       , _nowDrinking    = Nothing
                       , _regenQueue     = Nothing
                       , _interp         = Just . interpName $ 0 }
            pc   = PC  { _race       = r
                       , _introduced = []
                       , _linked     = []
                       , _skillPts   = 0 }
            pla  = Pla { _currHostName = host
                       , _connectTime  = Just ct
                       , _plaFlags     = initPlaFlags
                       , _columns      = 80
                       , _pageLines    = 24
                       , _peepers      = []
                       , _peeping      = []
                       , _possessing   = Nothing
                       , _retainedMsgs = []
                       , _logoutRmId   = Nothing
                       , _bonusTime    = Nothing
                       , _spiritAsync  = Nothing }
            ms'  = ms  & activeEffectsTbl.ind i .~ []
                       & coinsTbl        .ind i .~ mempty
                       & entTbl          .ind i .~ e
                       & eqTbl           .ind i .~ M.empty
                       & invTbl          .ind i .~ []
                       & mobTbl          .ind i .~ m
                       & msgQueueTbl     .ind i .~ mq
                       & pausedEffectsTbl.ind i .~ []
                       & pcSingTbl       .at  s ?~ i
                       & pcTbl           .ind i .~ pc
                       & plaTbl          .ind i .~ pla
                       & rndmNamesMstrTbl.ind i .~ M.empty
                       & teleLinkMstrTbl .ind i .~ M.empty
                       & typeTbl         .ind i .~ PCType
        in (ms' & invTbl.ind iWelcome %~ addToInv ms' (pure i), (i, s))


randomSex :: IO Sex
randomSex = ([ Male, Female ] !!) . fromEnum <$> (randomIO :: IO Bool)


randomRace :: IO Race
randomRace = randomIO


initPlaFlags :: Int
initPlaFlags = foldl setBit zeroBits . map fromEnum $ [ IsShowingHp
                                                      , IsShowingMp
                                                      , IsShowingPp
                                                      , IsShowingFp ]


dumpTitle :: MsgQueue -> MudStack ()
dumpTitle mq = liftIO mkFilename >>= try . takeADump >>= eitherRet (fileIOExHandler "dumpTitle")
  where
    mkFilename   = ("title" ++) . show <$> randomRIO (1, noOfTitles)
    takeADump fn = send mq . nlPrefix . nl =<< liftIO (T.readFile =<< (</> fn) <$> mkMudFilePath titleDirFun)
