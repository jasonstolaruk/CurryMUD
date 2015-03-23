{-# LANGUAGE LambdaCase, MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.Login (interpName) where

import Mud.Cmds.Pla
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Set
import Mud.Misc.ANSI
import Mud.Misc.Logging hiding (logNotice, logPla)
import Mud.TheWorld.Ids
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception.Lifted (try)
import Control.Lens (at)
import Control.Lens.Operators ((&), (?~), (.~), (^.))
import Control.Monad ((>=>), guard, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.IntMap.Lazy ((!))
import Data.Ix (inRange)
import Data.List (delete)
import Data.Monoid ((<>), Any(..), mempty)
import Network (HostName)
import qualified Data.Set as S (fromList, member)
import qualified Data.Text as T
import qualified Data.Text.IO as T (appendFile, readFile)


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Interp.Login"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Interp.Login"


-- ==================================================


interpName :: Interp
interpName (T.toLower -> cn@(capitalize -> cn')) (NoArgs' i mq)
  | not . inRange (3, 12) . T.length $ cn = promptRetryName mq "Your name must be between three and twelve characters \
                                                               \long."
  | T.any (`elem` illegalChars) cn        = promptRetryName mq "Your name cannot include any numbers or symbols."
  | otherwise                             = doWhileFalse [ checkProfanitiesDict i mq cn
                                                         , checkPropNamesDict     mq cn
                                                         , checkWordsDict         mq cn ] nextPrompt
  where
    illegalChars = [ '!' .. '@' ] ++ [ '[' .. '`' ] ++ [ '{' .. '~' ]
    doWhileFalse :: [MudStack Any] -> MudStack () -> MudStack () -- TODO: Ok? Rename? Refactor?
    doWhileFalse []     final = final
    doWhileFalse (a:as) final = a >>= (`unless` doWhileFalse as final) . getAny
    nextPrompt = do
        prompt mq . nlPrefix $ "Your name will be " <> dblQuote (cn' <> ",") <> " is that OK? [yes/no]"
        setInterp i . Just . interpConfirmName $ cn'
interpName _ (ActionParams { plaMsgQueue }) = promptRetryName plaMsgQueue "Your name must be a single word."


promptRetryName :: MsgQueue -> T.Text -> MudStack ()
promptRetryName mq msg = do
    send mq . nlPrefix $ msg |!| nl msg
    prompt mq "Let's try this again. By what name are you known?"


checkProfanitiesDict :: Id -> MsgQueue -> CmdName -> MudStack Any
checkProfanitiesDict i mq cn = checkNameHelper (Just profanitiesFile) "checkProfanitiesDict" sorry cn
  where
    sorry = getState >>= \ms -> do
        let s  = parensQuote . getSing i $ ms
            hn = getHostName i ms
        send mq . nlPrefix . nl $ bootMsgColor                                                                     <>
                                  "Nice try. Your IP address has been logged. Keep this up and you'll get banned." <>
                                  dfltColor
        sendMsgBoot mq . Just $ "Come back when you're ready to act like an adult!"
        logProfanity cn hn
        logNotice "checkProfanitiesDict" . T.concat $ [ "booting player ", showText i, " ", s, " due to profanity." ]


checkNameHelper :: Maybe FilePath -> T.Text -> MudStack () -> CmdName -> MudStack Any
checkNameHelper Nothing     _       _     _  = return mempty
checkNameHelper (Just file) funName sorry cn = (liftIO . T.readFile $ file) |$| try >=> either
                                                   (emptied . fileIOExHandler funName)
                                                   helper
  where
    helper (S.fromList . T.lines . T.toLower -> set) = let isNG = cn `S.member` set
                                                       in when isNG sorry >> (return . Any $ isNG)


logProfanity :: CmdName -> HostName -> MudStack ()
logProfanity cn (T.pack -> hn) =
    liftIO (helper =<< mkTimestamp |$| try) >>= eitherRet (fileIOExHandler "logProfanity")
  where
    helper ts = T.appendFile profanityLogFile . T.concat $ [ ts, " ", hn, " ", cn ]


checkPropNamesDict :: MsgQueue -> CmdName -> MudStack Any
checkPropNamesDict mq = checkNameHelper propNamesFile "checkPropNamesDict" sorry
  where
    sorry = promptRetryName mq "Your name cannot be a real-world proper name. Please choose an original fantasy name."


checkWordsDict :: MsgQueue -> CmdName -> MudStack Any
checkWordsDict mq = checkNameHelper wordsFile "checkWordsDict" sorry
  where
    sorry = promptRetryName mq "Your name cannot be an English word. Please choose an original fantasy name."


interpConfirmName :: Sing -> Interp
interpConfirmName s cn (NoArgs i mq cols) = case yesNo cn of
  Just True -> helper |$| modifyState >=> \(ms@(getPla i -> p), oldSing) -> do
      send mq . nl $ ""
      showMotd mq cols
      look ActionParams { plaId = i, plaMsgQueue = mq, plaCols = cols, args = [] }
      prompt mq dfltPrompt
      notifyArrival i ms
      when (getPlaFlag IsAdmin p) . stopInacTimer i $ mq
      initPlaLog i s
      logPla    "interpConfirmName" i $ "new player logged on from " <> T.pack (p^.hostName) <> "."
      logNotice "interpConfirmName"   $ dblQuote oldSing <> " has logged on as " <> s <> "."
  Just False -> promptRetryName  mq "" >> setInterp i (Just interpName)
  Nothing    -> promptRetryYesNo mq
  where
    helper ms = let et   = ms^.entTbl -- TODO: Can we make this prettier?
                    it   = ms^.invTbl
                    pct  = ms^.pcTbl
                    plat = ms^.plaTbl

                    e        = et ! i
                    oldSing  = e^.sing
                    et'      = et & at i ?~ (e & sing .~ s)

                    originIs = i `delete` (it ! iWelcome)
                    destIs   = sortInv ms $ it ! iCentral ++ [i]
                    it'      = it & at iWelcome ?~ originIs & at iCentral ?~ destIs

                    pc       = pct ! i & rmId .~ iCentral
                    pct'     = pct & at i ?~ pc

                    pla      = setPlaFlag IsAdmin (T.head s == 'Z') (plat ! i) & interp .~ Nothing
                    plat'    = plat & at i ?~ pla

                    ms'      = ms & entTbl .~ et' & invTbl .~ it' & pcTbl .~ pct' & plaTbl .~ plat'
        in (ms', (ms', oldSing))
interpConfirmName _ _ (ActionParams { plaMsgQueue }) = promptRetryYesNo plaMsgQueue


yesNo :: T.Text -> Maybe Bool
yesNo (T.toLower -> a) = guard (not . T.null $ a) >> helper
  where
    helper | a `T.isPrefixOf` "yes" = return True
           | a `T.isPrefixOf` "no"  = return False
           | otherwise              = Nothing


stopInacTimer :: Id -> MsgQueue -> MudStack ()
stopInacTimer i mq = do
    liftIO . atomically . writeTQueue mq $ InacStop
    logPla "stopInacTimer" i "stopping the inactivity timer."


notifyArrival :: Id -> MudState -> MudStack ()
notifyArrival i ms = let s = getSing i ms in do
    bcastOtherAdmins i $ s <> " has logged on."
    bcastOthersInRm i . nlnl $ mkSerializedNonStdDesig i ms s A <> " has arrived in the game."


promptRetryYesNo :: MsgQueue -> MudStack ()
promptRetryYesNo mq = prompt mq . T.concat $ [ "Please answer ", dblQuote "yes", " or ", dblQuote "no", "." ]
