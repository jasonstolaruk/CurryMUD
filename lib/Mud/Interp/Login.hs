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
import Mud.Misc.ANSI
import Mud.Misc.Logging hiding (logNotice, logPla)
import Mud.TheWorld.Ids
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Exception.Lifted (try)
import Control.Lens (at)
import Control.Lens.Getter (view)
import Control.Lens.Operators ((&), (?~), (.~), (^.))
import Control.Monad ((>=>), guard, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.IntMap.Lazy ((!))
import Data.List (delete, sort)
import Data.Monoid ((<>))
import Network (HostName)
import System.Directory (doesFileExist)
import qualified Data.Set as S (fromList, member, notMember)
import qualified Data.Text as T
import qualified Data.Text.IO as T (appendFile, readFile, writeFile)


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Interp.Login"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Interp.Login"


-- ==================================================


interpName :: Interp
interpName (T.toLower -> cn@(capitalize -> cn')) (NoArgs' i mq)
  | l <- T.length cn, l < 3 || l > 12 = promptRetryName mq "Your name must be between three and twelve characters long."
  | T.any (`elem` illegalChars) cn    = promptRetryName mq "Your name cannot include any numbers or symbols."
  | otherwise                         = f [ checkProfanitiesDict cn i mq
                                          , checkPropNamesDict   cn   mq
                                          , checkWordsDict       cn   mq ] $ do
                                            prompt mq . nlPrefix $ "Your name will be " <> dblQuote (cn' <> ",") <>
                                                                   " is that OK? [yes/no]"
                                            modifyState helper
{-
      isProfane <- checkProfanity cn i mq
      unless isProfane $ do
          isPropName <- checkPropNamesDict cn mq
          unless isPropName $ do
              isWord <- checkWordsDict cn mq
              unless isWord $ do
                  prompt mq . nlPrefix $ "Your name will be " <> dblQuote (cn' <> ",") <> " is that OK? [yes/no]"
                  modifyState helper
-}
  where
    illegalChars = [ '!' .. '@' ] ++ [ '[' .. '`' ] ++ [ '{' .. '~' ]
    f :: [MudStack Bool] -> MudStack () -> MudStack () -- TODO: Ok? Rename? Refactor?
    f []     b = b
    f [a]    b = a >>= flip unless b
    f (a:as) _ = a >>= flip unless (f as)
    helper ms  = let p = getPla i ms in (ms & plaTbl.at i .~ (p & interp .~ (Just . interpConfirmName $ cn')), ())
interpName _ (ActionParams { plaMsgQueue }) = promptRetryName plaMsgQueue "Your name must be a single word."


promptRetryName :: MsgQueue -> T.Text -> MudStack ()
promptRetryName mq msg = do
    send mq . nlPrefix $ msg |!| nl msg
    prompt mq "Let's try this again. By what name are you known?"


checkProfanitiesDict :: CmdName -> Id -> MsgQueue -> MudStack Bool
checkProfanitiesDict cn i mq = checkNameHelper profanitiesFile "checkProfanitiesDict" sorry mq cn
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


checkNameHelper :: FilePath -> T.Text -> MudStack () -> MsgQueue -> CmdName -> MudStack Bool
checkNameHelper file funName sorry mq cn = (liftIO . T.readFile $ file) |$| try >=> either
    (\e -> fileIOExHandler funName e >> return False) -- TODO: Use "emptied". "Any"?
    helper
  where
    helper (S.fromList . T.lines -> set) = let isNG = cn `S.member` set in when isNG sorry >> return isNG


logProfanity :: CmdName -> HostName -> MudStack ()
logProfanity cn (T.pack -> hn) =
    liftIO (helper =<< mkTimestamp |$| try) >>= eitherRet (fileIOExHandler "logProfanity")
  where
    helper ts = T.appendFile profanityLogFile . T.concat $ [ ts, " ", hn, " ", cn ]


checkPropNamesDict :: MsgQueue -> CmdName -> MudStack Bool
checkPropNamesDict mq = checkNameHelper propNamesFile "checkPropNamesDict" sorry mq
  where
    sorry = promptRetryName mq "Your name cannot be a real-world proper name. Please choose an original fantasy name."


checkWordsDict :: MsgQueue -> CmdName -> MudStack Bool
checkWordsDict mq = checkNameHelper wordsFile "checkWordsDict" sorry mq
  where
    sorry = promptRetryName mq "Your name cannot be an English word. Please choose an original fantasy name."


interpConfirmName :: Sing -> Interp
interpConfirmName s cn (NoArgs i mq cols) = case yesNo cn of
  Just True -> ask >>= liftIO . atomically . helperSTM >>= \(et, it, mt, mqt, oldSing, pcTbl, p, plaTbl, tt) -> do
      logNotice "interpConfirmName" $ dblQuote oldSing <> " has logged on as " <> s <> "."
      initPlaLog i s
      logPla "interpConfirmName" i $ "new player logged on from " <> T.pack (p^.hostName) <> "."
      when (getPlaFlag IsAdmin p) . stopInacTimer i $ mq
      notifyArrival i et it mt mqt pcTbl plaTbl tt
      send mq . nl $ ""
      showMotd mq cols
      look ActionParams { plaId       = i
                        , plaMsgQueue = mq
                        , plaCols     = cols
                        , args        = [] }
      prompt mq dfltPrompt
  Just False -> promptRetryName mq "" >> ask >>= \md -> liftIO . atomically $ do
      pt <- readTVar $ md^.plaTblTVar
      let p = pt ! i & interp .~ Just interpName
      writeTVar (md^.plaTblTVar) $ pt & at i ?~ p
  Nothing -> promptRetryYesNo mq
  where
    helper ms = let et   = ms^.entTbl
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
                    pct'     = pcTbl & at i ?~ pc

                    pla      = setPlaFlag IsAdmin (T.head s == 'Z') (plaTbl ! i) & interp .~ Nothing
                    plat'    = plaTbl & at i ?~ pla
        in (ms & entTbl .~ et' & invTbl .~ it' & pcTbl .~ pct' & plaTbl .~ plat', ())
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
    bcastAdmins $ s <> " has logged on."
    bcastOthersInRm i . nlnl $ mkSerializedNonStdDesig i ms s A <> " has arrived in the game."


promptRetryYesNo :: MsgQueue -> MudStack ()
promptRetryYesNo mq = prompt mq . T.concat $ [ "Please answer ", dblQuote "yes", " or ", dblQuote "no", "." ]
