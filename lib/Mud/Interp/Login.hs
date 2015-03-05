{-# LANGUAGE LambdaCase, MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.Login (interpName) where

import Mud.Cmds.Pla
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.State
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
import qualified Data.Set as S (member)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile, writeFile)


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Interp.Login"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Interp.Login"


-- ==================================================


interpName :: Interp
interpName (T.toLower -> cn@(capitalize -> cn')) (NoArgs' i mq)
  | l <- T.length cn, l < 3 || l > 12 = promptRetryName mq "Your name must be between three and twelve characters long."
  | T.any (`elem` illegalChars) cn    = promptRetryName mq "Your name cannot include any numbers or symbols."
  | otherwise                         = do
      isProfane <- checkProfanity cn i mq
      unless isProfane $ do
          isPropName <- checkPropNamesDict cn mq
          unless isPropName $ do
              isWord <- checkWordsDict cn mq
              unless isWord $ do
                  prompt mq . nlPrefix $ "Your name will be " <> dblQuote (cn' <> ",") <> " is that OK? [yes/no]"
                  ask >>= liftIO . atomically . helperSTM
  where
    illegalChars = [ '!' .. '@' ] ++ [ '[' .. '`' ] ++ [ '{' .. '~' ]
    helperSTM md = do
        pt <- readTVar $ md^.plaTblTVar
        let p = pt ! i & interp .~ (Just . interpConfirmName $ cn')
        writeTVar (md^.plaTblTVar) $ pt & at i ?~ p
interpName _ (ActionParams { plaMsgQueue }) = promptRetryName plaMsgQueue "Your name must be a single word."


promptRetryName :: MsgQueue -> T.Text -> MudStack ()
promptRetryName mq msg = do
    send mq . nlPrefix $ msg |!| nl msg
    prompt mq "Let's try this again. By what name are you known?"


checkProfanity :: CmdName -> Id -> MsgQueue -> MudStack Bool
checkProfanity cn i mq =
      (liftIO . T.readFile $ profanitiesFile) |$| try >=> either
          (\e -> fileIOExHandler "checkProfanity" e >> return False)
          helper
  where
    helper profanities = if cn `notElem` T.lines profanities
      then return False
      else ask >>= liftIO . atomically . helperSTM >>= \(parensQuote . view sing . (! i) -> s, view hostName . (! i) -> hn) -> do
          logNotice "checkProfanity" . T.concat $ [ "booting player ", showText i, " ", s, " due to profanity." ]
          logProfanity cn hn
          send mq . nlPrefix . nl $ bootMsgColor                                                                     <>
                                    "Nice try. Your IP address has been logged. Keep this up and you'll get banned." <>
                                    dfltColor
          sendMsgBoot mq . Just $ "Come back when you're ready to act like an adult!"
          return True
    helperSTM md = (,) <$> readTVar (md^.entTblTVar) <*> readTVar (md^.plaTblTVar)


logProfanity :: CmdName -> HostName -> MudStack ()
logProfanity cn (T.pack -> hn) =
    liftIO (mkTimestamp >>= try . helper) >>= eitherRet (fileIOExHandler "logProfanity")
  where
    helper ts = T.writeFile profanityLogFile =<< [ T.unlines . sort $ newEntry : cont
                                                 | cont <- mIf (doesFileExist profanityLogFile)
                                                               (T.lines <$> T.readFile profanityLogFile)
                                                               (return [])
                                                 , let newEntry = T.concat [ ts, " ", hn, " ", cn ] ]


checkPropNamesDict :: CmdName -> MsgQueue -> MudStack Bool
checkPropNamesDict cn mq = ask >>= maybe (return False) helper . view propNamesSet
  where
    helper pns = if cn `S.member` pns
      then do
          promptRetryName mq "Your name cannot be a real-world proper name. Please choose an original fantasy name."
          return True
      else return False


checkWordsDict :: CmdName -> MsgQueue -> MudStack Bool
checkWordsDict cn mq = ask >>= maybe (return False) helper . view wordsSet
  where
    helper ws = if cn `S.member` ws
      then do
          promptRetryName mq "Your name cannot be an English word. Please choose an original fantasy name."
          return True
      else return False


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
  Nothing    -> promptRetryYesNo mq
  where
    helperSTM md = (,,,,,,) <$> readTVar (md^.entTblTVar)
                            <*> readTVar (md^.invTblTVar)
                            <*> readTVar (md^.mobTblTVar)
                            <*> readTVar (md^.msgQueueTblTVar)
                            <*> readTVar (md^.pcTblTVar)
                            <*> readTVar (md^.plaTblTVar)
                            <*> readTVar (md^.typeTblTVar) >>= \(et, it, mt, mqt, pcTbl, plaTbl, tt) ->
        let e        = et ! i
            oldSing  = e^.sing
            et'      = et & at i ?~ (e & sing .~ s)
            originIs = i `delete` (it ! iWelcome)
            destIs   = sortInv et' tt $ it ! iCentral ++ [i]
            it'      = it & at iWelcome ?~ originIs & at iCentral ?~ destIs
            pc        = pcTbl ! i & rmId .~ iCentral
            pcTbl'    = pcTbl & at i ?~ pc
            pla       = setPlaFlag IsAdmin (T.head s == 'Z') (plaTbl ! i) & interp .~ Nothing
            plaTbl'   = plaTbl & at i ?~ pla
        in do
            writeTVar (md^.entTblTVar) et'
            writeTVar (md^.invTblTVar) it'
            writeTVar (md^.pcTblTVar)  pcTbl'
            writeTVar (md^.plaTblTVar) plaTbl'
            return (et', it', mt, mqt, oldSing, pcTbl', pla, plaTbl', tt)
interpConfirmName _ _ (ActionParams { plaMsgQueue }) = promptRetryYesNo plaMsgQueue


yesNo :: T.Text -> Maybe Bool
yesNo (T.toLower -> a) = guard (not . T.null $ a) >> helper
  where
    helper | a `T.isPrefixOf` "yes" = Just True
           | a `T.isPrefixOf` "no"  = Just False
           | otherwise              = Nothing


stopInacTimer :: Id -> MsgQueue -> MudStack ()
stopInacTimer i mq = do
    logPla "stopInacTimer" i "stopping the inactivity timer."
    liftIO . atomically . writeTQueue mq $ InacStop


notifyArrival :: Id -> EntTbl -> InvTbl -> MobTbl -> MsgQueueTbl -> PCTbl -> PlaTbl -> TypeTbl -> MudStack ()
notifyArrival i et it mt mqt pcTbl plaTbl tt = let s = (et ! i)^.sing in do
    bcastAdmins mt mqt pcTbl plaTbl $ s <> " has logged on."
    bcastOthersInRm i it mt mqt pcTbl plaTbl tt . nlnl $ mkSerializedNonStdDesig i mt pcTbl s A <> " has arrived in \
                                                                                                   \the game."


promptRetryYesNo :: MsgQueue -> MudStack ()
promptRetryYesNo mq = prompt mq . T.concat $ [ "Please answer ", dblQuote "yes", " or ", dblQuote "no", "." ]
