{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.Login (interpName) where

import Mud.ANSI
import Mud.Cmds.Pla
import Mud.Cmds.Util.Misc
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.State
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Pla
import Mud.Data.State.Util.STM
import Mud.Logging hiding (logNotice, logPla)
import Mud.TheWorld.Ids
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Quoting
import qualified Mud.Logging as L (logNotice, logPla)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Applicative ((<$>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception.Lifted (try)
import Control.Lens (at)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (?~), (.~), (^.))
import Control.Monad (guard, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)
import Data.IntMap.Lazy ((!))
import Data.List (delete, sort)
import Data.Monoid ((<>))
import Network (HostName)
import System.Directory (doesFileExist)
import qualified Data.IntMap.Lazy as IM (IntMap)
import qualified Data.Set as S (member)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile, writeFile)


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Interp.Login"


-----


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Interp.Login"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Interp.Login"


-- ==================================================


interpName :: Interp
interpName (T.toLower -> cn) (NoArgs' i mq)
  | l <- T.length cn, l < 3 || l > 12 = promptRetryName mq "Your name must be between three and twelve characters long."
  | T.any (`elem` illegalChars) cn    = promptRetryName mq "Your name cannot include any numbers or symbols."
  | otherwise                         = do
      isProfane <- checkProfanity cn i mq
      unless isProfane $ do
          isPropName <- checkPropNamesDict cn mq
          unless isPropName $ do
              isWord <- checkWordsDict cn mq
              unless isWord $ let cn' = capitalize cn in do
                  prompt mq . nl' $ "Your name will be " <> (dblQuote $ cn' <> ",") <> " is that OK? [yes/no]"
                  void . modifyPla i interp . Just $ interpConfirmName cn'
  where
    illegalChars = [ '!' .. '@' ] ++ [ '[' .. '`' ] ++ [ '{' .. '~' ]
interpName _  (WithArgs _ mq _ _) = promptRetryName mq "Your name must be a single word."
interpName cn p                   = patternMatchFail "interpName" [ cn, showText p ]


promptRetryName :: MsgQueue -> T.Text -> MudStack ()
promptRetryName mq msg = do
    send mq . nl' $ if not . T.null $ msg then nl msg else ""
    prompt mq "Let's try this again. By what name are you known?"


checkProfanity :: CmdName -> Id -> MsgQueue -> MudStack Bool
checkProfanity cn i mq =
    try (liftIO . T.readFile $ profanitiesFile) >>= either (\e -> fileIOExHandler "checkProfanity" e >> return False)
                                                           helper
  where
    helper profanities = if cn `notElem` T.lines profanities
      then return False
      else do
          (parensQuote -> s) <- getEntSing i
          logNotice "checkProfanity" . T.concat $ [ "booting player ", showText i, " ", s, " due to profanity." ]
          views hostName (logProfanity cn) =<< getPla i
          send mq . nl' $ bootMsgColor                                                                     <>
                          "Nice try. Your IP address has been logged. Keep this up and you'll get banned." <>
                          dfltColor
          sendMsgBoot mq . Just $ "Come back when you're ready to act like an adult!"
          return True


logProfanity :: CmdName -> HostName -> MudStack ()
logProfanity cn (T.pack -> hn) =
    liftIO mkTimestamp >>= try . liftIO . helper >>= eitherRet (fileIOExHandler "logProfanity")
  where
    helper ts = let newEntry = T.concat [ ts, " ", hn, " ", cn ]
                in getLogConts >>= T.writeFile profanityLogFile . T.unlines . sort . (newEntry :)
    getLogConts = doesFileExist profanityLogFile >>= \case
      True  -> T.lines <$> T.readFile profanityLogFile
      False -> return []


checkPropNamesDict :: CmdName -> MsgQueue -> MudStack Bool
checkPropNamesDict cn mq = gets (view (nonWorldState.dicts.propNamesDict)) >>= \case
  Nothing                      -> return False
  Just pnd | cn `S.member` pnd -> do
      promptRetryName mq "Your name cannot be a real-world proper name. Please choose an original fantasy name."
      return True
  _                            -> return False


checkWordsDict :: CmdName -> MsgQueue -> MudStack Bool
checkWordsDict cn mq = gets (view (nonWorldState.dicts.wordsDict)) >>= \case
  Nothing                    -> return False
  Just wd | cn `S.member` wd -> do
      promptRetryName mq "Your name cannot be an English word. Please choose an original fantasy name."
      return True
  _                          -> return False


interpConfirmName :: Sing -> Interp
interpConfirmName s cn (NoArgs i mq cols) = case yesNo cn of
  Just True -> do
      oldSing <- onWS $ \(t, ws) ->
          let e       = (ws^.entTbl) ! i
              oldSing = e^.sing
              e'      = e & sing .~ s
          in putTMVar t (ws & entTbl.at i ?~ e') >> return oldSing
      (pt, p) <- onNWS plaTblTMVar $ \(ptTMVar, pt) ->
          let p   = pt ! i
              p'  = setFlag IsAdmin (T.head s == 'Z') p & interp .~ Nothing
              pt' = pt & at i ?~ p'
          in putTMVar ptTMVar pt' >> return (pt, p')
      logNotice "interpConfirmName" $ dblQuote oldSing <> " has logged on as " <> s <> "."
      initPlaLog i s
      logPla "interpConfirmName" i $ "new player logged on from " <> (T.pack $ p^.hostName) <> "."
      when (getFlag IsAdmin p) $ stopInacTimer i mq
      movePC
      notifyArrival i pt
      send mq . nl $ ""
      showMotd mq cols
      look ActionParams { plaId       = i
                        , plaMsgQueue = mq
                        , plaCols     = cols
                        , args        = [] }
      prompt mq dfltPrompt
  Just False -> promptRetryName mq "" >> (void . modifyPla i interp . Just $ interpName)
  Nothing    -> promptRetryYesNo mq
  where
    movePC = onWS $ \(t, ws) ->
        let p         = (ws^.pcTbl)  ! i
            p'        = p & rmId .~ iHill
            originIs  = (ws^.invTbl) ! iWelcome
            originIs' = i `delete` originIs
            destIs    = (ws^.invTbl) ! iHill
            destIs'   = sortInv ws $ destIs ++ [i]
        in putTMVar t (ws & pcTbl.at  i        ?~ p'
                          & invTbl.at iWelcome ?~ originIs'
                          & invTbl.at iHill    ?~ destIs')
interpConfirmName _ _  (WithArgs _ mq _ _) = promptRetryYesNo mq
interpConfirmName s cn p                   = patternMatchFail "interpConfirmName" [ s, cn, showText p ]


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


notifyArrival :: Id -> IM.IntMap Pla -> MudStack ()
notifyArrival i pt = getEntSing' i >>= \(ws, s) -> do
    bcastAdmins pt $ s <> " has logged on."
    bcastOthersInRm i . nlnl $ mkSerializedNonStdDesig i ws s A <> " has arrived in the game."


promptRetryYesNo :: MsgQueue -> MudStack ()
promptRetryYesNo mq = prompt mq . T.concat $ [ "Please answer ", dblQuote "yes", " or ", dblQuote "no", "." ]
