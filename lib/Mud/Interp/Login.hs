{-# LANGUAGE LambdaCase, MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, RecordWildCards, ViewPatterns #-}

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
import Control.Lens (_1, _2, at)
import Control.Lens.Operators ((%~), (&), (.~), (^.))
import Control.Monad ((>=>), guard, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (orM)
import Data.Functor ((<$>))
import Data.Ix (inRange)
import Data.List (delete, intersperse)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Any(..), mempty)
import Network (HostName)
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (foldrWithKey)
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
  | otherwise                             = helper |$| modifyState >=> \case
    Left  (Just msg) -> promptRetryName mq msg
    Left  Nothing    -> mIf (orM . map (getAny <$>) $ [ checkProfanitiesDict i mq cn
                                                      , checkPropNamesDict     mq cn
                                                      , checkWordsDict         mq cn ])
                            (return ())
                            nextPrompt
    Right (originId, oldSing) -> getState >>= \ms -> let cols = getColumns i ms in do
      greet cols
      handleLogin ActionParams { plaId = i, plaMsgQueue = mq, plaCols = cols, args = [] }
      logPla    "interpName" i $ "logged on from " <> T.pack (getHostName i ms) <> "."
      logNotice "interpName" . T.concat $ [ dblQuote oldSing
                                          , " has logged on as "
                                          , cn'
                                          , ". Id "
                                          , showText originId
                                          , " has been changed to "
                                          , showText i
                                          , "." ]
  where
    illegalChars = [ '!' .. '@' ] ++ [ '[' .. '`' ] ++ [ '{' .. '~' ]
    helper ms    =
        let sorted  = IM.foldrWithKey (\pi pla acc -> acc & if isLoggedIn pla
                                        then _1 %~ (getSing pi ms       :)
                                        else _2 %~ ((pi, getSing pi ms) :))
                                      ([], [])
                                      (ms^.plaTbl)
            matches = filter ((== cn') . snd) . snd $ sorted
        in if cn' `elem` fst sorted
          then (ms, Left . Just $ cn' <> " is already logged in.")
          else case matches of [(pi, _)] -> logIn i ms (getHostName i ms) pi
                               _         -> (ms, Left Nothing)
    nextPrompt = do
        prompt mq . nlPrefix $ "Your name will be " <> dblQuote (cn' <> ",") <> " is that OK? [yes/no]"
        setInterp i . Just . interpConfirmName $ cn'
    greet cols = wrapSend mq cols . nlPrefix $ if cn' == "Root"
      then let sudoLecture = "HELLO, ROOT! We trust you have received the usual lecture from the local System \
                             \Administrator..."
           in zingColor <> sudoLecture <> dfltColor
      else "Welcome back, " <> cn' <> "!"
interpName _ (ActionParams { plaMsgQueue }) = promptRetryName plaMsgQueue "Your name must be a single word."


promptRetryName :: MsgQueue -> T.Text -> MudStack ()
promptRetryName mq msg = do
    send mq . nlPrefix $ msg |!| nl msg
    prompt mq "Let's try this again. By what name are you known?"


logIn :: Id -> MudState -> HostName -> Id -> (MudState, Either (Maybe T.Text) (Id, Sing))
logIn newId ms host originId = (peepNewId . movePC $ adoptNewId, Right (originId, getSing newId ms))
  where
    movePC ms'  = let newRmId = fromJust . getLastRmId newId $ ms'
                  in ms' & invTbl  .ind iWelcome       %~ (newId    `delete`)
                         & invTbl  .ind iLoggedOff     %~ (originId `delete`)
                         & invTbl  .ind newRmId        %~ (sortInv ms' . (++ [newId]))
                         & pcTbl   .ind newId.rmId     .~ newRmId
                         & plaTbl  .ind newId.lastRmId .~ Nothing
    adoptNewId  =    ms  & coinsTbl.ind newId          .~ getCoins   originId ms
                         & coinsTbl.at  originId       .~ Nothing
                         & entTbl  .ind newId          .~ (getEnt    originId ms & entId .~ newId)
                         & entTbl  .at  originId       .~ Nothing
                         & eqTbl   .ind newId          .~ getEqMap   originId ms
                         & eqTbl   .at  originId       .~ Nothing
                         & invTbl  .ind newId          .~ getInv     originId ms
                         & invTbl  .at  originId       .~ Nothing
                         & mobTbl  .ind newId          .~ getMob     originId ms
                         & mobTbl  .at  originId       .~ Nothing
                         & pcTbl   .ind newId          .~ getPC      originId ms
                         & pcTbl   .at  originId       .~ Nothing
                         & plaTbl  .ind newId          .~ (getPla    originId ms & hostName .~ host)
                         & plaTbl  .ind newId.peepers  .~ getPeepers originId ms
                         & plaTbl  .at  originId       .~ Nothing
                         & typeTbl .at  originId       .~ Nothing
    peepNewId ms'@(getPeepers newId -> peeperIds) =
        let replaceId = (newId :) . (originId `delete`)
        in ms' & plaTbl %~ flip (foldr (\peeperId -> ind peeperId.peeping %~ replaceId)) peeperIds


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
  Just True -> helper |$| modifyState >=> \(getPla i -> p, oldSing) -> do
      send mq . nl $ ""
      handleLogin ActionParams { plaId = i, plaMsgQueue = mq, plaCols = cols, args = [] }
      logPla    "interpConfirmName" i $ "new character logged on from " <> T.pack (p^.hostName) <> "."
      logNotice "interpConfirmName"   $ dblQuote oldSing <> " has logged on as " <> s <> " (new character)."
  Just False -> promptRetryName  mq "" >> setInterp i (Just interpName)
  Nothing    -> promptRetryYesNo mq
  where
    helper ms = let ms'  = ms  & entTbl.ind i.sing   .~ s
                               & invTbl.ind iWelcome %~ (i `delete`)
                               & pcTbl .ind i.rmId   .~ iCentral
                               & plaTbl.ind i.interp .~ Nothing
                    ms'' = ms' & invTbl.ind iCentral %~ (sortInv ms' . (++ [i]))
                in (ms'', (ms'', getSing i ms))
interpConfirmName _ _ (ActionParams { plaMsgQueue }) = promptRetryYesNo plaMsgQueue


yesNo :: T.Text -> Maybe Bool
yesNo (T.toLower -> a) = guard (not . T.null $ a) >> helper
  where
    helper | a `T.isPrefixOf` "yes" = return True
           | a `T.isPrefixOf` "no"  = return False
           | otherwise              = Nothing


handleLogin :: ActionParams -> MudStack ()
handleLogin params@(ActionParams { .. }) = do
    showMotd plaMsgQueue plaCols
    (ms@(getSing plaId -> s), p) <- showRetainedMsgs
    look params
    prompt plaMsgQueue dfltPrompt
    notifyArrival ms s
    when (getPlaFlag IsAdmin p) . stopInacTimer plaId $ plaMsgQueue
    initPlaLog plaId s
  where
    showRetainedMsgs = helper |$| modifyState >=> \(ms, msgs, p) -> do
        unless (null msgs) $ do
            send plaMsgQueue adminTellColor
            multiWrapSend plaMsgQueue plaCols . intersperse "" $ msgs
            send plaMsgQueue dfltColor
            logPla "handleLogin showRetainedMsgs" plaId "Showed retained messages."
        return (ms, p)
    helper ms = let p   = getPla plaId ms
                    p'  = p  & retainedMsgs     .~ []
                    ms' = ms & plaTbl.ind plaId .~ p'
                in (ms', (ms', p^.retainedMsgs, p'))
    notifyArrival ms s = do
        bcastOtherAdmins plaId $ s <> " has logged on."
        bcastOthersInRm  plaId . nlnl $ mkSerializedNonStdDesig plaId ms s A <> " slowly materializes out of thin air."
    stopInacTimer i mq = do
        liftIO . atomically . writeTQueue mq $ InacStop
        logPla "handleLogin stopInacTimer" i "stopping the inactivity timer."


promptRetryYesNo :: MsgQueue -> MudStack ()
promptRetryYesNo mq = prompt mq . T.concat $ [ "Please answer ", dblQuote "yes", " or ", dblQuote "no", "." ]
