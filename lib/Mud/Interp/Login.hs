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
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.List
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Arrow (first)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception.Lifted (try)
import Control.Lens (_1, _2, at, views)
import Control.Lens.Operators ((%~), (&), (.~), (^.))
import Control.Monad ((>=>), guard, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (orM)
import Data.Ix (inRange)
import Data.List (delete, intersperse, partition)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Any(..))
import Data.Time (UTCTime)
import Network (HostName)
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (foldr, foldrWithKey)
import qualified Data.Set as S (Set, empty, fromList, insert, member)
import qualified Data.Text as T
import qualified Data.Text.IO as T (appendFile, readFile)


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Interp.Login"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Interp.Login"


-- ==================================================


interpName :: Interp
interpName (T.toLower -> cn@(capitalize -> cn')) p@(NoArgs' i mq)
  | not . inRange (minNameLen, maxNameLen) . T.length $ cn =
      promptRetryName mq . T.concat $ [ "Your name must be between "
                                      , minNameLenTxt
                                      , " and "
                                      , maxNameLenTxt
                                      , " characters long." ]
  | T.any (`elem` illegalChars) cn = promptRetryName mq "Your name cannot include any numbers or symbols."
  | otherwise                      = helper |&| modifyState >=> \case
    (_,  Left  (Just msg)) -> promptRetryName mq msg
    (ms, Left  Nothing   ) -> mIf (orM . map (getAny <$>) $ [ checkProfanitiesDict i  mq cn
                                                            , checkIllegalNames    ms mq cn
                                                            , checkPropNamesDict      mq cn
                                                            , checkWordsDict          mq cn ])
                                  unit
                                  nextPrompt
    (ms, Right (originId, oldSing)) -> let cols = getColumns i ms in do
        greet cols
        handleLogin p { args = [] }
        logPla    "interpName" i $ "logged in from " <> T.pack (getCurrHostName i ms) <> "."
        logNotice "interpName" . T.concat $ [ dblQuote oldSing
                                            , " has logged in as "
                                            , cn'
                                            , ". Id "
                                            , showText originId
                                            , " has been changed to "
                                            , showText i
                                            , "." ]
  where
    illegalChars = [ '!' .. '@' ] ++ [ '[' .. '`' ] ++ [ '{' .. '~' ]
    helper ms    =
        let newPla  = getPla i ms
            sorted  = IM.foldrWithKey (\pi pla acc -> acc & if isLoggedIn pla
                                        then _1 %~ (     getSing pi ms  :)
                                        else _2 %~ ((pi, getSing pi ms) :))
                                      ([], [])
                                      (ms^.plaTbl)
            matches = filter ((== cn') . snd) . snd $ sorted
        in if cn' `elem` fst sorted
          then (ms, (ms, Left . Just $ cn' <> " is already logged in."))
          else case matches of [(pi, _)] -> logIn i ms (newPla^.currHostName) (newPla^.connectTime) pi
                               _         -> (ms, (ms, Left Nothing))
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


logIn :: Id -> MudState -> HostName -> Maybe UTCTime -> Id -> (MudState, (MudState, Either (Maybe T.Text) (Id, Sing)))
logIn newId ms newHost newTime originId = let ms' = peepNewId . movePC $ adoptNewId
                                          in (ms', (ms', Right (originId, getSing newId ms')))
  where
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
                         & plaTbl  .ind newId          .~ (getPla    originId ms & currHostName .~ newHost
                                                                                 & connectTime  .~ newTime)
                         & plaTbl  .ind newId.peepers  .~ getPeepers originId ms
                         & plaTbl  .at  originId       .~ Nothing
                         & typeTbl .at  originId       .~ Nothing
    movePC ms'  = let newRmId = fromJust . getLastRmId newId $ ms'
                  in ms' & invTbl  .ind iWelcome       %~ (newId    `delete`)
                         & invTbl  .ind iLoggedOut     %~ (originId `delete`)
                         & invTbl  .ind newRmId        %~ (sortInv ms' . (++ pure newId))
                         & pcTbl   .ind newId.rmId     .~ newRmId
                         & plaTbl  .ind newId.lastRmId .~ Nothing
    peepNewId ms'@(getPeepers newId -> peeperIds) =
        let replaceId = (newId :) . (originId `delete`)
        in ms' & plaTbl %~ flip (foldr (\peeperId -> ind peeperId.peeping %~ replaceId)) peeperIds


checkProfanitiesDict :: Id -> MsgQueue -> CmdName -> MudStack Any
checkProfanitiesDict i mq cn = checkNameHelper (Just profanitiesFile) "checkProfanitiesDict" sorry cn
  where
    sorry = getState >>= \ms -> do
        let s  = parensQuote . getSing i $ ms
            hn = getCurrHostName i ms
        send mq . nlPrefix . nl $ bootMsgColor                                                                     <>
                                  "Nice try. Your IP address has been logged. Keep this up and you'll get banned." <>
                                  dfltColor
        sendMsgBoot mq . Just $ "Come back when you're ready to act like an adult!"
        logProfanity cn hn
        let logMsg = T.concat [ "booting player ", showText i, " ", s, " due to profanity." ]
        logNotice "checkProfanitiesDict sorry" logMsg


checkNameHelper :: Maybe FilePath -> T.Text -> MudStack () -> CmdName -> MudStack Any
checkNameHelper Nothing     _       _     _  = return mempty
checkNameHelper (Just file) funName sorry cn = (liftIO . T.readFile $ file) |&| try >=> either
                                                   (emptied . fileIOExHandler funName)
                                                   (checkSet cn sorry . S.fromList . T.lines . T.toLower)


checkSet :: CmdName -> MudStack () -> S.Set T.Text -> MudStack Any
checkSet cn sorry set = let isNG = cn `S.member` set in when isNG sorry >> (return . Any $ isNG)


logProfanity :: CmdName -> HostName -> MudStack ()
logProfanity cn (T.pack -> hn) =
    liftIO (helper =<< mkTimestamp |&| try) >>= eitherRet (fileIOExHandler "logProfanity")
  where
    helper ts = T.appendFile profanitiesDbFile . T.concat $ [ ts, " ", hn, " ", cn ]


checkIllegalNames :: MudState -> MsgQueue -> CmdName -> MudStack Any
checkIllegalNames ms mq cn = checkSet cn sorry . insertEntNames $ insertRaceNames
  where
    insertRaceNames = foldr helper S.empty (allValues :: [Race])
      where
        helper (uncapitalize . showText -> r) acc = foldr S.insert acc . (r :) . map (`T.cons` r) $ "mf"
    insertEntNames = views entTbl (flip (IM.foldr (views entName helper))) ms
      where
        helper Nothing  = id
        helper (Just n) = S.insert n
    sorry = promptRetryName mq "Sorry, but that name is already taken."


checkPropNamesDict :: MsgQueue -> CmdName -> MudStack Any
checkPropNamesDict mq = checkNameHelper propNamesFile "checkPropNamesDict" sorry
  where
    sorry = promptRetryName mq "Your name cannot be a real-world proper name. Please choose an original fantasy name."


checkWordsDict :: MsgQueue -> CmdName -> MudStack Any
checkWordsDict mq = checkNameHelper wordsFile "checkWordsDict" sorry
  where
    sorry = promptRetryName mq "Your name cannot be an English word. Please choose an original fantasy name."


interpConfirmName :: Sing -> Interp
interpConfirmName s cn params@(NoArgs' i mq) = case yesNo cn of
  Just True -> helper |&| modifyState >=> \(getPla i -> p, oldSing) -> do
      send mq . nl $ ""
      handleLogin params { args = [] }
      logPla    "interpConfirmName" i $ "new character logged in from " <> T.pack (p^.currHostName) <> "."
      logNotice "interpConfirmName"   $ dblQuote oldSing <> " has logged in as " <> s <> " (new character)."
  Just False -> promptRetryName  mq "" >> setInterp i (Just interpName)
  Nothing    -> promptRetryYesNo mq
  where
    helper ms = let ms'  = ms  & entTbl.ind i.sing   .~ s
                               & invTbl.ind iWelcome %~ (i `delete`)
                               & pcTbl .ind i.rmId   .~ iCentral
                               & plaTbl.ind i.interp .~ Nothing
                    ms'' = ms' & invTbl.ind iCentral %~ (sortInv ms' . (++ pure i))
                in (ms'', (ms'', getSing i ms))
interpConfirmName _ _ (ActionParams { plaMsgQueue }) = promptRetryYesNo plaMsgQueue


yesNo :: T.Text -> Maybe Bool
yesNo (T.toLower -> a) = guard (()!# a) >> helper
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
    showRetainedMsgs = helper |&| modifyState >=> \(ms, msgs, p) -> do
        unless (()# msgs) $ do
            let (fromPpl, others) = first (map T.tail) . partition ((== fromPersonMarker) . T.head) $ msgs
            others  |#| multiWrapSend plaMsgQueue plaCols . intersperse ""
            fromPpl |#| let m   = "message" <> case fromPpl of [_] -> ""
                                                               _   -> "s"
                            msg = "You missed the following " <> m <> " while you were away:"
                        in multiWrapSend plaMsgQueue plaCols . (msg :)
            logPla "handleLogin showRetainedMsgs" plaId "Showed retained messages."
        return (ms, p)
    helper ms = let p   = getPla plaId ms
                    p'  = p  & retainedMsgs     .~ []
                    ms' = ms & plaTbl.ind plaId .~ p'
                in (ms', (ms', p^.retainedMsgs, p'))
    notifyArrival ms s = do
        bcastOtherAdmins plaId $ s <> " has logged in."
        bcastOthersInRm  plaId . nlnl $ mkSerializedNonStdDesig plaId ms s A DoCap <> " slowly materializes out of \
                                                                                      \thin air."
    stopInacTimer i mq = do
        liftIO . atomically . writeTQueue mq $ InacStop
        logPla "handleLogin stopInacTimer" i "stopping the inactivity timer."


promptRetryYesNo :: MsgQueue -> MudStack ()
promptRetryYesNo mq = prompt mq . T.concat $ [ "Please answer ", dblQuote "yes", " or ", dblQuote "no", "." ]
