{-# LANGUAGE LambdaCase, MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, RecordWildCards, ViewPatterns #-}

module Mud.Interp.Login (interpName) where

import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Msgs.Sorry
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
import Mud.Misc.Database
import Mud.Misc.Logging hiding (logNotice, logPla)
import Mud.TheWorld.AdminZoneIds (iCentral, iLoggedOut, iWelcome)
import Mud.Threads.Regen
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
import Data.Bits (setBit, zeroBits)
import Data.Ix (inRange)
import Data.List (delete, intersperse, partition)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Any(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Network (HostName)
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (foldr, foldrWithKey)
import qualified Data.Set as S (Set, empty, fromList, insert, member)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Interp.Login"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Interp.Login"


-- ==================================================


interpName :: Interp
interpName (T.toLower -> cn@(capitalize -> cn')) p@(NoArgs i mq cols)
  | not . inRange (minNameLen, maxNameLen) . T.length $ cn = promptRetryName mq sorryInterpNameLen
  | T.any (`elem` illegalChars) cn                         = promptRetryName mq sorryInterpNameIllegal
  | otherwise                                              = getSing i <$> getState >>= \oldSing ->
      (withDbExHandler "interpName" . isPlaBanned $ cn') >>= \case
        Nothing          -> wrapSend mq cols dbErrorMsg
        Just (Any True ) -> handleBanned
        Just (Any False) -> handleNotBanned oldSing
  where
    handleBanned = (T.pack . getCurrHostName i <$> getState) >>= \host -> do
        sendMsgBoot mq . Just . sorryInterpNameBanned $ cn'
        let msg  = T.concat [ cn', " has been booted at login ", parensQuote "player is banned", "." ]
        bcastAdmins $ msg <> " Consider also banning host " <> dblQuote host <> "."
        logNotice "interpName" msg
    handleNotBanned oldSing = helper |&| modifyState >=> \case
        (_,  Left  (Just msg)) -> promptRetryName mq msg
        (ms, Left  Nothing   ) -> mIf (orM . map (getAny <$>) $ [ checkProfanitiesDict i  mq cn
                                                                , checkIllegalNames    ms mq cn
                                                                , checkPropNamesDict      mq cn
                                                                , checkWordsDict          mq cn
                                                                , checkRndmNames          mq cn ])
                                      unit
                                      nextPrompt
        (ms, Right originId  ) -> do
            logNotice "interpName" . T.concat $ [ dblQuote oldSing
                                                , " has logged in as "
                                                , cn'
                                                , ". Id "
                                                , showText originId
                                                , " has been changed to "
                                                , showText i
                                                , "." ]
            initPlaLog i cn'
            logPla "interpName handleNotBanned" i $ "logged in from " <> T.pack (getCurrHostName i ms) <> "."
            handleLogin cn' p { args = [] }
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
          then (ms, (ms, Left . Just . sorryInterpNameLoggedIn $ cn'))
          else case matches of [(pi, _)] -> logIn i ms (newPla^.currHostName) (newPla^.connectTime) pi
                               _         -> (ms, (ms, Left Nothing))
    nextPrompt = do
        sendPrompt mq . nlPrefix $ "Your name will be " <> dblQuote (cn' <> ",") <> " is that OK? [yes/no]"
        setInterp i . Just . interpConfirmName $ cn'
interpName _ ActionParams { plaMsgQueue } = promptRetryName plaMsgQueue sorryInterpNameExcessArgs


promptRetryName :: MsgQueue -> Text -> MudStack ()
promptRetryName mq msg =
    (send mq . nlPrefix $ msg |!| nl msg) >> sendPrompt mq "Let's try this again. By what name are you known?"


logIn :: Id -> MudState -> HostName -> Maybe UTCTime -> Id -> (MudState, (MudState, Either (Maybe Text) Id))
logIn newId ms newHost newTime originId = let ms' = peepNewId . movePC $ adoptNewId
                                          in (ms', (ms', Right originId))
  where
    adoptNewId  =    ms  & coinsTbl        .ind newId         .~ getCoins        originId ms
                         & coinsTbl        .at  originId      .~ Nothing
                         & entTbl          .ind newId         .~ (getEnt         originId ms & entId .~ newId)
                         & entTbl          .at  originId      .~ Nothing
                         & eqTbl           .ind newId         .~ getEqMap        originId ms
                         & eqTbl           .at  originId      .~ Nothing
                         & invTbl          .ind newId         .~ getInv          originId ms
                         & invTbl          .at  originId      .~ Nothing
                         & mobTbl          .ind newId         .~ getMob          originId ms
                         & mobTbl          .at  originId      .~ Nothing
                         & pcTbl           .ind newId         .~ getPC           originId ms
                         & pcTbl           .at  originId      .~ Nothing
                         & plaTbl          .ind newId         .~ (getPla         originId ms & currHostName .~ newHost
                                                                                             & connectTime  .~ newTime)
                         & plaTbl          .ind newId.peepers .~ getPeepers      originId ms
                         & plaTbl          .at  originId      .~ Nothing
                         & rndmNamesMstrTbl.ind newId         .~ getRndmNamesTbl originId ms
                         & rndmNamesMstrTbl.at  originId      .~ Nothing
                         & teleLinkMstrTbl .ind newId         .~ getTeleLinkTbl  originId ms
                         & teleLinkMstrTbl .at  originId      .~ Nothing
                         & typeTbl         .at  originId      .~ Nothing
    movePC ms'  = let newRmId = fromJust . getLastRmId newId $ ms'
                  in ms' & invTbl  .ind iWelcome       %~ (newId    `delete`)
                         & invTbl  .ind iLoggedOut     %~ (originId `delete`)
                         & invTbl  .ind newRmId        %~ (sortInv ms' . (++ pure newId))
                         & mobTbl  .ind newId.rmId     .~ newRmId
                         & plaTbl  .ind newId.lastRmId .~ Nothing
    peepNewId ms'@(getPeepers newId -> peeperIds) =
        let replaceId = (newId :) . (originId `delete`)
        in ms' & plaTbl %~ flip (foldr (\peeperId -> ind peeperId.peeping %~ replaceId)) peeperIds


checkProfanitiesDict :: Id -> MsgQueue -> CmdName -> MudStack Any
checkProfanitiesDict i mq cn = checkNameHelper (Just profanitiesFile) "checkProfanitiesDict" sorry cn
  where
    sorry = getState >>= \ms -> do
        let s  = parensQuote . getSing i $ ms
            hn = T.pack . getCurrHostName i $ ms
        send mq . nlPrefix . nl . colorWith bootMsgColor $ sorryInterpNameProfanityLogged
        sendMsgBoot mq . Just $ sorryInterpNameProfanityBoot
        ts <- liftIO mkTimestamp
        let prof = ProfRec ts hn cn
        withDbExHandler_ "checkProfanitiesDict sorry" . insertDbTblProf $ prof
        bcastAdmins $ "Profanity logged: " <> pp prof
        let logMsg = T.concat [ "booting player ", showText i, " ", s, " due to profanity." ]
        logNotice "checkProfanitiesDict sorry" logMsg


checkNameHelper :: Maybe FilePath -> Text -> MudStack () -> CmdName -> MudStack Any
checkNameHelper Nothing     _       _     _  = return mempty
checkNameHelper (Just file) funName sorry cn = (liftIO . T.readFile $ file) |&| try >=> either
                                                   (emptied . fileIOExHandler funName)
                                                   (checkSet cn sorry . S.fromList . T.lines . T.toLower)


checkSet :: CmdName -> MudStack () -> S.Set Text -> MudStack Any
checkSet cn sorry set = let isNG = cn `S.member` set in when isNG sorry >> (return . Any $ isNG)


checkIllegalNames :: MudState -> MsgQueue -> CmdName -> MudStack Any
checkIllegalNames ms mq cn = checkSet cn (promptRetryName mq sorryInterpNameTaken) . insertEntNames $ insertRaceNames
  where
    insertRaceNames = foldr helper S.empty (allValues :: [Race])
      where
        helper (uncapitalize . showText -> r) acc = foldr S.insert acc . (r :) . map (`T.cons` r) $ "mf"
    insertEntNames = views entTbl (flip (IM.foldr (views entName (maybe id S.insert)))) ms


checkPropNamesDict :: MsgQueue -> CmdName -> MudStack Any
checkPropNamesDict mq =
    checkNameHelper propNamesFile "checkPropNamesDict" . promptRetryName mq $ sorryInterpNamePropName


checkWordsDict :: MsgQueue -> CmdName -> MudStack Any
checkWordsDict mq = checkNameHelper wordsFile "checkWordsDict" . promptRetryName mq $ sorryInterpNameDict


checkRndmNames :: MsgQueue -> CmdName -> MudStack Any
checkRndmNames mq = checkNameHelper (Just rndmNamesFile) "checkRndmNames" . promptRetryName mq $ sorryInterpNameTaken


interpConfirmName :: Sing -> Interp
interpConfirmName s cn params@(NoArgs' i mq) = case yesNo cn of
  Just True -> helper |&| modifyState >=> \(ms@(getPla i -> p), oldSing) -> do
      logNotice "interpConfirmName"   . T.concat $ [ dblQuote oldSing
                                                   , " has logged in as "
                                                   , s
                                                   , " "
                                                   , parensQuote "new character"
                                                   , "." ]
      initPlaLog i s
      logPla "interpConfirmName" i $ "new character logged in from " <> views currHostName T.pack p <> "."
      send mq . nl $ ""
      handleLogin s params { args = [] }
      notifyQuestion i ms
  Just False -> promptRetryName  mq "" >> setInterp i (Just interpName)
  Nothing    -> promptRetryYesNo mq
  where
    helper ms = let ms'  = ms  & entTbl.ind i.sing     .~ s
                               & invTbl.ind iWelcome   %~ (i `delete`)
                               & mobTbl.ind i.rmId     .~ iCentral
                               & mobTbl.ind i.interp   .~ Nothing
                               & plaTbl.ind i.plaFlags .~ (setBit zeroBits . fromEnum $ IsTunedQuestion)
                    ms'' = ms' & invTbl.ind iCentral   %~ (sortInv ms' . (++ pure i))
                in (ms'', (ms'', getSing i ms))
interpConfirmName _ _ ActionParams { plaMsgQueue } = promptRetryYesNo plaMsgQueue


notifyQuestion :: Id -> MudState -> MudStack ()
notifyQuestion i ms =
    let msg      = f "A new character has arrived in CurryMUD."
        f        = (colorWith arrowColor "<- " <>) . colorWith questionArrivalColor
        tunedIds = uncurry (++) . getTunedQuestionIds i $ ms
    in bcastNl =<< expandEmbeddedIds ms questionChanContext =<< formatQuestion i ms (msg, tunedIds)


yesNo :: Text -> Maybe Bool
yesNo (T.toLower -> a) = guard (()!# a) >> helper
  where
    helper | a `T.isPrefixOf` "yes" = return True
           | a `T.isPrefixOf` "no"  = return False
           | otherwise              = Nothing


handleLogin :: Sing -> ActionParams -> MudStack ()
handleLogin s params@ActionParams { .. } = do
    greet
    showMotd plaMsgQueue plaCols
    (ms, p) <- showRetainedMsgs
    look params
    sendDfltPrompt plaMsgQueue myId
    when (getPlaFlag IsAdmin p) stopInacTimer
    runRegenAsync myId
    notifyArrival ms
  where
    greet = wrapSend plaMsgQueue plaCols . nlPrefix $ if s == "Root"
      then colorWith zingColor sudoMsg
      else "Welcome back, " <> s <> "!"
    showRetainedMsgs = helper |&| modifyState >=> \(ms, msgs, p) -> do
        unless (()# msgs) $ do
            let (fromPpl, others) = first (map T.tail) . partition ((== fromPersonMarker) . T.head) $ msgs
            others  |#| multiWrapSend plaMsgQueue plaCols . intersperse ""
            fromPpl |#| let m   = "message" <> case fromPpl of [_] -> ""
                                                               _   -> "s"
                            msg = "You missed the following " <> m <> " while you were away:"
                        in multiWrapSend plaMsgQueue plaCols . (msg :)
            logPla "handleLogin showRetainedMsgs" myId "showed retained messages."
        return (ms, p)
    helper ms = let p   = getPla myId ms
                    p'  = p  & retainedMsgs    .~ []
                    ms' = ms & plaTbl.ind myId .~ p'
                in (ms', (ms', p^.retainedMsgs, p'))
    stopInacTimer = do
        liftIO . atomically . writeTQueue plaMsgQueue $ InacStop
        logPla "handleLogin stopInacTimer" myId "stopping the inactivity timer."
    notifyArrival ms = do
        bcastOtherAdmins myId $ s <> " has logged in."
        bcastOthersInRm  myId . nlnl . notifyArrivalMsg . mkSerializedNonStdDesig myId ms s A $ DoCap


promptRetryYesNo :: MsgQueue -> MudStack ()
promptRetryYesNo mq = sendPrompt mq . T.concat $ [ "Please answer ", dblQuote "yes", " or ", dblQuote "no", "." ]
