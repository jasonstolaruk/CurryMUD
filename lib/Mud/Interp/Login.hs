{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, MonadComprehensions, MultiWayIf, NamedFieldPuns, OverloadedStrings, PatternSynonyms, RecordWildCards, ViewPatterns #-}

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
import Mud.TheWorld.Zones.AdminZoneIds (iCentral, iLoggedOut, iWelcome)
import Mud.Threads.Digester
import Mud.Threads.Effect
import Mud.Threads.Misc
import Mud.Threads.Regen
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Telnet
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.List
import Mud.Util.Misc hiding (blowUp, patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice, logPla)
import qualified Mud.Util.Misc as U (blowUp, patternMatchFail)

import Control.Arrow (first)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception.Lifted (try)
import Control.Lens (at, both, views)
import Control.Lens.Operators ((%~), (&), (.~), (^.))
import Control.Monad ((>=>), guard, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (orM)
import Crypto.BCrypt (validatePassword)
import Data.Bits (setBit, zeroBits)
import Data.Ix (inRange)
import Data.List (delete, intersperse, partition)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Any(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Network (HostName)
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (foldr, toList)
import qualified Data.Set as S (Set, empty, fromList, insert, member)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T (readFile)


default (Int)


-----


blowUp :: Text -> Text -> [Text] -> a
blowUp = U.blowUp "Mud.Interp.Login"


patternMatchFail :: Text -> [Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Interp.Login"


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Interp.Login"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Interp.Login"


-- ==================================================


-- TODO: Consider improving the messaging to admins describing how a player is progressing through login.
interpName :: Interp
interpName (T.toLower -> cn@(capitalize -> cn')) (NoArgs' i mq)
  | not . inRange (minNameLen, maxNameLen) . T.length $ cn = promptRetryName mq sorryInterpNameLen
  | T.any (`elem` illegalChars) cn                         = promptRetryName mq sorryInterpNameIllegal
  | otherwise                                              = getState >>= \ms ->
      case filter ((== cn') . (`getSing` ms) . fst) . views plaTbl IM.toList $ ms of
        [] -> mIf (orM . map (getAny <$>) $ [ checkProfanitiesDict i  mq cn
                                            , checkIllegalNames    ms mq cn
                                            , checkPropNamesDict      mq cn
                                            , checkWordsDict          mq cn
                                            , checkRndmNames          mq cn ])
                  unit
                  confirmName
        [(targetId, targetPla)] -> do
            sendPrompt mq $ telnetHideInput <> "Password:"
            setInterp i . Just . interpPW cn' targetId $ targetPla
        (map fst -> xs) -> patternMatchFail "interpName" [ showText xs ]
  where
    illegalChars = [ '!' .. '@' ] ++ [ '[' .. '`' ] ++ [ '{' .. '~' ]
    confirmName  = do
        sendPrompt mq . nlPrefix $ "Your name will be " <> dblQuote (cn' <> ",") <> " is that OK? [yes/no]"
        setInterp i . Just . interpConfirmName $ cn'
interpName _ ActionParams { plaMsgQueue } = promptRetryName plaMsgQueue sorryInterpNameExcessArgs


promptRetryName :: MsgQueue -> Text -> MudStack ()
promptRetryName mq msg =
    (send mq . nlPrefix $ msg |!| nl msg) >> sendPrompt mq "Let's try this again. By what name are you known?"


-----


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


-- ==================================================


interpConfirmName :: Sing -> Interp
interpConfirmName s cn (NoArgs' i mq) = case yesNoHelper cn of
  Just True  -> do
      sendPrompt mq . T.concat $ [ telnetHideInput, desc, "New password:" ]
      setInterp i . Just . interpNewPW $ s
  Just False -> promptRetryName  mq "" >> setInterp i (Just interpName)
  Nothing    -> promptRetryYesNo mq
  where
    desc = "Please choose a password for " <> s <> ". Passwords must be 6-20 characters in length and contain:\n\
           \* 1 or more lowercase characters\n\
           \* 1 or more uppercase characters\n\
           \* 1 or more digits\n\
           \* 1 or more symbols\n" |&| nlPrefix
interpConfirmName _ _ ActionParams { plaMsgQueue } = promptRetryYesNo plaMsgQueue


promptRetryYesNo :: MsgQueue -> MudStack ()
promptRetryYesNo mq = sendPrompt mq . T.concat $ [ "Please answer ", dblQuote "yes", " or ", dblQuote "no", "." ]


yesNoHelper :: Text -> Maybe Bool
yesNoHelper (T.toLower -> a) = guard (()!# a) >> helper
  where
    helper | a `T.isPrefixOf` "yes" = return True
           | a `T.isPrefixOf` "no"  = return False
           | otherwise              = Nothing


-- ==================================================


interpNewPW :: Sing -> Interp
interpNewPW s cn (NoArgs' i mq)
  | True {- TODO -} = do
      sendPrompt mq "Verify password:"
      setInterp i . Just . interpVerifyNewPW s $ cn
  | otherwise = promptRetryNewPW mq "Invalid password."
interpNewPW _ _ ActionParams { plaMsgQueue } = promptRetryNewPW_Whitespace plaMsgQueue


promptRetryNewPW_Whitespace :: MsgQueue -> MudStack ()
promptRetryNewPW_Whitespace mq = promptRetryNewPW mq "Passwords may not contain whitespace."


promptRetryNewPW :: MsgQueue -> Text -> MudStack ()
promptRetryNewPW mq msg =
    (send mq . nlPrefix $ msg |!| nl msg) >> sendPrompt mq "Let's try this again. New password:"


-- ==================================================


interpVerifyNewPW :: Sing -> Text -> Interp
interpVerifyNewPW s pass cn params@(NoArgs' i mq)
  | cn == pass = do
      withDbExHandler_ "unpw" . insertDbTblUnPw . UnPwRec s $ pass
      sendPrompt mq telnetShowInput
      helper |&| modifyState >=> \(ms@(getPla i -> p), oldSing) -> do
          logNotice "interpConfirmName"   . T.concat $ [ dblQuote oldSing
                                                       , " has logged in as "
                                                       , s
                                                       , " "
                                                       , parensQuote "new character"
                                                       , "." ]
          initPlaLog i s
          logPla "interpConfirmName" i $ "new character logged in from " <> views currHostName T.pack p <> "."
          send mq . nl $ ""
          handleLogin s True params
          notifyQuestion i ms
  | otherwise  = promptRetryNewPW mq "Passwords do not match."
  where
    helper ms = let ms'  = ms  & entTbl.ind i.sing     .~ s
                               & invTbl.ind iWelcome   %~ (i `delete`)
                               & mobTbl.ind i.rmId     .~ iCentral
                               & mobTbl.ind i.interp   .~ Nothing
                               & plaTbl.ind i.plaFlags .~ (setBit zeroBits . fromEnum $ IsTunedQuestion)
                    ms'' = ms' & invTbl.ind iCentral   %~ addToInv ms' (pure i)
                in (ms'', (ms'', getSing i ms))
interpVerifyNewPW _ _ _ ActionParams { plaMsgQueue } = promptRetryNewPW_Whitespace plaMsgQueue


notifyQuestion :: Id -> MudState -> MudStack ()
notifyQuestion i ms =
    let msg      = f "A new character has arrived in CurryMUD."
        f        = (colorWith arrowColor "<- " <>) . colorWith questionArrivalColor
        tunedIds = uncurry (++) . getTunedQuestionIds i $ ms
    in bcastNl =<< expandEmbeddedIds ms questionChanContext =<< formatQuestion i ms (msg, tunedIds)


-- ==================================================


interpPW :: Sing -> Id -> Pla -> Interp -- Returning player.
interpPW targetSing targetId targetPla cn params@(WithArgs i mq cols as) = send mq telnetShowInput >> if
  | ()# cn || ()!# as -> sorryHelper sorryInterpPW
  | otherwise         -> getState >>= \ms -> do
      let oldSing = getSing i ms
      (withDbExHandler "interpPW" . liftIO . lookupPW $ targetSing) >>= \case
        Nothing        -> wrapSend mq cols dbErrorMsg -- TODO: Test this.
        Just (Just pw) -> if uncurry validatePassword ((pw, cn) & both %~ T.encodeUtf8)
          then if isLoggedIn targetPla
            then sorry (sorryInterpPWLoggedIn targetSing) . T.concat $ [ oldSing
                                                                       , " has entered the correct password for "
                                                                       , targetSing
                                                                       , "; however, "
                                                                       , targetSing
                                                                       , " is already logged in." ]
            else (withDbExHandler "interpPW" . isPlaBanned $ targetSing) >>= \case
              Nothing          -> wrapSend mq cols dbErrorMsg -- TODO: Test this.
              Just (Any True ) -> handleBanned ms oldSing
              Just (Any False) -> handleNotBanned oldSing
          else sorry sorryInterpPW . T.concat $ [ oldSing, " has entered an incorrect password for ", targetSing, "." ]
        Just Nothing -> blowUp "interpPW" "existing PC name not found in password database" . pure $ targetSing
  where
    sorry sorryMsg msg = do
        bcastAdmins msg
        logNotice "interpPW sorry" msg
        sorryHelper sorryMsg
    sorryHelper sorryMsg = do
        liftIO . threadDelay $ 2 * 10 ^ 6
        promptRetryName mq sorryMsg
        setInterp i . Just $ interpName
    handleBanned ms oldSing = do
        let host = T.pack . getCurrHostName i $ ms
            msg  = T.concat [ oldSing
                            , " has been booted at login upon entering the correct password for "
                            , targetSing
                            , " "
                            , parensQuote "player is banned"
                            , "." ]
        sendMsgBoot mq . Just . sorryInterpPWBanned $ targetSing
        bcastAdmins $ msg <> " Consider also banning host " <> dblQuote host <> "."
        logNotice "interpPW handleBanned" msg
    handleNotBanned oldSing =
        let helper ms = dup . logIn i ms (targetPla^.currHostName) (targetPla^.connectTime) $ targetId
        in helper |&| modifyState >=> \ms -> do
            logNotice "interpName" . T.concat $ [ oldSing
                                                , " has logged in as "
                                                , targetSing
                                                , ". Id "
                                                , showText targetId
                                                , " has been changed to "
                                                , showText i
                                                , "." ]
            initPlaLog i targetSing
            logPla "interpPW handleNotBanned" i $ "logged in from " <> T.pack (getCurrHostName i ms) <> "."
            handleLogin targetSing False params { args = [] }
interpPW _ _ _ _ p = patternMatchFail "interpPW" [ showText p ]


logIn :: Id -> MudState -> HostName -> Maybe UTCTime -> Id -> MudState
logIn newId ms newHost newTime originId = peepNewId . movePC $ adoptNewId
  where
    adoptNewId = ms & activeEffectsTbl.ind newId         .~ getActiveEffects originId ms
                    & activeEffectsTbl.at  originId      .~ Nothing
                    & coinsTbl        .ind newId         .~ getCoins         originId ms
                    & coinsTbl        .at  originId      .~ Nothing
                    & entTbl          .ind newId         .~ (getEnt          originId ms & entId .~ newId)
                    & entTbl          .at  originId      .~ Nothing
                    & eqTbl           .ind newId         .~ getEqMap         originId ms
                    & eqTbl           .at  originId      .~ Nothing
                    & invTbl          .ind newId         .~ getInv           originId ms
                    & invTbl          .at  originId      .~ Nothing
                    & mobTbl          .ind newId         .~ getMob           originId ms
                    & mobTbl          .at  originId      .~ Nothing
                    & pausedEffectsTbl.ind newId         .~ getPausedEffects originId ms
                    & pausedEffectsTbl.at  originId      .~ Nothing
                    & pcTbl           .ind newId         .~ getPC            originId ms
                    & pcTbl           .at  originId      .~ Nothing
                    & plaTbl          .ind newId         .~ (getPla          originId ms & currHostName .~ newHost
                                                                                         & connectTime  .~ newTime)
                    & plaTbl          .ind newId.peepers .~ getPeepers       originId ms
                    & plaTbl          .at  originId      .~ Nothing
                    & rndmNamesMstrTbl.ind newId         .~ getRndmNamesTbl  originId ms
                    & rndmNamesMstrTbl.at  originId      .~ Nothing
                    & teleLinkMstrTbl .ind newId         .~ getTeleLinkTbl   originId ms
                    & teleLinkMstrTbl .at  originId      .~ Nothing
                    & typeTbl         .at  originId      .~ Nothing
    movePC ms' = let newRmId = fromJust . getLastRmId newId $ ms'
                 in ms' & invTbl  .ind iWelcome       %~ (newId    `delete`)
                        & invTbl  .ind iLoggedOut     %~ (originId `delete`)
                        & invTbl  .ind newRmId        %~ addToInv ms' (pure newId)
                        & mobTbl  .ind newId.rmId     .~ newRmId
                        & plaTbl  .ind newId.lastRmId .~ Nothing
    peepNewId ms'@(getPeepers newId -> peeperIds) =
        let replaceId = (newId :) . (originId `delete`)
        in ms' & plaTbl %~ flip (foldr (\peeperId -> ind peeperId.peeping %~ replaceId)) peeperIds


handleLogin :: Sing -> Bool -> ActionParams -> MudStack ()
handleLogin s isNew params@ActionParams { .. } = do
    greet
    showMotd plaMsgQueue plaCols
    (ms, p) <- showRetainedMsgs
    look params
    sendDfltPrompt plaMsgQueue myId
    when (getPlaFlag IsAdmin p) stopInacTimer
    runDigesterAsync     myId
    runRegenAsync        myId
    restartPausedEffects myId
    notifyArrival ms
  where
    greet = wrapSend plaMsgQueue plaCols . nlPrefix $ if | s == "Root" -> colorWith zingColor sudoMsg
                                                         | isNew       -> "Welcome to CurryMUD, " <> s <> "!"
                                                         | otherwise   -> "Welcome back, " <> s <> "!"
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
        bcastOtherAdmins myId . (s <> ) $ if isNew
          then " has arrived in CurryMUD."
          else " has logged in."
        bcastOthersInRm  myId . nlnl . notifyArrivalMsg . mkSerializedNonStdDesig myId ms s A $ DoCap
