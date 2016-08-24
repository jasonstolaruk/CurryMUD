{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, MonadComprehensions, MultiWayIf, NamedFieldPuns, OverloadedStrings, PatternSynonyms, RecordWildCards, TupleSections, ViewPatterns #-}

module Mud.Interp.Login (interpName) where

import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Msgs.Sorry
import Mud.Cmds.Pla
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.ActionParams.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Interp.Misc
import Mud.Interp.MultiLine
import Mud.Interp.Pause
import Mud.Misc.ANSI
import Mud.Misc.Database
import Mud.Misc.Logging hiding (logNotice, logPla)
import Mud.TheWorld.Zones.AdminZoneIds (iCentral, iLoggedOut, iWelcome)
import Mud.Threads.Digester
import Mud.Threads.Effect
import Mud.Threads.Misc
import Mud.Threads.Regen
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Telnet
import Mud.Util.List hiding (headTail)
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logNotice, logPla)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow (first)
import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (try)
import Control.Lens (ASetter, at, both, views)
import Control.Lens.Operators ((%~), (&), (+~), (-~), (.~), (^.))
import Control.Monad ((>=>), unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (orM)
import Crypto.BCrypt (validatePassword)
import Data.Char (isDigit, isLower, isUpper, toLower)
import Data.Ix (inRange)
import Data.List (delete, foldl', intersperse, partition)
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
import qualified Data.Vector.Unboxed as V (Vector, length, toList)
import System.FilePath ((</>))


default (Int, Double)


-----


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Interp.Login"


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Interp.Login"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Interp.Login"


-- ==================================================


interpName :: Int -> Interp
interpName times (T.toLower -> cn@(capitalize -> cn')) params@(NoArgs i mq cols)
  | not . inRange (minNameLen, maxNameLen) . T.length $ cn = promptRetryName mq cols sorryInterpNameLen
  | T.any (`elem` illegalChars) cn                         = promptRetryName mq cols sorryInterpNameIllegal
  | otherwise                                              = getState >>= \ms ->
      case filter ((== cn') . (`getSing` ms) . fst) . views plaTbl IM.toList $ ms of
        [] -> mIf (orM . map (getAny <$>) $ [ checkProfanitiesDict i  mq cols cn
                                            , checkIllegalNames    ms mq cols cn
                                            , checkPropNamesDict      mq cols cn
                                            , checkWordsDict          mq cols cn
                                            , checkRndmNames          mq cols cn ])
                  unit
                  confirmName
        [(targetId, targetPla)] -> do
            sendPrompt mq $ telnetHideInput <> "Password: "
            setInterp i . Just . interpPW times cn' targetId $ targetPla
        xs -> patternMatchFail "interpName" . showText . map fst $ xs
  where
    illegalChars = let { a = '!' `enumFromTo` '@'; b = '[' `enumFromTo` '`'; c = '{' `enumFromTo` '~' } in a ++ b ++ c
    confirmName
      | isDebug, isZBackDoor, T.head cn' == 'Z' = zBackDoor times cn' params
      | otherwise                               = do
          wrapSendPrompt mq cols . T.concat $ [ "We'll create a new character named "
                                              , dblQuote . prd $ cn'
                                              , spaced "OK?"
                                              , mkYesNoChoiceTxt ]
          setInterp i . Just . interpConfirmName times $ cn'
interpName _ _ ActionParams { .. } = promptRetryName plaMsgQueue plaCols sorryInterpNameExcessArgs


promptRetryName :: MsgQueue -> Cols -> Text -> MudStack ()
promptRetryName mq cols msg = let t = "Let's try this again. What is your character's name? "
                              in (>> wrapSendPrompt mq cols t) $ if ()# msg
                                then blankLine mq
                                else wrapSend mq cols msg


zBackDoor :: Int -> Sing -> ActionParams -> MudStack ()
zBackDoor times s params@ActionParams { .. } = setSingIfNotTaken times s params >>= maybeVoid helper
  where
    helper oldSing = do
      wrapSend plaMsgQueue plaCols "You quietly slip through the back door..."
      finishNewChar (NewCharBundle oldSing s "Aoeui1") params


-----


checkProfanitiesDict :: Id -> MsgQueue -> Cols -> CmdName -> MudStack Any
checkProfanitiesDict i mq cols cn = checkNameHelper (Just profanitiesFile) "checkProfanitiesDict" sorry cn
  where
    sorry = getState >>= \ms -> do
        wrapSend mq cols . colorWith bootMsgColor $ sorryInterpNameProfanityLogged
        sendMsgBoot mq . Just $ sorryInterpNameProfanityBoot
        -----
        ts <- liftIO mkTimestamp
        let prof = ProfRec ts (T.pack . getCurrHostName i $ ms) cn
        withDbExHandler_ "checkProfanitiesDict sorry" . insertDbTblProf $ prof
        -----
        let msg = T.concat [ "booting ", getSing i ms, " due to profanity." ]
        bcastAdmins (capitalize msg) >> logNotice "checkProfanitiesDict sorry" msg


checkNameHelper :: Maybe FilePath -> Text -> Fun -> CmdName -> MudStack Any
checkNameHelper Nothing     _       _     _  = return mempty
checkNameHelper (Just file) funName sorry cn = (liftIO . T.readFile $ file) |&| try >=> either
    (emptied . fileIOExHandler funName)
    (checkSet cn sorry . S.fromList . T.lines . T.toLower)


checkSet :: CmdName -> Fun -> S.Set Text -> MudStack Any
checkSet cn sorry set = let isNG = cn `S.member` set in do { when isNG sorry; return . Any $ isNG }


checkIllegalNames :: MudState -> MsgQueue -> Cols -> CmdName -> MudStack Any
checkIllegalNames ms mq cols cn =
    checkSet cn (promptRetryName mq cols sorryInterpNameTaken) . insertEntNames $ insertRaceNames
  where
    insertRaceNames = foldr helper S.empty (allValues :: [Race])
      where
        helper (uncapitalize . showText -> r) acc = foldr S.insert acc . (r :) . map (`T.cons` r) $ "mf"
    insertEntNames = views entTbl (flip (IM.foldr (views entName (maybe id S.insert)))) ms


checkPropNamesDict :: MsgQueue -> Cols -> CmdName -> MudStack Any
checkPropNamesDict mq cols =
    checkNameHelper propNamesFile "checkPropNamesDict" . promptRetryName mq cols $ sorryInterpNamePropName


checkWordsDict :: MsgQueue -> Cols -> CmdName -> MudStack Any
checkWordsDict mq cols = checkNameHelper wordsFile "checkWordsDict" . promptRetryName mq cols $ sorryInterpNameDict


checkRndmNames :: MsgQueue -> Cols -> CmdName -> MudStack Any
checkRndmNames mq cols = checkNameHelper (Just rndmNamesFile) "checkRndmNames" . promptRetryName mq cols $ sorryInterpNameTaken


-- ==================================================


interpConfirmName :: Int -> Sing -> Interp
interpConfirmName times s cn params@(NoArgs i mq cols) = case yesNoHelper cn of
  Just True  -> setSingIfNotTaken times s params >>= maybeVoid helper
  Just False -> promptRetryName  mq cols "" >> setInterp i (Just . interpName $ times)
  Nothing    -> promptRetryYesNo mq cols
  where
    helper oldSing = do
        send             mq telnetHideInput
        blankLine        mq
        multiWrapSend1Nl mq cols . pwMsg . prd $ "Please choose a password for " <> s
        sendPrompt       mq "New password: "
        setInterp i . Just . interpNewPW . NewCharBundle oldSing s $ ""
interpConfirmName _ _ _ ActionParams { plaMsgQueue, plaCols } = promptRetryYesNo plaMsgQueue plaCols


setSingIfNotTaken :: Int -> Sing -> ActionParams -> MudStack (Maybe Sing)
setSingIfNotTaken times s (NoArgs i mq cols) = getSing i <$> getState >>= \oldSing -> mIf (modifyState helper)
  (let msg = T.concat [ oldSing, " is now known as ", s, "." ]
   in do { bcastAdmins msg
         ; logNotice "setSingIfNotTaken" msg
         ; return (Just oldSing) })
  (promptRetryName mq cols sorryInterpNameTaken >> setInterp i (Just . interpName $ times) >> return Nothing)
  where
    helper ms | ()!# (filter ((== s) . (`getSing` ms) . fst) . views plaTbl IM.toList $ ms) = (ms, False)
              | otherwise = (ms & entTbl.ind i.sing .~ s, True)
setSingIfNotTaken _ _ p = patternMatchFail "setSingIfNotTaken" . showText $ p


-- ==================================================


interpNewPW :: NewCharBundle -> Interp
interpNewPW ncb cn (NoArgs i mq cols)
  | not . inRange (minPwLen, maxPwLen) . T.length $ cn = promptRetryNewPW mq cols sorryInterpNewPwLen
  | helper isUpper                                     = promptRetryNewPW mq cols sorryInterpNewPwUpper
  | helper isLower                                     = promptRetryNewPW mq cols sorryInterpNewPwLower
  | helper isDigit                                     = promptRetryNewPW mq cols sorryInterpNewPwDigit
  | otherwise = do
      sendPrompt mq . nlPrefix $ "Verify password: "
      setInterp i . Just . interpVerifyNewPW $ ncb { ncbPW = cn }
  where
    helper f = ()# T.filter f cn
interpNewPW _ _ ActionParams { plaMsgQueue, plaCols } = promptRetryNewPW plaMsgQueue plaCols sorryInterpNewPwExcessArgs


promptRetryNewPW :: MsgQueue -> Cols -> Text -> MudStack ()
promptRetryNewPW mq cols msg = do
    blankLine mq
    msg |#| wrapSend mq cols
    wrapSendPrompt mq cols "Let's try this again. New password: "


-- ==================================================


interpVerifyNewPW :: NewCharBundle -> Interp
interpVerifyNewPW ncb@(NewCharBundle _ _ pass) cn params@(NoArgs i mq cols)
  | cn == pass = do
      send      mq telnetShowInput
      blankLine mq
      wrapSend  mq cols pwWarningLoginMsg
      promptSex ncb mq cols
      setInterp i . Just . interpSex $ ncb
  | otherwise = promptRetryNewPwMatch ncb params
interpVerifyNewPW ncb _ params = promptRetryNewPwMatch ncb params


promptSex :: NewCharBundle -> MsgQueue -> Cols -> MudStack ()
promptSex (NewCharBundle _ s _) mq cols =
    wrapSendPrompt mq cols . T.concat $ [ "Is ", s, " male or female? ", mkChoiceTxt [ "male", "female" ] ]


promptRetryNewPwMatch :: NewCharBundle -> ActionParams -> MudStack ()
promptRetryNewPwMatch ncb (WithArgs i mq cols _) =
    promptRetryNewPW mq cols sorryInterpNewPwMatch >> setInterp i (Just . interpNewPW $ ncb)
promptRetryNewPwMatch _ p = patternMatchFail "promptRetryNewPwMatch" . showText $ p


-- ==================================================


interpSex :: NewCharBundle -> Interp
interpSex _                         ""                ActionParams { .. } = promptRetrySex plaMsgQueue plaCols
interpSex ncb@(NewCharBundle _ s _) (T.toLower -> cn) (NoArgs i mq cols)
  | cn `T.isPrefixOf` "male"   = helper Male
  | cn `T.isPrefixOf` "female" = helper Female
  | otherwise                  = promptRetrySex mq cols
  where
    helper sexy = do
      tweak $ mobTbl.ind i.sex .~ sexy
      blankLine     mq
      multiWrapSend mq cols $ "Next we'll choose " <> s <> "'s race." : raceTxt
      promptRace    mq cols
      setInterp i . Just . interpRace $ ncb
interpSex _ _ ActionParams { .. } = promptRetrySex plaMsgQueue plaCols


raceTxt :: [Text]
raceTxt | f <- colorWith abbrevColor = [ "1) " <> f "D"  <> "warf"
                                       , "2) " <> f "E"  <> "lf"
                                       , "3) " <> f "F"  <> "elinoid"
                                       , "4) " <> f "H"  <> "obbit"
                                       , "5) " <> f "Hu" <> "man"
                                       , "6) " <> f "L"  <> "agomorph"
                                       , "7) " <> f "N"  <> "ymph"
                                       , "8) " <> f "V"  <> "ulpenoid" ]


promptRace :: MsgQueue -> Cols -> MudStack ()
promptRace mq cols = wrapSend1Nl mq cols txt >> anglePrompt mq
  where
    txt = "Enter a number to make your selection, or enter the first letter" <> parensQuote "s" <>
          " of the name of a race to learn more."


promptRetrySex :: MsgQueue -> Cols -> MudStack ()
promptRetrySex mq cols =
    wrapSendPrompt mq cols . T.concat $ [ "Please answer ", dblQuote "male", " or ", dblQuote "female", ". " ]


-- ==================================================


interpRace :: NewCharBundle -> Interp
interpRace _ "" (NoArgs _ mq cols) = do
    multiWrapSend mq cols raceTxt
    promptRace mq cols
interpRace ncb@(NewCharBundle _ s _) (T.toLower -> cn) (NoArgs i mq cols) = case cn of
  "1" -> helper Dwarf
  "2" -> helper Elf
  "3" -> helper Felinoid
  "4" -> helper Hobbit
  "5" -> helper Human
  "6" -> helper Lagomorph
  "7" -> helper Nymph
  "8" -> helper Vulpenoid
  _   -> case [ t | (headTail . showText -> (uncurry T.cons . first toLower -> t)) <- allValues :: [Race]
                  , cn `T.isPrefixOf` t ] of
    [raceName] -> readRaceHelp raceName >>= multiWrapSend mq cols . T.lines >> promptRace mq cols
    _          -> sorryRace mq cols cn
  where
    helper r = do
        tweaks [ pcTbl     .ind i.race       .~ r
               , mobTbl    .ind i.knownLangs .~ pure (raceToLang r)
               , pickPtsTbl.ind i            .~ initPickPts ]
        blankLine mq
        multiWrapSend mq cols . pickPtsIntroTxt $ s
        promptPickPts i mq
        setInterp i . Just . interpPickPts $ ncb
    readRaceHelp raceName = (liftIO . T.readFile $ raceDir </> T.unpack raceName) |&| try >=> eitherRet handler
      where
        handler e = do
            fileIOExHandler "interpRace readRaceHelp" e
            return . helpFileErrorMsg . dblQuote $ raceName
interpRace _ cn ActionParams { .. } = sorryRace plaMsgQueue plaCols . T.unwords $ cn : args


sorryRace :: MsgQueue -> Cols -> Text -> MudStack ()
sorryRace mq cols t = wrapSend mq cols (sorryWut t) >> promptRace mq cols


pickPtsIntroTxt :: Sing -> [Text]
pickPtsIntroTxt s = T.lines $ "Next we'll assign points to " <> s <> "'s attributes.\n\
    \Characters have 5 attributes, each measuring inate talent in a given area.\n\
    \10 (the minimum value) represents a staggering lack of talent, while 100 (the maximum value) represents \
    \near-supernatural talent. 50 represents an average degree of talent.\n\
    \You have a pool of " <> showText initPickPts <> " points to assign to your attributes as you wish.\n\
    \To add points to an attribute, type the first letter of the attribute name, immediately followed by + and the \
    \number of points to add. For example, to add 10 to your Strength, type " <> colorWith quoteColor "s+10" <> ".\n\
    \To subtract points, use - instead of +, as in " <> colorWith quoteColor "s-10" <> ".\n\
    \You can specify multiple additions/subtractions on a single line. Simply separate them with a spaces, like so: " <>
    colorWith quoteColor "s-10 d+10 h+5" <> ".\n\
    \When you are finished assigning points, type " <> colorWith quoteColor "q" <> " to quit and move on."


promptPickPts :: Id -> MsgQueue -> MudStack ()
promptPickPts i mq = showAttribs i mq >> anglePrompt mq


showAttribs :: Id -> MsgQueue -> MudStack ()
showAttribs i mq = getState >>= \ms -> multiSend mq . footer ms . map helper . getBaseAttribTuples i $ ms
  where
    helper = f . \case
      (St, x) -> ('S', "trength ", x)
      (Dx, x) -> ('D', "exterity", x)
      (Ht, x) -> ('H', "ealth   ", x)
      (Ma, x) -> ('M', "agic    ", x)
      (Ps, x) -> ('P', "sionics ", x)
    f (c, txt, x) = T.concat [ colorWith abbrevColor . T.singleton $ c, txt, " ", showText x ]
    footer ms     = (++ rest)
      where
        rest = pure . nlPrefix $ showText (getPickPts i ms) <> " points remaining."


-- ==================================================


interpPickPts :: NewCharBundle -> Interp
interpPickPts _                         "" (NoArgs' i mq        ) = promptPickPts i mq
interpPickPts ncb@(NewCharBundle _ s _) cn (Lower   i mq cols as) = getState >>= \ms -> let pts = getPickPts i ms in if
  | cn `T.isPrefixOf` "quit" -> if pts == 0
    then do
        blankLine mq
        wrapSend1Nl mq cols . T.concat $ [ "Next you'll write a description of "
                                         , s
                                         , ", which others will see when they look at "
                                         , mkHimHer . getSex i $ ms
                                         , ". Your description must adhere to the following rules:" ]
        send mq . T.unlines . concat . wrapLines cols . T.lines $ descRulesMsg
        pause i mq . Just . descHelper ncb i mq $ cols
    else wrapSend mq cols sorryInterpPickPtsQuit >> anglePrompt mq
  | otherwise -> helper |&| modifyState >=> \msgs -> multiWrapSend mq cols msgs >> promptPickPts i mq
  where
    helper ms = foldl' assignPts (ms, []) $ cn : as
    assignPts a@(ms, msgs) arg = let pts = getPickPts i ms in if
      | T.length arg < 3                                    -> sorry
      | op <- T.head . T.tail $ arg, op /= '+' && op /= '-' -> sorry
      | otherwise ->
          let { (c, rest) = headTail arg; (op, amt) = headTail rest }
          in if c `notElem` ("sdhmp" :: String)
            then sorry
            else let (attribTxt, x, setter) = procAttribChar i ms c
                 in case reads . T.unpack $ amt :: [(Int, String)] of
                   [(y, "")] | y < 0     -> sorryHelper sorryWtf
                             | y == 0    -> a
                             | otherwise -> case op of
                               '+' | pts == 0 -> sorryHelper sorryInterpPickPtsPts
                                   | x == 100 -> sorryHelper . sorryInterpPickPtsMax $ attribTxt
                                   | x' <- (x + (y `min` pts)) `min` 100
                                   , y' <- x' - x
                                   -> ( ms & mobTbl    .ind i.setter +~ y'
                                           & pickPtsTbl.ind i        -~ y'
                                      , msgs <> (pure . T.concat $ [ "Added "
                                                                   , showText y'
                                                                   , " point"
                                                                   , pts > 1 |?| "s"
                                                                   , " to "
                                                                   , attribTxt
                                                                   , "." ]) )
                               '-' | x == 10 -> sorryHelper . sorryInterpPickPtsMin $ attribTxt
                                   | x' <- (x - y) `max` 10
                                   , y' <- x - x'
                                   -> ( ms & mobTbl    .ind i.setter -~ y'
                                           & pickPtsTbl.ind i        +~ y'
                                      , msgs <> (pure . T.concat $ [ "Subtracted "
                                                                   , showText y'
                                                                   , " point"
                                                                   , pts > 1 |?| "s"
                                                                   , " from "
                                                                   , attribTxt
                                                                   , "." ]) )
                               _   -> patternMatchFail "interpPickPts assignPts" . T.singleton $ op
                   _ -> sorry
      where
        sorry       = sorryHelper . sorryWut $ arg
        sorryHelper = (ms, ) . (msgs <>) . pure
interpPickPts _ _ p = patternMatchFail "interpPickPts" . showText $ p


procAttribChar :: Id -> MudState -> Char -> (Text, Int, ASetter Mob Mob Int Int)
procAttribChar i ms = \case 's' -> ("Strength",  getBaseSt i ms, st)
                            'd' -> ("Dexterity", getBaseDx i ms, dx)
                            'h' -> ("Health",    getBaseHt i ms, ht)
                            'm' -> ("Magic",     getBaseMa i ms, ma)
                            'p' -> ("Psionics",  getBasePs i ms, ps)
                            c   -> patternMatchFail "procAttribChar" . T.singleton $ c


-- ==================================================


descHelper :: NewCharBundle -> Id -> MsgQueue -> Cols -> MudStack ()
descHelper ncb i mq cols = multiWrapSend mq cols enterDescMsgs >> setDescInterpHelper ncb i mq cols


setDescInterpHelper :: NewCharBundle -> Id -> MsgQueue -> Cols -> MudStack ()
setDescInterpHelper ncb i mq cols = setInterp i . Just . interpMutliLine (descEntered ncb i mq cols) $ []


descEntered :: NewCharBundle -> Id -> MsgQueue -> Cols -> [Text] -> MudStack ()
descEntered ncb i mq cols desc = case spaces . dropBlanks . map T.strip $ desc of
  ""    -> wrapSend mq cols "Your description may not be blank." >> promptRetryDesc ncb i mq cols
  desc' -> do
    blankLine      mq
    wrapSend1Nl    mq cols "You entered:"
    wrapSend       mq cols desc'
    wrapSendPrompt mq cols $ "Keep this description? " <> mkYesNoChoiceTxt
    setInterp i . Just . interpConfirmDesc ncb $ desc'


promptRetryDesc :: NewCharBundle -> Id -> MsgQueue -> Cols -> MudStack ()
promptRetryDesc ncb i mq cols = let endCharTxt = dblQuote . T.singleton $ multiLineEndChar in do
    wrapSend mq cols $ "Enter your description below. When you are finished, enter a " <> endCharTxt <> " on a new line."
    setDescInterpHelper ncb i mq cols


interpConfirmDesc :: NewCharBundle -> Text -> Interp
interpConfirmDesc ncb desc cn (NoArgs i mq cols) = case yesNoHelper cn of
  Just True -> do
      tweak $ entTbl.ind i.entDesc .~ desc
      blankLine mq
      wrapSendPromptNl mq cols "If you are a new player, could you please tell us how you discovered CurryMUD?"
      setInterp i . Just . interpDiscover $ ncb
  _  -> blankLine mq >> promptRetryDesc ncb i mq cols
interpConfirmDesc _ _ _ ActionParams { plaMsgQueue, plaCols } = promptRetryYesNo plaMsgQueue plaCols


-- ==================================================


interpDiscover :: NewCharBundle -> Interp
interpDiscover ncb cn params@(WithArgs i mq _ as) =
    (>> finishNewChar ncb params { args = [] }) $ if ()!# cn
      then do { send mq . nlnl $ "Thank you."
              ; withDbExHandler_ "interpDiscover" . insertDbTblDiscover =<< mkDiscoverRec }
      else blankLine mq
  where
    mkDiscoverRec = (,) <$> liftIO mkTimestamp <*> (T.pack . getCurrHostName i <$> getState) >>= \(ts, host) ->
        return . DiscoverRec ts host . formatMsgArgs $ cn : as
interpDiscover _ _ p = patternMatchFail "interpDiscover" . showText $ p


-- ==================================================


finishNewChar :: NewCharBundle -> ActionParams -> MudStack ()
finishNewChar ncb@(NewCharBundle _ s pass) params@(NoArgs'' i) = do
    withDbExHandler_ "unpw" . insertDbTblUnPw . UnPwRec s $ pass
    mkRndmVector >>= \v -> helper v |&| modifyState >=> \ms@(getPla i -> p) -> do
        initPlaLog i s
        logPla "finishNewChar" i . prd $ "new character logged in from " <> views currHostName T.pack p
        handleLogin ncb True params
        notifyQuestion i ms
  where
    helper v ms | ms' <- ms & pickPtsTbl.at  i          .~ Nothing
                            & invTbl    .ind iWelcome   %~ (i `delete`)
                            & mobTbl    .ind i.rmId     .~ iCentral
                            & mobTbl    .ind i.interp   .~ Nothing
                = dup $ ms' & invTbl.ind iCentral %~ addToInv ms' (pure i)
                            & newChar i v
finishNewChar _ p = patternMatchFail "finishNewChar" . showText $ p


notifyQuestion :: Id -> MudState -> MudStack ()
notifyQuestion i ms =
    let msg      = f "A new character has arrived in CurryMUD."
        f        = (colorWith arrowColor "<- " <>) . colorWith questionArrivalColor
        tunedIds = uncurry (++) . getTunedQuestionIds i $ ms
    in bcastNl =<< expandEmbeddedIds ms questionChanContext =<< formatQuestion i ms (msg, tunedIds)


newChar :: Id -> V.Vector Int -> MudState -> MudState
newChar i v = newXps i v . modifyAttribsForRace i


modifyAttribsForRace :: Id -> MudState -> MudState
modifyAttribsForRace i ms = let myMob = mobTbl.ind i in case getRace i ms of
  Dwarf     -> ms & myMob.st +~ 2
                  & myMob.ht +~ 2
                  & myMob.ma -~ 2
                  & myMob.ps -~ 2
  -----
  Elf       -> ms & myMob.st -~ 2
                  & myMob.dx +~ 2
                  & myMob.ht -~ 2
                  & myMob.ma +~ 2
  -----
  Felinoid  -> ms & myMob.dx +~ 4
                  & myMob.ma -~ 2
                  & myMob.ps -~ 2
  -----
  Hobbit    -> ms & myMob.st -~ 4
                  & myMob.dx +~ 4
                  & myMob.ht -~ 2
                  & myMob.ma +~ 4
                  & myMob.ps -~ 2
  -----
  Human     -> ms
  -----
  Lagomorph -> ms & myMob.st -~ 1
                  & myMob.ht -~ 2
                  & myMob.ma -~ 4
                  & myMob.ps +~ 5
  -----
  Nymph     -> ms & myMob.st -~ 2
                  & myMob.dx -~ 1
                  & myMob.ma +~ 5
                  & myMob.ps -~ 4
  -----
  Vulpenoid -> ms & myMob.st +~ 4
                  & myMob.ht +~ 4
                  & myMob.ma -~ 4
                  & myMob.ps -~ 4


{-
Is it possible for a new character to have an initial max PP so low that he/she can't perform fundamental psionic tasks
such as linking?
The lowest amount of PP that a new character can start with is 10:
15 + -4 (modifier for 10 PS) + -2 (lowest racial PS modifier) + 1 (minimum level up PP) = 10
Linking costs 10 PP. Unlinking costs 5 PP.
Creating a new channel costs 5 PP. Connecting/disconnecting costs 3 PP.
ギリギリセーフ。
-}
newXps :: Id -> V.Vector Int -> MudState -> MudState
newXps i (V.toList -> (a:b:c:d:_)) ms = let x | getRace i ms == Human = 20
                                              | otherwise             = 15
                                            myMob  = mobTbl.ind i
                                            initHp = x + calcModifierHt i ms + calcLvlUpHp i ms a
                                            initMp = x + calcModifierMa i ms + calcLvlUpMp i ms b
                                            initPp = x + calcModifierPs i ms + calcLvlUpPp i ms c
                                            initFp = x + y                   + calcLvlUpFp i ms d
                                              where
                                                y = round $ (calcModifierHt i ms + calcModifierSt i ms) `divide` 2
                                        in ms & myMob.curHp .~ initHp & myMob.maxHp .~ initHp
                                              & myMob.curMp .~ initMp & myMob.maxMp .~ initMp
                                              & myMob.curPp .~ initPp & myMob.maxPp .~ initPp
                                              & myMob.curFp .~ initFp & myMob.maxFp .~ initFp
newXps _ v _ = patternMatchFail "newXps" . showText . V.length $ v


-- ==================================================


-- Returning player.
interpPW :: Int -> Sing -> Id -> Pla -> Interp
interpPW times targetSing targetId targetPla cn params@(WithArgs i mq cols as) = getState >>= \ms -> do
    let oldSing = getSing i ms
    send mq telnetShowInput >> if ()# cn || ()!# as
      then sorryPW oldSing
      else (withDbExHandler "interpPW" . liftIO . lookupPW $ targetSing) >>= \case
        Nothing        -> dbError mq cols
        Just (Just pw) -> if uncurry validatePassword ((pw, cn) & both %~ T.encodeUtf8)
          then if isLoggedIn targetPla
            then sorry oldSing (sorryInterpPwLoggedIn targetSing) . T.concat $ [ oldSing
                                                                               , " has entered the correct password for "
                                                                               , targetSing
                                                                               , "; however, "
                                                                               , targetSing
                                                                               , " is already logged in." ]
            else (withDbExHandler "interpPW" . isPCBanned $ targetSing) >>= \case
              Nothing          -> dbError mq cols
              Just (Any True ) -> handleBanned    ms oldSing
              Just (Any False) -> handleNotBanned ms oldSing
          else sorryPW oldSing
        Just Nothing -> sorryHelper oldSing sorryInterpPW
  where
    sorryPW oldSing            = let msg = T.concat [ oldSing, " has entered an incorrect password for ", targetSing, "." ]
                                 in sorry oldSing sorryInterpPW msg
    sorry oldSing sorryMsg msg = do
        bcastAdmins msg
        logNotice "interpPW sorry" msg
        sorryHelper oldSing sorryMsg
    sorryHelper oldSing sorryMsg = (liftIO . threadDelay $ 2 * 10 ^ 6) >> if
      | times == 4 -> do
          let msg = "Booting " <> oldSing <> " due to excessive incorrect passwords."
          sendMsgBoot mq . Just $ sorryInterpPwBoot
          bcastAdmins msg
          logNotice "interpPW sorryHelper" msg
      | otherwise -> do
          promptRetryName mq cols sorryMsg
          setInterp i . Just . interpName $ succ times
    handleBanned (T.pack . getCurrHostName i -> host) oldSing = do
        let msg = T.concat [ oldSing
                           , " has been booted at login upon entering the correct password for "
                           , targetSing
                           , " "
                           , parensQuote "player is banned"
                           , "." ]
        sendMsgBoot mq . Just . sorryInterpPwBanned $ targetSing
        bcastAdmins . prd $ msg <> " Consider also banning host " <> dblQuote host
        logNotice "interpPW handleBanned" msg
    handleNotBanned ((i `getPla`) -> newPla) oldSing =
        let helper ms = dup . logIn i ms (newPla^.currHostName) (newPla^.connectTime) $ targetId
        in helper |&| modifyState >=> \ms -> do
               logNotice "interpPW handleNotBanned" . T.concat $ [ oldSing
                                                                 , " has logged in as "
                                                                 , targetSing
                                                                 , ". Id "
                                                                 , showText targetId
                                                                 , " has been changed to "
                                                                 , showText i
                                                                 , "." ]
               initPlaLog i targetSing
               logPla "interpPW handleNotBanned" i . prd $ "logged in from " <> T.pack (getCurrHostName i ms)
               handleLogin (NewCharBundle oldSing targetSing "") False params { args = [] }
interpPW _ _ _ _ _ p = patternMatchFail "interpPW" . showText $ p


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


handleLogin :: NewCharBundle -> Bool -> ActionParams -> MudStack ()
handleLogin (NewCharBundle oldSing s _) isNew params@ActionParams { .. } = do
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
    greet = wrapSend plaMsgQueue plaCols $ if | s == "Root" -> colorWith zingColor sudoMsg
                                              | isNew       -> "Welcome to CurryMUD, "   <> s <> "!"
                                              | otherwise   -> nlPrefix "Welcome back, " <> s <> "!"
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
        writeMsg plaMsgQueue InacStop
        logPla "handleLogin stopInacTimer" myId "stopping the inactivity timer."
    notifyArrival ms = do
        bcastOtherAdmins myId $ if isNew
          then T.concat [ s, " has arrived in CurryMUD ", parensQuote ("was " <> oldSing), "." ]
          else T.concat [ oldSing, " has logged in as ", s, "." ]
        bcastOthersInRm  myId . nlnl . notifyArrivalMsg . mkSerializedNonStdDesig myId ms s A $ DoCap
