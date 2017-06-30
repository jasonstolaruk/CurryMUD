{-# LANGUAGE LambdaCase, MonadComprehensions, MultiWayIf, NamedFieldPuns, OverloadedStrings, PatternSynonyms, RecordWildCards, TupleSections, ViewPatterns #-}

module Mud.Interp.Login ( interpName
                        , promptName ) where

import           Mud.Cmds.Msgs.Misc
import           Mud.Cmds.Msgs.Sorry
import           Mud.Cmds.Pla
import           Mud.Cmds.Util.Misc
import           Mud.Data.Misc
import           Mud.Data.State.ActionParams.ActionParams
import           Mud.Data.State.ActionParams.Misc
import           Mud.Data.State.MsgQueue
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Calc
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Lang
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import           Mud.Interp.Misc
import           Mud.Interp.MultiLine
import           Mud.Interp.Pause
import           Mud.Misc.ANSI
import           Mud.Misc.CurryTime
import           Mud.Misc.Database
import qualified Mud.Misc.Logging as L (logNotice, logPla)
import           Mud.Misc.Logging hiding (logNotice, logPla)
import           Mud.Misc.Misc
import           Mud.Misc.Readymade
import           Mud.TheWorld.Zones.AdminZoneIds (iCentral, iLoggedOut, iWelcome)
import           Mud.TheWorld.Zones.LoplenkoIds (iLoplenkoWelcome)
import           Mud.Threads.Digester
import           Mud.Threads.Effect
import           Mud.Threads.Misc
import           Mud.Threads.Regen
import           Mud.TopLvlDefs.Chars
import           Mud.TopLvlDefs.FilePaths
import           Mud.TopLvlDefs.Misc
import           Mud.TopLvlDefs.Telnet.Chars
import           Mud.Util.List hiding (headTail)
import qualified Mud.Util.Misc as U (pmf)
import           Mud.Util.Misc hiding (pmf)
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text
import           Mud.Util.Wrapping

import           Control.Applicative (liftA2)
import           Control.Arrow (first)
import           Control.Exception.Lifted (try)
import           Control.Lens (ASetter, at, both, set, views)
import           Control.Lens.Operators ((-~), (?~), (.~), (&), (%~), (^.), (+~))
import           Control.Monad ((>=>), join, unless, void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Loops (orM)
import           Crypto.BCrypt (validatePassword)
import           Data.Char (isDigit, isLower, isUpper)
import           Data.Ix (inRange)
import           Data.List (delete, find, foldl', intersperse, partition)
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Monoid ((<>), Any(..))
import           Data.Text (Text)
import           Data.Time (UTCTime, getCurrentTime)
import           GHC.Stack (HasCallStack)
import           Network (HostName)
import           Prelude hiding (pi)
import qualified Data.IntMap.Strict as IM (foldr, keys, toList)
import qualified Data.Set as S (Set, empty, fromList, insert, member, union)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T (readFile)
import qualified Data.Vector.Unboxed as V (Vector, length, toList)
import           System.FilePath ((</>))


pmf :: PatternMatchFail
pmf = U.pmf "Mud.Interp.Login"


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Interp.Login"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Interp.Login"


-- ==================================================


interpName :: HasCallStack => Int -> Interp
interpName times (T.toLower -> cn@(capitalize -> cn')) params@(NoArgs i mq cols)
  | cn == "new"                                                  = new =<< getServerSettings
  | not . inRange (minNameLen, maxNameLen) . T.length $ cn       = promptRetryName mq cols sorryInterpNameLen
  | T.any (`elem` illegalChars) cn                               = promptRetryName mq cols sorryInterpNameIllegal
  | (> 1) . length . filter (== '\'') . T.unpack $ cn            = promptRetryName mq cols sorryInterpNameApostropheCount -- Unpacking to avoid what appears to be a bizarre GHC bug.
  | ((&&) <$> T.any (== '\'') <*> (== minNameLen) . T.length) cn = promptRetryName mq cols sorryInterpNameLenApostrophe
  | let f = ((== '\'') .) in ((||) <$> f T.head <*> f T.last) cn = promptRetryName mq cols sorryInterpNameApostrophePosition
  | otherwise                                                    = getState >>= \ms ->
      if views plaTbl (isNothing . find ((== cn') . (`getSing` ms)) . IM.keys) ms
        then mIf (orM . fmap2 getAny $ [ uncurry3 f (mq, cols, cn) | f <- [ checkProfanitiesDict i
                                                                          , checkIllegalNames ms
                                                                          , checkPropNamesDict
                                                                          , checkWordsDict
                                                                          , checkRndmNames ] ])
                 unit
                 (confirmName =<< getServerSettings)
        else do sendPrompt mq $ telnetHideInput <> cn' <> " is an existing character. Password:"
                setInterp i . Just . interpPW times $ cn'
  where
    new settings  = do send mq . nlPrefix . nl . T.unlines . parseWrapXform settings cols $ newPlaMsg
                       promptName mq
    illegalChars  = concat [ uncurry enumFromTo pair | pair <- [ ('!', '&'), ('(', '@'), ('[', '`'), ('{', '~') ] ]
    confirmName s | settingDebug s, settingZBackDoor s, T.head cn' == 'Z' = zBackDoor times cn' params
                  | otherwise = do wrapSendPrompt mq cols . T.concat $ [ "We'll create a new character named "
                                                                       , dblQuote . prd $ cn'
                                                                       , spaced "OK?"
                                                                       , mkYesNoChoiceTxt ]
                                   setInterp i . Just . interpConfirmNewChar times $ cn'
interpName _ _ ActionParams { .. } = promptRetryName plaMsgQueue plaCols sorryInterpNameExcessArgs


promptName :: HasCallStack => MsgQueue -> MudStack ()
promptName = flip sendPrompt "What is your character's name?"


promptRetryName :: HasCallStack => MsgQueue -> Cols -> Text -> MudStack ()
promptRetryName mq cols msg = let t = "Let's try this again. What is your character's name?"
                              in (>> wrapSendPrompt mq cols t) $ if ()# msg
                                then blankLine mq
                                else wrapSend mq cols msg


zBackDoor :: HasCallStack => Int -> Sing -> ActionParams -> MudStack ()
zBackDoor times s params@ActionParams { .. } = setSingIfNotTaken times s params >>= maybeVoid helper
  where
    helper oldSing = let l = mobTbl.ind myId in do
      tweaks . map (50 |&|) $ [ (l.st .~), (l.dx .~), (l.ht .~), (l.ma .~), (l.ps .~) ]
      wrapSend plaMsgQueue plaCols . thrice prd $ "You quietly slip through the back door"
      finishNewChar (NewCharBundle oldSing s "Aoeui1") params


-----


checkProfanitiesDict :: HasCallStack => Id -> MsgQueue -> Cols -> CmdName -> MudStack Any
checkProfanitiesDict i mq cols cn =
    checkNameHelper "checkProfanitiesDict" sorry cn =<< liftIO (mkMudFilePath profanitiesFileFun)
  where
    sorry = getState >>= \ms -> do let msg = T.concat [ "booting ", getSing i ms, " due to profanity." ]
                                   bcastAdmins (capitalize msg) >> logNotice "checkProfanitiesDict sorry" msg
                                   -----
                                   wrapSend mq cols . colorWith bootMsgColor $ sorryInterpNameProfanityLogged
                                   sendMsgBoot mq . Just $ sorryInterpNameProfanityBoot
                                   -----
                                   ts <- liftIO mkTimestamp
                                   let prof = ProfRec ts (T.pack . getCurrHostName i $ ms) cn
                                   withDbExHandler_ "checkProfanitiesDict sorry" . insertDbTblProf $ prof


checkNameHelper :: HasCallStack => Text -> Fun -> CmdName -> FilePath -> MudStack Any
checkNameHelper funName sorry cn file = liftIO (T.readFile file) |&| try >=> either
    (emptied . fileIOExHandler funName)
    (checkSet cn sorry . S.fromList . T.lines . T.toLower)


checkSet :: HasCallStack => CmdName -> Fun -> S.Set Text -> MudStack Any
checkSet cn sorry s = let isNG = cn `S.member` s in when isNG sorry >> return (Any isNG)


checkIllegalNames :: HasCallStack => MudState -> MsgQueue -> Cols -> CmdName -> MudStack Any
checkIllegalNames ms mq cols cn = checkSet cn (promptRetryName mq cols sorryInterpNameTaken) illegalNames
  where
    illegalNames   = insertMisc . insertEntNames . insertGodNames $ raceNames
    f xs           = S.union (S.fromList xs)
    insertMisc     = f [ "brahn"
                       , "deltheru"
                       , "dwarvish"
                       , "elendar"
                       , "eston"
                       , "felinoid"
                       , "felinoidean"
                       , "gorhna"
                       , "hobbit"
                       , "hobbitish"
                       , "lagomorphean"
                       , "morton"
                       , "naelyni"
                       , "oleverost"
                       , "orrik"
                       , "taran"
                       , "threis"
                       , "vulpenoid"
                       , "vulpenoidean" ]
    insertEntNames = views entTbl (flip (IM.foldr (views entName (maybe id S.insert)))) ms
    insertGodNames = f [ uncapitalize . pp $ x | x <- allValues :: [GodName] ]
    raceNames      = foldr helper S.empty (allValues :: [Race])
      where
        helper (uncapitalize . showTxt -> r) acc = foldr S.insert acc . (r :) . map (`T.cons` r) $ "mf"


checkPropNamesDict :: HasCallStack => MsgQueue -> Cols -> CmdName -> MudStack Any
checkPropNamesDict = checkDictHelper "checkPropNamesDict" lookupPropName sorryInterpNamePropName


checkDictHelper :: HasCallStack => FunName
                                -> (Text -> IO (Maybe Text))
                                -> Text
                                -> MsgQueue
                                -> Cols
                                -> CmdName
                                -> MudStack Any
checkDictHelper fn lookupFun sorryTxt mq cols cn = join <$> withDbExHandler fn (lookupFun cn) >>= \case
  Nothing -> return . Any $ False
  Just _  -> promptRetryName mq cols sorryTxt >> return (Any True)


checkWordsDict :: HasCallStack => MsgQueue -> Cols -> CmdName -> MudStack Any
checkWordsDict = checkDictHelper "checkWordsDict" lookupWord sorryInterpNameDict


checkRndmNames :: HasCallStack => MsgQueue -> Cols -> CmdName -> MudStack Any
checkRndmNames mq cols cn = liftIO (mkMudFilePath rndmNamesFileFun) >>=
    checkNameHelper "checkRndmNames" (promptRetryName mq cols sorryInterpNameTaken) cn


-- ==================================================


interpConfirmNewChar :: HasCallStack => Int -> Sing -> Interp
interpConfirmNewChar times s cn params@(NoArgs i mq cols) = case yesNoHelper cn of
  Just True  -> setSingIfNotTaken times s params >>= maybeVoid helper
  Just False -> promptRetryName  mq cols "" >> setInterp i (Just . interpName $ times)
  Nothing    -> promptRetryYesNo mq cols
  where
    helper oldSing = do blankLine  mq
                        sendPrompt mq $ "Are you at least 18 years of age? " <> mkYesNoChoiceTxt
                        setInterp i . Just . interpConfirmAge . NewCharBundle oldSing s $ ""
interpConfirmNewChar _ _ _ ActionParams { plaMsgQueue, plaCols } = promptRetryYesNo plaMsgQueue plaCols


setSingIfNotTaken :: HasCallStack => Int -> Sing -> ActionParams -> MudStack (Maybe Sing)
setSingIfNotTaken times s (NoArgs i mq cols) = getSing i <$> getState >>= \oldSing -> mIf (modifyState . helper $ oldSing)
  (let msg = T.concat [ oldSing, " is now known as ", s, "." ]
   in logNotice "setSingIfNotTaken" msg >> bcastAdmins msg >> return (Just oldSing))
  (emptied . sequence_ $ [ promptRetryName mq cols sorryInterpNameTaken, setInterp i . Just . interpName $ times ])
  where
    helper oldSing ms = case views plaTbl (find ((== s) . (`getSing` ms) . fst) . IM.toList) ms of
        Nothing -> (upd ms [ entTbl.ind i.sing .~ s, pcSingTbl.at oldSing .~ Nothing, pcSingTbl.at s ?~ i ], True)
        Just _  -> (ms, False)
setSingIfNotTaken _ _ p = pmf "setSingIfNotTaken" p


-- ==================================================


interpConfirmAge :: HasCallStack => NewCharBundle -> Interp
interpConfirmAge ncb cn (NoArgs i mq cols) = case yesNoHelper cn of
  Just True -> do sendPrompt mq $ "Have you read the rules? " <> mkYesNoChoiceTxt
                  setInterp i . Just . interpConfirmReadRules $ ncb
  Just False -> wrapSend         mq cols "You must be at least 18 years old to play CurryMUD." >> writeMsg mq SilentBoot
  Nothing    -> promptRetryYesNo mq cols
interpConfirmAge _ _ ActionParams { plaMsgQueue, plaCols } = promptRetryYesNo plaMsgQueue plaCols


-- ==================================================


interpConfirmReadRules :: HasCallStack => NewCharBundle -> Interp
interpConfirmReadRules ncb cn (NoArgs i mq cols) = case yesNoHelper cn of
  Just True  -> next
  Just False -> getServerSettings >>= \s -> do blankLine mq
                                               pager i mq (Just next) . parseWrapXform s cols $ rulesMsg
  Nothing    -> promptRetryYesNo mq cols
  where
    next = do sendPrompt mq $ "Do you understand and agree to follow the rules? " <> mkYesNoChoiceTxt
              setInterp i . Just . interpConfirmFollowRules $ ncb
interpConfirmReadRules _ _ ActionParams { plaMsgQueue, plaCols } = promptRetryYesNo plaMsgQueue plaCols


-- ==================================================


interpConfirmFollowRules :: HasCallStack => NewCharBundle -> Interp
interpConfirmFollowRules ncb@(NewCharBundle _ s _) cn (NoArgs i mq cols) = case yesNoHelper cn of
  Just True  -> do send             mq telnetHideInput
                   blankLine        mq
                   multiWrapSend1Nl mq cols . pwMsg . prd $ "Please choose a password for " <> s
                   sendPrompt       mq "New password:"
                   setInterp i . Just . interpNewPW $ ncb
  Just False -> do wrapSend mq cols "You can't play CurryMUD if you don't agree to follow the rules."
                   writeMsg mq SilentBoot
  Nothing    -> promptRetryYesNo mq cols
interpConfirmFollowRules _ _ ActionParams { plaMsgQueue, plaCols } = promptRetryYesNo plaMsgQueue plaCols


-- ==================================================


interpNewPW :: HasCallStack => NewCharBundle -> Interp
interpNewPW ncb cn (NoArgs i mq cols)
  | not . inRange (minPwLen, maxPwLen) . T.length $ cn = promptRetryNewPW mq cols sorryInterpNewPwLen
  | helper isUpper                                     = promptRetryNewPW mq cols sorryInterpNewPwUpper
  | helper isLower                                     = promptRetryNewPW mq cols sorryInterpNewPwLower
  | helper isDigit                                     = promptRetryNewPW mq cols sorryInterpNewPwDigit
  | otherwise = do sendPrompt mq "Verify password:"
                   setInterp i . Just . interpVerifyNewPW $ ncb { ncbPW = cn }
  where
    helper f = ()# T.filter f cn
interpNewPW _ _ ActionParams { plaMsgQueue, plaCols } = promptRetryNewPW plaMsgQueue plaCols sorryInterpNewPwExcessArgs


promptRetryNewPW :: HasCallStack => MsgQueue -> Cols -> Text -> MudStack ()
promptRetryNewPW mq cols msg = do msg |#| wrapSend mq cols
                                  wrapSendPrompt mq cols "Let's try this again. New password:"


-- ==================================================


interpVerifyNewPW :: HasCallStack => NewCharBundle -> Interp
interpVerifyNewPW ncb@(NewCharBundle _ _ pass) cn params@(NoArgs i mq cols)
  | cn == pass = do send      mq telnetShowInput
                    wrapSend  mq cols pwWarningLoginMsg
                    promptSex ncb mq cols
                    setInterp i . Just . interpSex $ ncb
  | otherwise  = promptRetryNewPwMatch ncb params
interpVerifyNewPW ncb _ params = promptRetryNewPwMatch ncb params


promptSex :: HasCallStack => NewCharBundle -> MsgQueue -> Cols -> MudStack ()
promptSex (NewCharBundle _ s _) mq cols =
    wrapSendPrompt mq cols . T.concat $ [ "Is ", s, " male or female? ", mkChoiceTxt [ "male", "female" ] ]


promptRetryNewPwMatch :: HasCallStack => NewCharBundle -> ActionParams -> MudStack ()
promptRetryNewPwMatch ncb (WithArgs i mq cols _) =
    promptRetryNewPW mq cols sorryInterpNewPwMatch >> setInterp i (Just . interpNewPW $ ncb)
promptRetryNewPwMatch _ p = pmf "promptRetryNewPwMatch" p


-- ==================================================


interpSex :: HasCallStack => NewCharBundle -> Interp
interpSex _   ""                ActionParams { .. } = promptRetrySex plaMsgQueue plaCols
interpSex ncb (T.toLower -> cn) (NoArgs i mq cols)
  | cn `T.isPrefixOf` "male"   = helper Male
  | cn `T.isPrefixOf` "female" = helper Female
  | otherwise                  = promptRetrySex mq cols
  where
    helper sexy = do tweak $ mobTbl.ind i.sex .~ sexy
                     blankLine       mq
                     promptReadymade ncb mq cols
                     setInterp i . Just . interpReadymade $ ncb
interpSex _ _ ActionParams { .. } = promptRetrySex plaMsgQueue plaCols


promptRetrySex :: HasCallStack => MsgQueue -> Cols -> MudStack ()
promptRetrySex mq cols =
    wrapSendPrompt mq cols . T.concat $ [ "Please answer ", dblQuote "male", " or ", dblQuote "female", "." ]


promptReadymade :: HasCallStack => NewCharBundle -> MsgQueue -> Cols -> MudStack ()
promptReadymade (NewCharBundle _ s _) mq cols = multiWrapSend1Nl mq cols ts >> anglePrompt mq
  where
    ts = [ "Would you like to:"
         , "1) Go through the steps of creating a new character for " <> s <> ", or"
         , "2) Choose a \"readymade\" character for " <> s <> "?" ]


-- ==================================================


interpReadymade :: HasCallStack => NewCharBundle -> Interp
interpReadymade _                         "" (NoArgs _ mq cols) = promptRetryReadymade mq cols
interpReadymade ncb@(NewCharBundle _ s _) cn (NoArgs i mq cols) = case cn of
  "1" -> do multiWrapSend mq cols $ "" : "Next we'll choose " <> s <> "'s race." : raceTxt
            promptRace    mq cols
            setInterp i . Just . interpRace $ ncb
  "2" -> do promptReadymadePC mq cols
            setInterp i . Just . interpReadymadePC $ ncb
  _   -> promptRetryReadymade mq cols
interpReadymade _ _ ActionParams { .. } = promptRetryReadymade plaMsgQueue plaCols


promptRetryReadymade :: HasCallStack => MsgQueue -> Cols -> MudStack ()
promptRetryReadymade mq cols =
    wrapSendPrompt mq cols . T.concat $ [ "Please answer ", dblQuote "1", " or ", dblQuote "2", "." ]


raceTxt :: [Text]
raceTxt | f <- colorWith abbrevColor = [ "1) " <> f "D"  <> "warf"
                                       , "2) " <> f "E"  <> "lf"
                                       , "3) " <> f "F"  <> "elinoid"
                                       , "4) " <> f "H"  <> "obbit"
                                       , "5) " <> f "Hu" <> "man"
                                       , "6) " <> f "L"  <> "agomorph"
                                       , "7) " <> f "N"  <> "ymph"
                                       , "8) " <> f "V"  <> "ulpenoid" ]


promptRace :: HasCallStack => MsgQueue -> Cols -> MudStack ()
promptRace mq cols = wrapSend1Nl mq cols txt >> anglePrompt mq
  where
    txt = "Enter a number to make your selection, or enter the first letter" <>
          parensQuote (T.singleton 's')                                      <>
          " of the name of a race to learn more."


-- ==================================================


interpRace :: HasCallStack => NewCharBundle -> Interp
interpRace _ "" (NoArgs _ mq cols) = multiWrapSend mq cols raceTxt >> promptRace mq cols
interpRace ncb@(NewCharBundle _ s _) (T.toLower -> cn) (NoArgs i mq cols) = case cn of
  "1" -> helper Dwarf
  "2" -> helper Elf
  "3" -> helper Felinoid
  "4" -> helper Hobbit
  "5" -> helper Human
  "6" -> helper Lagomorph
  "7" -> helper Nymph
  "8" -> helper Vulpenoid
  _   -> case [ x | x <- map pp (allValues :: [Race]), cn `T.isPrefixOf` x ] of
    (raceName:_) -> readRaceHelp raceName >>= multiWrapSend mq cols . T.lines >> promptRace mq cols
    _            -> sorryRace mq cols cn
  where
    helper r              = do tweaks [ pcTbl     .ind i.race       .~ r
                                      , mobTbl    .ind i.knownLangs .~ pure (raceToLang r)
                                      , pickPtsTbl.ind i            .~ initPickPts ]
                               blankLine mq
                               settings <- getServerSettings
                               send mq . nl . T.unlines . parseWrapXform settings cols . mkPickPtsIntroTxt $ s
                               promptPickPts i mq
                               setInterp i . Just . interpPickPts $ ncb
    readRaceHelp raceName = let f = (</> T.unpack raceName) <$> mkMudFilePath raceDirFun
                            in liftIO (T.readFile =<< f) |&| try >=> eitherRet handler
      where
        handler e = do fileIOExHandler "interpRace readRaceHelp" e
                       return . helpFileErrorMsg . dblQuote $ raceName
interpRace _ cn ActionParams { .. } = sorryRace plaMsgQueue plaCols . T.unwords $ cn : args


sorryRace :: HasCallStack => MsgQueue -> Cols -> Text -> MudStack ()
sorryRace mq cols t = sequence_ [ multiWrapSend mq cols $ sorryWut t : "" : raceTxt, promptRace mq cols ]


mkPickPtsIntroTxt :: HasCallStack => Sing -> Text
mkPickPtsIntroTxt s = T.unlines . map (lSpcs <>) $ ts
  where
    ts = [ "Next we'll assign points to " <> s <> "'s attributes."
         , "Characters have 5 attributes, each measuring innate talent in a given area. 10 (the minimum value) represents a staggering lack of talent, while 100 (the maximum value) represents near-supernatural talent. 50 represents an average degree of talent."
         , "You have a pool of " <> showTxt initPickPts <> " points to assign to your attributes as you wish. To add points to an attribute, type the first letter of the attribute name, immediately followed by + and the number of points to add. For example, to add 10 to your Strength, type " <> colorWith quoteColor "s+10" <> ". To subtract points, use - instead of +, as in " <> prd (colorWith quoteColor "s-10")
         , "You can specify multiple additions/subtractions on a single line. Simply separate them with a spaces, like so: " <> prd (colorWith quoteColor "s-10 d+10 h+5")
         , "When you are finished assigning points, type " <> colorWith quoteColor (T.singleton 'q') <> " to quit and move on." ]


promptPickPts :: HasCallStack => Id -> MsgQueue -> MudStack ()
promptPickPts i mq = showAttribs i mq >> anglePrompt mq


showAttribs :: HasCallStack => Id -> MsgQueue -> MudStack ()
showAttribs i mq = getState >>= \ms -> multiSend mq . footer ms . map helper . getBaseAttribTuples i $ ms
  where
    helper = f . \case
      (St, x) -> ('S', "trength ", x)
      (Dx, x) -> ('D', "exterity", x)
      (Ht, x) -> ('H', "ealth   ", x)
      (Ma, x) -> ('M', "agic    ", x)
      (Ps, x) -> ('P', "sionics ", x)
    f (c, txt, x) = T.concat [ colorWith abbrevColor . T.singleton $ c, txt, " ", showTxt x ]
    footer ms     = (++ pure (msg <> hint))
      where
        pts  = getPickPts i ms
        msg  = nlPrefix $ showTxt pts <> " points remaining."
        hint = isZero pts |?| (" Type " <> colorWith quoteColor (T.singleton 'q') <> " to quit and move on.")


-- ==================================================


interpPickPts :: HasCallStack => NewCharBundle -> Interp
interpPickPts _   ""               (NoArgs' i mq        ) = promptPickPts i mq
interpPickPts ncb (T.toLower ->cn) (Lower   i mq cols as) = getState >>= \ms ->
    let pts = getPickPts i ms in if
      | cn `T.isPrefixOf` "quit" -> if isZero pts
        then promptDesc ncb i mq cols
        else wrapSend mq cols sorryInterpPickPtsQuit >> anglePrompt mq
      | otherwise -> helper |&| modifyState >=> \msgs -> multiWrapSend mq cols msgs >> promptPickPts i mq
  where
    helper ms = foldl' assignPts (ms, []) $ cn : as
    assignPts a@(ms, msgs) arg = let pts = getPickPts i ms in if
      | T.length arg < 3                                               -> sorry
      | op <- T.head . T.tail $ arg, liftA2 (&&) (/= '+') (/= '-') op  -> sorry
      | otherwise ->
          let { (c, rest) = headTail arg; (op, amt) = headTail rest }
          in if c `notElem` ("sdhmp" :: String)
            then sorry
            else let (attribTxt, x, setter) = procAttribChar i ms c
                 in case reads . T.unpack $ amt :: [(Int, String)] of
                   [(y, "")] | y < 0    -> sorryHelper sorryWtf
                             | isZero y -> a
                             | otherwise -> case op of
                               '+' | isZero pts -> sorryHelper sorryInterpPickPtsPts
                                   | x == 100   -> sorryHelper . sorryInterpPickPtsMax $ attribTxt
                                   | x' <- (x + (y `min` pts)) `min` 100
                                   , y' <- x' - x
                                   -> ( upd ms [ mobTbl.ind i.setter +~ y', pickPtsTbl.ind i -~ y' ]
                                      , msgs <> (pure . T.concat $ [ "Added "
                                                                   , showTxt y'
                                                                   , " point"
                                                                   , pts > 1 |?| "s"
                                                                   , " to "
                                                                   , attribTxt
                                                                   , "." ]) )
                               '-' | x == 10 -> sorryHelper . sorryInterpPickPtsMin $ attribTxt
                                   | x' <- (x - y) `max` 10
                                   , y' <- x - x'
                                   -> ( upd ms [ mobTbl.ind i.setter -~ y', pickPtsTbl.ind i +~ y' ]
                                      , msgs <> (pure . T.concat $ [ "Subtracted "
                                                                   , showTxt y'
                                                                   , " point"
                                                                   , pts > 1 |?| "s"
                                                                   , " from "
                                                                   , attribTxt
                                                                   , "." ]) )
                               _   -> pmf "interpPickPts assignPts" op
                   _ -> sorry
      where
        sorry       = sorryHelper . sorryWut $ arg
        sorryHelper = (ms, ) . (msgs <>) . pure
interpPickPts _ _ p = pmf "interpPickPts" p


promptDesc :: HasCallStack => NewCharBundle -> Id -> MsgQueue -> Cols -> MudStack ()
promptDesc ncb@(NewCharBundle _ s _) i mq cols = mkHimHer . getSex i <$> getState >>= \himHer -> do
    blankLine mq
    let msgs = [ lSpcs <> "Next you'll write a description of "
               , s
               , ", which others will see when they look at "
               , himHer
               , ". Your description must adhere to the following rules:" ]
    settings <- getServerSettings
    send mq . T.unlines . parseWrapXform settings cols . T.concat $ msgs
    send mq . T.unlines . concat . wrapLines cols . T.lines $ descRulesMsg
    pause i mq . Just . descHelper ncb i mq $ cols


procAttribChar :: HasCallStack => Id -> MudState -> Char -> (Text, Int, ASetter Mob Mob Int Int)
procAttribChar i ms = \case 's' -> ("Strength",  getBaseSt i ms, st)
                            'd' -> ("Dexterity", getBaseDx i ms, dx)
                            'h' -> ("Health",    getBaseHt i ms, ht)
                            'm' -> ("Magic",     getBaseMa i ms, ma)
                            'p' -> ("Psionics",  getBasePs i ms, ps)
                            c   -> pmf "procAttribChar" c


-- ==================================================


promptReadymadePC :: HasCallStack => MsgQueue -> Cols -> MudStack ()
promptReadymadePC mq cols = multiWrapSend1Nl mq cols ts >> anglePrompt mq
  where
    ts  = "" : (readymadeTxt ++ pure mempty ++ pure readymadePromptTxt)


readymadeTxt :: [Text]
readymadeTxt | f <- colorWith abbrevColor = [ "You may choose from one of the following readymade characters:"
                                            , "1) " <> f "D" <> "warf warrior"
                                            , "2) " <> f "F" <> "elinoid thief"
                                            , "3) " <> f "L" <> "agomoprh psionicist"
                                            , "4) " <> f "N" <> "ymph mage"
                                            , "5) " <> f "V" <> "ulpenoid warrior" ]


readymadePromptTxt :: Text
readymadePromptTxt = "Enter a number to make your selection, or enter the first letter of the name of a race to learn more."


interpReadymadePC :: HasCallStack => NewCharBundle -> Interp
interpReadymadePC _   "" (NoArgs _ mq cols) = promptRetryReadymadePC mq cols
interpReadymadePC ncb cn (NoArgs i mq cols) = case cn of
  "1" -> readymadeDwarf i >> next
  t   -> sequence_ [ wrapSend1Nl mq cols . sorryWut $ t, promptReadymadePC mq cols ]
  where
    next = promptDesc ncb i mq cols
interpReadymadePC _ _ ActionParams { .. } = promptRetryReadymadePC plaMsgQueue plaCols


promptRetryReadymadePC :: HasCallStack => MsgQueue -> Cols -> MudStack ()
promptRetryReadymadePC mq cols = wrapSend1Nl mq cols readymadePromptTxt >> anglePrompt mq


-- ==================================================


descHelper :: HasCallStack => NewCharBundle -> Id -> MsgQueue -> Cols -> MudStack ()
descHelper ncb i mq cols = getServerSettings >>= \s -> do writeMsg mq . InacSecs $ maxInacSecsCompose
                                                          send mq . nl . T.unlines . parseWrapXform s cols $ enterDescMsg
                                                          setDescInterpHelper ncb i mq cols


setDescInterpHelper :: HasCallStack => NewCharBundle -> Id -> MsgQueue -> Cols -> MudStack ()
setDescInterpHelper ncb i mq cols = setInterp i . Just . interpMutliLine (descEntered ncb i mq cols) $ []


descEntered :: HasCallStack => NewCharBundle -> Id -> MsgQueue -> Cols -> [Text] -> MudStack ()
descEntered ncb i mq cols desc = case spaces . dropBlanks . map T.strip $ desc of
  ""    -> wrapSend mq cols "Your description may not be blank." >> promptRetryDesc ncb i mq cols
  desc' -> do blankLine      mq
              wrapSend1Nl    mq cols "You entered:"
              wrapSend       mq cols desc'
              wrapSendPrompt mq cols $ "Keep this description? " <> mkYesNoChoiceTxt
              setInterp i . Just . interpConfirmDesc ncb $ desc'


promptRetryDesc :: HasCallStack => NewCharBundle -> Id -> MsgQueue -> Cols -> MudStack ()
promptRetryDesc ncb i mq cols = let endCharTxt = dblQuote . T.singleton $ multiLineEndChar in do
    wrapSend mq cols $ "Enter your description below. When you are finished, enter a " <> endCharTxt <> " on a new line."
    setDescInterpHelper ncb i mq cols


interpConfirmDesc :: HasCallStack => NewCharBundle -> Text -> Interp
interpConfirmDesc ncb desc cn (NoArgs i mq cols) = case yesNoHelper cn of
  Just True -> do
      tweak $ entTbl.ind i.entDesc .~ desc
      blankLine mq
      wrapSendPrompt mq cols "If you are a new player, could you please tell us how you discovered CurryMUD?"
      setInterp i . Just . interpDiscover $ ncb
      writeMsg mq . InacSecs $ maxInacSecs
  Just False -> blankLine mq >> promptRetryDesc ncb i mq cols
  Nothing    -> promptRetryYesNo mq cols
interpConfirmDesc _ _ _ ActionParams { plaMsgQueue, plaCols } = promptRetryYesNo plaMsgQueue plaCols


-- ==================================================


interpDiscover :: HasCallStack => NewCharBundle -> Interp
interpDiscover ncb cn params@(WithArgs i mq _ as) = (>> finishNewChar ncb params { args = [] }) $ if ()!# cn
  then sequence_ [ send mq . nlnl $ "Thank you.", withDbExHandler_ "interpDiscover" . insertDbTblDiscover =<< mkDiscoverRec ]
  else blankLine mq
  where
    mkDiscoverRec = (,) <$> liftIO mkTimestamp <*> (T.pack . getCurrHostName i <$> getState) >>= \(ts, host) ->
        return . DiscoverRec ts host . formatMsgArgs $ cn : as
interpDiscover _ _ p = pmf "interpDiscover" p


-- ==================================================


finishNewChar :: HasCallStack => NewCharBundle -> ActionParams -> MudStack ()
finishNewChar ncb@(NewCharBundle _ s pass) params@(NoArgs'' i) = do
    withDbExHandler_ "unpw" . insertDbTblUnPw . UnPwRec s $ pass
    mkRndmVector >>= \v -> helper v |&| modifyState >=> \ms@(getPla i -> p) -> do
        initPlaLog i s
        logPla "finishNewChar" i . prd $ "new character logged in from " <> views currHostName T.pack p
        handleLogin ncb True params
        notifyQuestion i ms
  where
    helper v ms | ms' <- upd ms [ pickPtsTbl.at  i        .~ Nothing -- TODO: Starting equipment. Include a light source and a holy symbol.
                                , invTbl    .ind iWelcome %~ (i `delete`)
                                , mobTbl    .ind i.rmId   .~ iCentral
                                , mobTbl    .ind i.interp .~ Nothing
                                , plaTbl    .ind i        %~ setPlaFlag IsTunedQuestion True ]
                = dup $ upd ms' [ invTbl    .ind iCentral %~ addToInv ms' (pure i)
                                , newChar i v ]
finishNewChar _ p = pmf "finishNewChar" p


notifyQuestion :: HasCallStack => Id -> MudState -> MudStack ()
notifyQuestion i ms =
    let msg      = f "A new character has arrived in Rumia."
        f        = (colorWith arrowColor "<- " <>) . colorWith questionArrivalColor
        tunedIds = uncurry (++) . getTunedQuestionIds i $ ms
    in bcastNl =<< expandEmbeddedIds ms questionChanContext =<< formatQuestion i ms (msg, tunedIds)


newChar :: HasCallStack => Id -> V.Vector Int -> MudState -> MudState
newChar i v = newXps i v . modifyMobForRace i


modifyMobForRace :: HasCallStack => Id -> MudState -> MudState
modifyMobForRace i ms = let r     = getRace i ms
                            myMob = mobTbl.ind i
                            ms'   = upd ms [ myMob.corpseWeight   .~ calcCorpseWeight   r
                                           , myMob.corpseVol      .~ calcCorpseVol      r
                                           , myMob.corpseCapacity .~ calcCorpseCapacity r ]
                        in case r of Dwarf     -> upd ms' [ myMob.st +~ 2
                                                          , myMob.ht +~ 2
                                                          , myMob.ma -~ 2
                                                          , myMob.ps -~ 2 ]
                                     -----
                                     Elf       -> upd ms' [ myMob.st -~ 2
                                                          , myMob.dx +~ 2
                                                          , myMob.ht -~ 2
                                                          , myMob.ma +~ 2 ]
                                     -----
                                     Felinoid  -> upd ms' [ myMob.dx +~ 4
                                                          , myMob.ma -~ 2
                                                          , myMob.ps -~ 2 ]
                                     -----
                                     Hobbit    -> upd ms' [ myMob.st -~ 4
                                                          , myMob.dx +~ 4
                                                          , myMob.ht -~ 2
                                                          , myMob.ma +~ 4
                                                          , myMob.ps -~ 2 ]
                                     -----
                                     Human     -> ms'
                                     -----
                                     Lagomorph -> upd ms' [ myMob.st -~ 1
                                                          , myMob.ht -~ 2
                                                          , myMob.ma -~ 4
                                                          , myMob.ps +~ 5 ]
                                     -----
                                     Nymph     -> upd ms' [ myMob.st -~ 2
                                                          , myMob.dx -~ 1
                                                          , myMob.ma +~ 5
                                                          , myMob.ps -~ 4 ]
                                     -----
                                     Vulpenoid -> upd ms' [ myMob.st +~ 4
                                                          , myMob.ht +~ 4
                                                          , myMob.ma -~ 4
                                                          , myMob.ps -~ 4 ]


{-
Is it possible for a new character to have an initial max PP so low that he/she can't perform fundamental psionic tasks
such as linking?
The lowest amount of PP that a new character can start with is 10:
15 + -4 (modifier for 10 PS) + -2 (lowest racial PS modifier) + 1 (minimum level up PP) = 10
Linking costs 10 PP. Unlinking costs 5 PP.
Creating a new channel costs 5 PP. Connecting/disconnecting costs 3 PP.
ギリギリセーフ。
-}
newXps :: HasCallStack => Id -> V.Vector Int -> MudState -> MudState
newXps i (V.toList -> (a:b:c:d:_)) ms = let x | getRace i ms == Human = 20
                                              | otherwise             = 15
                                            myMob  = mobTbl.ind i
                                            initHp = x + calcModifierHt i ms + calcLvlUpHp i ms a
                                            initMp = x + calcModifierMa i ms + calcLvlUpMp i ms b
                                            initPp = x + calcModifierPs i ms + calcLvlUpPp i ms c
                                            initFp = x + y                   + calcLvlUpFp i ms d
                                              where
                                                y = (calcModifierHt i ms + calcModifierSt i ms) `divideRound` 2
                                        in upd ms [ myMob.curHp .~ initHp
                                                  , myMob.maxHp .~ initHp
                                                  , myMob.curMp .~ initMp
                                                  , myMob.maxMp .~ initMp
                                                  , myMob.curPp .~ initPp
                                                  , myMob.maxPp .~ initPp
                                                  , myMob.curFp .~ initFp
                                                  , myMob.maxFp .~ initFp ]
newXps _ v _ = pmf "newXps" . V.length $ v


-- ==================================================


-- Returning player.
interpPW :: HasCallStack => Int -> Sing -> Interp
interpPW times targetSing cn params@(WithArgs i mq cols as) = getState >>= \ms -> do
    let (oldSing, targetId) = ((,) <$> getSing i <*> getIdForPCSing targetSing) ms
        targetPla           = getPla targetId ms
    send mq telnetShowInput >> if ()# cn || ()!# as
      then sorryPW oldSing
      else join <$> withDbExHandler "interpPW" (lookupPW targetSing) >>= \case
        Nothing -> sorryPW oldSing
        Just pw -> if uncurry validatePassword ((pw, cn) & both %~ TE.encodeUtf8)
          then let mkMsg t = T.concat [ oldSing
                                      , " has entered the correct password for "
                                      , targetSing
                                      , "; however, "
                                      , targetSing
                                      , t ]
               in if
                 | isDead targetId ms   -> sorry oldSing (sorryInterpPwDead     targetSing) . mkMsg $ " is deceased."
                 | isLoggedIn targetPla -> sorry oldSing (sorryInterpPwLoggedIn targetSing) . mkMsg $ " is already logged in."
                 | otherwise            -> (withDbExHandler "interpPW" . isPCBanned $ targetSing) >>= \case
                                             Nothing          -> dbError mq cols
                                             Just (Any True ) -> handleBanned    ms oldSing
                                             Just (Any False) -> handleNotBanned ms oldSing targetId
          else sorryPW oldSing
  where
    sorryPW oldSing            = let msg = T.concat [ oldSing, " has entered an incorrect password for ", targetSing, "." ]
                                 in sorry oldSing sorryInterpPW msg
    sorry oldSing sorryMsg msg = do logNotice "interpPW sorry" msg
                                    bcastAdmins msg
                                    liftIO . delaySecs $ 2
                                    sorryHelper oldSing sorryMsg
    sorryHelper oldSing sorryMsg = if times == 4
                                     then do let msg = "Booting " <> oldSing <> " due to excessive incorrect passwords."
                                             logNotice "interpPW sorryHelper" msg
                                             sendMsgBoot mq . Just $ sorryInterpPwBoot
                                             bcastAdmins msg
                                     else do promptRetryName mq cols sorryMsg
                                             setInterp i . Just . interpName $ succ times
    handleBanned (T.pack . getCurrHostName i -> host) oldSing = do
        let msg = T.concat [ oldSing
                           , " has been booted at login upon entering the correct password for "
                           , targetSing
                           , " "
                           , parensQuote "player is banned"
                           , "." ]
        logNotice "interpPW handleBanned" msg
        bcastAdmins . prd $ msg <> " Consider also banning host " <> dblQuote host
        sendMsgBoot mq . Just . sorryInterpPwBanned $ targetSing
    handleNotBanned ((i `getPla`) -> newPla) oldSing targetId =
        let helper ms = dup . logIn i ms oldSing (newPla^.currHostName) (newPla^.connectTime) $ targetId
        in helper |&| modifyState >=> \ms -> do
               logNotice "interpPW handleNotBanned" . T.concat $ [ oldSing
                                                                 , " has logged in as "
                                                                 , targetSing
                                                                 , ". Id "
                                                                 , showTxt targetId
                                                                 , " has been changed to "
                                                                 , showTxt i
                                                                 , "." ]
               initPlaLog i targetSing
               logPla "interpPW handleNotBanned" i . prd $ "logged in from " <> T.pack (getCurrHostName i ms)
               handleLogin (NewCharBundle oldSing targetSing "") False params { args = [] }
interpPW _ _ _ p = pmf "interpPW" p


logIn :: HasCallStack => Id -> MudState -> Sing -> HostName -> Maybe UTCTime -> Id -> MudState
logIn newId ms oldSing newHost newTime originId = upd adoptNewId [ movePC, peepNewId ]
  where
    adoptNewId = upd ms [ coinsTbl           .ind newId         .~ getCoins         originId ms
                        , coinsTbl           .at  originId      .~ Nothing
                        , durationalEffectTbl.ind newId         .~ getDurEffects    originId ms
                        , durationalEffectTbl.at  originId      .~ Nothing
                        , entTbl             .ind newId         .~ set entId newId  e
                        , entTbl             .at  originId      .~ Nothing
                        , eqTbl              .ind newId         .~ getEqMap         originId ms
                        , eqTbl              .at  originId      .~ Nothing
                        , invTbl             .ind newId         .~ getInv           originId ms
                        , invTbl             .at  originId      .~ Nothing
                        , mobTbl             .ind newId         .~ getMob           originId ms
                        , mobTbl             .at  originId      .~ Nothing
                        , pausedEffectTbl    .ind newId         .~ getPausedEffects originId ms
                        , pausedEffectTbl    .at  originId      .~ Nothing
                        , pcSingTbl          .at  (e^.sing)     ?~ newId
                        , pcSingTbl          .at  oldSing       .~ Nothing
                        , pcTbl              .ind newId         .~ getPC            originId ms
                        , pcTbl              .at  originId      .~ Nothing
                        , plaTbl             .ind newId         .~ (getPla          originId ms & currHostName .~ newHost
                                                                                                & connectTime  .~ newTime
                                                                                                & setPlaFlag IsGmcp gmcp)
                        , plaTbl             .ind newId.peepers .~ getPeepers       originId ms
                        , plaTbl             .at  originId      .~ Nothing
                        , rndmNamesMstrTbl   .ind newId         .~ getRndmNamesTbl  originId ms
                        , rndmNamesMstrTbl   .at  originId      .~ Nothing
                        , teleLinkMstrTbl    .ind newId         .~ getTeleLinkTbl   originId ms
                        , teleLinkMstrTbl    .at  originId      .~ Nothing
                        , typeTbl            .at  originId      .~ Nothing ]
      where
        e    = getEnt   originId ms
        gmcp = isGmcpId newId    ms
    movePC ms' = let newRmId = fromMaybe iLoplenkoWelcome . getLogoutRmId newId $ ms'
                 in upd ms' [ invTbl.ind iWelcome         %~ (newId    `delete`)
                            , invTbl.ind iLoggedOut       %~ (originId `delete`)
                            , invTbl.ind newRmId          %~ addToInv ms' (pure newId)
                            , mobTbl.ind newId.rmId       .~ newRmId
                            , plaTbl.ind newId.logoutRmId .~ Nothing ]
    peepNewId ms'@(getPeepers newId -> peeperIds) =
        let replaceId = (newId :) . (originId `delete`)
        in ms' & plaTbl %~ flip (foldr (\peeperId -> ind peeperId.peeping %~ replaceId)) peeperIds


handleLogin :: HasCallStack => NewCharBundle -> Bool -> ActionParams -> MudStack ()
handleLogin (NewCharBundle oldSing s _) isNew params@ActionParams { .. } = let pair = (plaMsgQueue, plaCols) in do
    logPla "handleLogin" myId "handling login."
    setLoginTime
    ms <- getState
    sendGmcpRmInfo (Just dfltZoom) myId ms
    when (isAdminId myId ms) stopInacTimer
    mapM_ (myId |&|) [ runDigesterAsync, runRegenAsync, restartPausedEffects ]
    notifyArrival
    greet
    ((>>) <$> uncurry showMotd <*> void . uncurry showDate) pair
    when (isOutside myId ms) . void . uncurry showTime $ pair
    showElapsedTime
    showRetainedMsgs
    look params
    sendDfltPrompt plaMsgQueue myId
  where
    setLoginTime = liftIO getCurrentTime >>= \ct -> do
        logPla "handleLogin setLoginTime" myId . prd $ "setting login time to " <> showTxt ct
        tweak $ plaTbl.ind myId.loginTime ?~ ct
    greet = wrapSend plaMsgQueue plaCols $ if | s == "Root" -> colorWith zingColor sudoMsg
                                              | isNew       -> "Welcome to CurryMUD, "   <> s <> "!"
                                              | otherwise   -> nlPrefix "Welcome back, " <> s <> "!"
    showElapsedTime = getState >>= \ms -> case (getLoginTime `fanUncurry` getDisconnectTime) (myId, ms) of
      (Just lt, Just dt) -> let f t = "It's been " <> t <> " since you went to sleep."
                            in showElapsedCurryTime lt dt |#| wrapSend plaMsgQueue plaCols . f
      _                  -> unit
    showRetainedMsgs = helper |&| modifyState >=> \msgs -> unless (()# msgs) $ do
       logPla "handleLogin showRetainedMsgs" myId "showing retained messages."
       let (fromPpl, others) = first (map T.tail) . partition ((== fromPersonMarker) . T.head) $ msgs
       others  |#| multiWrapSend plaMsgQueue plaCols . intersperse ""
       fromPpl |#| let m   = "message" <> case fromPpl of [_] -> ""
                                                          _   -> "s"
                       msg = "You missed the following " <> m <> " while you were away:"
                   in multiWrapSend plaMsgQueue plaCols . (msg :)
    helper ms     = let p   = getPla myId ms
                        p'  = p  & retainedMsgs    .~ []
                        ms' = ms & plaTbl.ind myId .~ p'
                    in (ms', p^.retainedMsgs)
    stopInacTimer = do logPla "handleLogin stopInacTimer" myId "stopping the inactivity timer."
                       writeMsg plaMsgQueue InacStop
    notifyArrival = getState >>= \ms -> do
        bcastOtherAdmins myId $ if isNew
          then T.concat [ s, " has arrived in Rumia ", parensQuote ("was " <> oldSing), "." ]
          else T.concat [ oldSing, " has logged in as ", s, "." ]
        bcastOthersInRm myId . nlnl . notifyArrivalMsg . mkSerializedNonStdDesig myId ms s A $ DoCap
