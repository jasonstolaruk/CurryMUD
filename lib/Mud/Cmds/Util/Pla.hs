{-# LANGUAGE LambdaCase, MonadComprehensions, MultiWayIf, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

-- This module contains helper functions used by multiple functions in "Mud.Cmds.Pla", as well as helper functions used
-- by both "Mud.Cmds.Pla" and "Mud.Cmds.ExpCmds".

module Mud.Cmds.Util.Pla ( InvWithCon
                         , IsConInRm
                         , armSubToSlot
                         , bugTypoLogger
                         , clothToSlot
                         , donMsgs
                         , dudeYou'reNaked
                         , dudeYourHandsAreEmpty
                         , findAvailSlot
                         , helperGetDropEitherCoins
                         , helperGetDropEitherInv
                         , helperPutRemEitherCoins
                         , helperPutRemEitherInv
                         , isNonStdLink
                         , isRingRol
                         , isSlotAvail
                         , linkDirToCmdName
                         , maybeSingleSlot
                         , mkCapStdDesig
                         , mkCoinsDesc
                         , mkCoinsSummary
                         , mkDropReadyBindings
                         , mkEntDescs
                         , mkEqDesc
                         , mkExitsSummary
                         , mkGetDropCoinsDesc
                         , mkGetDropInvDesc
                         , mkGetLookBindings
                         , mkInvCoinsDesc
                         , mkMaybeNthOfM
                         , mkPossPro
                         , mkPutRemBindings
                         , mkPutRemCoinsDescs
                         , mkPutRemInvDesc
                         , mkReadyMsgs
                         , mkReflexPro
                         , mkStdDesig
                         , mkThrPerPro
                         , moveReadiedItem
                         , otherHand
                         , putOnMsgs
                         , resolvePCInvCoins
                         , resolveRmInvCoins ) where

import Mud.ANSI
import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.State
import Mud.Data.State.Util.Coins
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.STM
import Mud.NameResolution
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.List (mkCountList)
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Logging as L (logPla)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow ((***))
import Control.Exception.Lifted (try)
import Control.Lens (_1, _2, _3, at, both, over, to)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (.~), (<>~), (?~), (^.))
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import Data.IntMap.Lazy ((!))
import Data.List ((\\), delete, elemIndex, find, intercalate, nub, sort)
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.Monoid ((<>), Sum(..), mempty)
import System.Directory (doesFileExist)
import qualified Data.Map.Lazy as M (toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile, writeFile)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.Pla"


-----


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Cmds.Util.Pla"


-- ==================================================


armSubToSlot :: ArmSub -> Slot
armSubToSlot = \case Head      -> HeadS
                     Torso     -> TorsoS
                     Arms      -> ArmsS
                     Hands     -> HandsS
                     LowerBody -> LowerBodyS
                     Feet      -> FeetS
                     Shield    -> undefined


-----


bugTypoLogger :: ActionParams -> WhichLog -> MudStack ()
bugTypoLogger (Msg i mq msg) wl@(pp -> wl') = getEntSing' i >>= \(ws, s) ->
    let p  = (ws^.pcTbl) ! i
        ri = p^.rmId
        r  = (ws^.rmTbl) ! ri
        helper ts = let newEntry = T.concat [ ts
                                            , " "
                                            , s
                                            , " "
                                            , parensQuote $ showText ri <> " " <> dblQuote (r^.rmName)
                                            , ": "
                                            , msg ]
                    in T.writeFile logFile =<< [ T.unlines . sort $ newEntry : cont | cont <- getLogConts ]
    in do
        logPla "bugTypoLogger" i . T.concat $ [ "logged a ", wl', ": ", msg ]
        liftIO (mkTimestamp >>= try . helper) >>= eitherRet (fileIOExHandler "bugTypoLogger")
        send mq . nlnl $ "Thank you."
        flip bcastAdmins (s <> " has logged a " <> wl' <> ".") =<< readTMVarInNWS plaTblTMVar
  where
    logFile     = case wl of BugLog  -> bugLogFile
                             TypoLog -> typoLogFile
    getLogConts = mIf (doesFileExist logFile)
                      (T.lines <$> T.readFile logFile)
                      (return [])
bugTypoLogger p wl = patternMatchFail "bugTypoLogger" [ showText p, showText wl ]


-----


clothToSlot :: Cloth -> Slot
clothToSlot = \case Shirt    -> ShirtS
                    Smock    -> SmockS
                    Coat     -> CoatS
                    Trousers -> TrousersS
                    Skirt    -> SkirtS
                    Dress    -> DressS
                    FullBody -> FullBodyS
                    Backpack -> BackpackS
                    Cloak    -> CloakS
                    _        -> undefined


-----


donMsgs :: Id -> PCDesig -> Sing -> (T.Text, Broadcast)
donMsgs = mkReadyMsgs "don" "dons"


type SndPerVerb = T.Text
type ThrPerVerb = T.Text


mkReadyMsgs :: SndPerVerb -> ThrPerVerb -> Id -> PCDesig -> Sing -> (T.Text, Broadcast)
mkReadyMsgs spv tpv i d s = (  T.concat [ "You ", spv, " the ", s, "." ]
                            , (T.concat [ serialize d, " ", tpv, " ", aOrAn s, "." ], i `delete` pcIds d) )


-----


isSlotAvail :: EqMap -> Slot -> Bool
isSlotAvail em s = em^.at s.to isNothing


findAvailSlot :: EqMap -> [Slot] -> Maybe Slot
findAvailSlot em = find (isSlotAvail em)


-----


type FromId = Id
type ToId   = Id


helperGetDropEitherCoins :: Id
                         -> PCDesig
                         -> GetOrDrop
                         -> FromId
                         -> ToId
                         -> (WorldState, [Broadcast], [T.Text])
                         -> Either [T.Text] Coins
                         -> (WorldState, [Broadcast], [T.Text])
helperGetDropEitherCoins i d god fi ti a@(ws, _, _) = \case
  Left  msgs -> a & _2 <>~ [ (msg, [i]) | msg <- msgs ]
  Right c | (fc, tc)      <- over both ((ws^.coinsTbl) !) (fi, ti)
          , ws'           <- ws & coinsTbl.at fi ?~ fc <> negateCoins c
                                & coinsTbl.at ti ?~ tc <> c
          , (bs, logMsgs) <- mkGetDropCoinsDesc i d god c
          -> a & _1 .~ ws' & _2 <>~ bs & _3 <>~ logMsgs


mkGetDropCoinsDesc :: Id -> PCDesig -> GetOrDrop -> Coins -> ([Broadcast], [T.Text])
mkGetDropCoinsDesc i d god c | bs <- mkCoinsBroadcasts c helper = (bs, extractLogMsgs i bs)
  where
    helper a cn | a == 1 =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, " ", aOrAn cn, "." ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " ", aOrAn cn, "." ], otherPCIds) ]
    helper a cn =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, " ", showText a, " ", cn, "s." ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " ", showText a, " ", cn, "s." ], otherPCIds) ]
    otherPCIds = i `delete` pcIds d


mkCoinsBroadcasts :: Coins -> (Int -> T.Text -> [Broadcast]) -> [Broadcast]
mkCoinsBroadcasts (Coins (cop, sil, gol)) f = concat . catMaybes $ [ c, s, g ]
  where
    c = cop /= 0 ? (Just . f cop $ "copper piece") :? Nothing
    s = sil /= 0 ? (Just . f sil $ "silver piece") :? Nothing
    g = gol /= 0 ? (Just . f gol $ "gold piece"  ) :? Nothing


extractLogMsgs :: Id -> [Broadcast] -> [T.Text]
extractLogMsgs i bs = [ fst b | b <- bs, snd b == [i] ]


mkGodVerb :: GetOrDrop -> Verb -> T.Text
mkGodVerb Get  SndPer = "pick up"
mkGodVerb Get  ThrPer = "picks up"
mkGodVerb Drop SndPer = "drop"
mkGodVerb Drop ThrPer = "drops"


-----


helperGetDropEitherInv :: Id
                       -> PCDesig
                       -> GetOrDrop
                       -> FromId
                       -> ToId
                       -> (WorldState, [Broadcast], [T.Text])
                       -> Either T.Text Inv
                       -> (WorldState, [Broadcast], [T.Text])
helperGetDropEitherInv i d god fi ti a@(ws, _, _) = \case
  Left  (mkBroadcast i -> b) -> a & _2 <>~ b
  Right is | (fis, tis)      <- over both ((ws^.invTbl) !) (fi, ti)
           , ws'             <- ws & invTbl.at fi ?~ fis \\ is
                                   & invTbl.at ti ?~ sortInv ws (tis ++ is)
           , (bs', logMsgs') <- mkGetDropInvDesc i ws' d god is
           -> a & _1 .~ ws' & _2 <>~ bs' & _3 <>~ logMsgs'


mkGetDropInvDesc :: Id -> WorldState -> PCDesig -> GetOrDrop -> Inv -> ([Broadcast], [T.Text])
mkGetDropInvDesc i ws d god (mkNameCountBothList i ws -> ncbs) | bs <- concatMap helper ncbs = (bs, extractLogMsgs i bs)
  where
    helper (_, c, (s, _))
      | c == 1 = [ (T.concat [ "You ",           mkGodVerb god SndPer, " the ", s,   "." ], [i])
                 , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " ", aOrAn s, "." ], otherPCIds) ]
    helper (_, c, b) =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, rest ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, rest ], otherPCIds) ]
      where
        rest = T.concat [ " ", showText c, " ", mkPlurFromBoth b, "." ]
    otherPCIds = i `delete` pcIds d


mkNameCountBothList :: Id -> WorldState -> Inv -> [(T.Text, Int, BothGramNos)]
mkNameCountBothList i ws is | ens   <- [ getEffName        i ws i' | i' <- is ]
                            , ebgns <- [ getEffBothGramNos i ws i' | i' <- is ]
                            , cs    <- mkCountList ebgns = nub . zip3 ens cs $ ebgns


-----


type NthOfM = (Int, Int)
type ToEnt  = Ent


helperPutRemEitherCoins :: Id
                        -> PCDesig
                        -> PutOrRem
                        -> Maybe NthOfM
                        -> FromId
                        -> ToId
                        -> ToEnt
                        -> (WorldState, [Broadcast], [T.Text])
                        -> Either [T.Text] Coins
                        -> (WorldState, [Broadcast], [T.Text])
helperPutRemEitherCoins i d por mnom fi ti te a@(ws, _, _) = \case
  Left  msgs -> a & _2 <>~ [ (msg, [i]) | msg <- msgs ]
  Right c | (fc, tc)      <- over both ((ws^.coinsTbl) !) (fi, ti)
          , ws'           <- ws & coinsTbl.at fi ?~ fc <> negateCoins c
                                & coinsTbl.at ti ?~ tc <> c
          , (bs, logMsgs) <- mkPutRemCoinsDescs i d por mnom c te
          -> a & _1 .~ ws' & _2 <>~ bs & _3 <>~ logMsgs


mkPutRemCoinsDescs :: Id -> PCDesig -> PutOrRem -> Maybe NthOfM -> Coins -> ToEnt -> ([Broadcast], [T.Text])
mkPutRemCoinsDescs i d por mnom c (view sing -> ts) | bs <- mkCoinsBroadcasts c helper = (bs, extractLogMsgs i bs)
  where
    helper a cn | a == 1 =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , " "
                    , aOrAn cn
                    , " "
                    , mkPorPrep por SndPer mnom ts
                    , rest ], [i])
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " "
                    , aOrAn cn
                    , " "
                    , mkPorPrep por ThrPer mnom ts
                    , rest ], otherPCIds) ]
    helper a cn =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , " "
                    , showText a
                    , " "
                    , cn
                    , "s "
                    , mkPorPrep por SndPer mnom ts
                    , rest ], [i])
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " "
                    , showText a
                    , " "
                    , cn
                    , "s "
                    , mkPorPrep por ThrPer mnom ts
                    , rest ], otherPCIds) ]
    rest       = onTheGround mnom <> "."
    otherPCIds = i `delete` pcIds d


mkPorVerb :: PutOrRem -> Verb -> T.Text
mkPorVerb Put SndPer = "put"
mkPorVerb Put ThrPer = "puts"
mkPorVerb Rem SndPer = "remove"
mkPorVerb Rem ThrPer = "removes"


mkPorPrep :: PutOrRem -> Verb -> Maybe NthOfM -> Sing -> T.Text
mkPorPrep Put SndPer Nothing       = ("in the "   <>)
mkPorPrep Put SndPer (Just (n, m)) = ("in the "   <>) . (descNthOfM n m <>)
mkPorPrep Rem SndPer Nothing       = ("from the " <>)
mkPorPrep Rem SndPer (Just (n, m)) = ("from the " <>) . (descNthOfM n m <>)
mkPorPrep Put ThrPer Nothing       = ("in "       <>) . aOrAn
mkPorPrep Put ThrPer (Just (n, m)) = ("in the "   <>) . (descNthOfM n m <>)
mkPorPrep Rem ThrPer Nothing       = ("from "     <>) . aOrAn
mkPorPrep Rem ThrPer (Just (n, m)) = ("from the " <>) . (descNthOfM n m <>)


descNthOfM :: Int -> Int -> T.Text
descNthOfM 1 1 = ""
descNthOfM n _ = mkOrdinal n <> " "


onTheGround :: Maybe NthOfM -> T.Text
onTheGround Nothing = ""
onTheGround _       = " on the ground"


-----


helperPutRemEitherInv :: Id
                      -> PCDesig
                      -> PutOrRem
                      -> Maybe NthOfM
                      -> FromId
                      -> ToId
                      -> ToEnt
                      -> (WorldState, [Broadcast], [T.Text])
                      -> Either T.Text Inv
                      -> (WorldState, [Broadcast], [T.Text])
helperPutRemEitherInv i d por mnom fi ti te a@(ws, bs, _) = \case
  Left  (mkBroadcast i -> b) -> a & _2 <>~ b
  Right is | (is', bs')      <- if ti `elem` is
                                  then (filter (/= ti) is, bs ++ [sorry])
                                  else (is, bs)
           , (fis, tis)      <- over both ((ws^.invTbl) !) (fi, ti)
           , ws'             <- ws & invTbl.at fi ?~ fis \\ is'
                                   & invTbl.at ti ?~ (sortInv ws . (tis ++) $ is')
           , (bs'', logMsgs) <- mkPutRemInvDesc i ws' d por mnom is' te
           -> a & _1 .~ ws' & _2 .~ (bs' ++ bs'') & _3 <>~ logMsgs
  where
    sorry = ("You can't put the " <> te^.sing <> " inside itself.", [i])


mkPutRemInvDesc :: Id -> WorldState -> PCDesig -> PutOrRem -> Maybe NthOfM -> Inv -> ToEnt -> ([Broadcast], [T.Text])
mkPutRemInvDesc i ws d por mnom is (view sing -> ts) | bs <- concatMap helper . mkNameCountBothList i ws $ is
                                                     = (bs, extractLogMsgs i bs)
  where
    helper (_, c, (s, _)) | c == 1 =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , " "
                    , withArticle
                    , " "
                    , mkPorPrep por SndPer mnom ts
                    , rest ], [i])
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " "
                    , aOrAn s
                    , " "
                    , mkPorPrep por ThrPer mnom ts
                    , rest ], otherPCIds) ]
      where
        withArticle | por == Put = "the " <> s
                    | otherwise  = aOrAn s
    helper (_, c, b) =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , " "
                    , showText c
                    , " "
                    , mkPlurFromBoth b
                    , " "
                    , mkPorPrep por SndPer mnom ts
                    , rest ], [i])
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " "
                    , showText c
                    , " "
                    , mkPlurFromBoth b
                    , " "
                    , mkPorPrep por ThrPer mnom ts
                    , rest ], otherPCIds) ]
    rest       = onTheGround mnom <> "."
    otherPCIds = i `delete` pcIds d


-----


isRingRol :: RightOrLeft -> Bool
isRingRol = \case R -> False
                  L -> False
                  _ -> True


-----


maybeSingleSlot :: EqMap -> Slot -> Maybe Slot
maybeSingleSlot em s = toMaybe (isSlotAvail em s) s


-----


mkCapStdDesig :: Id -> WorldState -> (PCDesig, Sing, PC, Id, Inv)
mkCapStdDesig i ws | (view sing -> s)    <- (ws^.entTbl) ! i
                   , p@(view rmId -> ri) <- (ws^.pcTbl)  ! i
                   , ris                 <- (ws^.invTbl) ! ri = (mkStdDesig i ws s True ris, s, p, ri, ris)


mkStdDesig :: Id -> WorldState -> Sing -> Bool -> Inv -> PCDesig
mkStdDesig i ws s ic ris = StdDesig { stdPCEntSing = Just s
                                    , isCap        = ic
                                    , pcEntName    = mkUnknownPCEntName i ws
                                    , pcId         = i
                                    , pcIds        = findPCIds ws ris }


-----


mkCoinsDesc :: Cols -> Coins -> T.Text
mkCoinsDesc cols (Coins (cop, sil, gol)) =
    T.unlines . intercalate [""] . map (wrap cols) . filter (not . T.null) $ [ Sum cop |!| copDesc
                                                                             , Sum sil |!| silDesc
                                                                             , Sum gol |!| golDesc ]
  where -- TODO: Come up with good descriptions.
    copDesc = "The copper piece is round and shiny."
    silDesc = "The silver piece is round and shiny."
    golDesc = "The gold piece is round and shiny."


-----


mkDropReadyBindings :: Id -> WorldState -> (PCDesig, Id, Inv, Coins)
mkDropReadyBindings i ws | (d, _, _, ri, _) <- mkCapStdDesig i ws
                         , is               <- (ws^.invTbl)   ! i
                         , c                <- (ws^.coinsTbl) ! i = (d, ri, is, c)


-----


mkEntDescs :: Id -> Cols -> WorldState -> Inv -> T.Text
mkEntDescs i cols ws eis = T.intercalate "\n" [ mkEntDesc i cols ws (ei, e) | ei <- eis, let e = (ws^.entTbl) ! ei ]


mkEntDesc :: Id -> Cols -> WorldState -> (Id, Ent) -> T.Text
mkEntDesc i cols ws (ei@(((ws^.typeTbl) !) -> t), e@(views entDesc (wrapUnlines cols) -> ed)) =
    case t of ConType ->                 (ed <>) . mkInvCoinsDesc i cols ws ei $ e
              MobType ->                 (ed <>) . mkEqDesc       i cols ws ei   e $ t
              PCType  -> (pcHeader <>) . (ed <>) . mkEqDesc       i cols ws ei   e $ t
              _       -> ed
  where
    pcHeader = wrapUnlines cols mkPCDescHeader
    mkPCDescHeader | (pp *** pp -> (s, r)) <- getSexRace ei ws = T.concat [ "You see a ", s, " ", r, "." ]


mkInvCoinsDesc :: Id -> Cols -> WorldState -> Id -> Ent -> T.Text
mkInvCoinsDesc i cols ws descI (view sing -> descS)
  | descIs <- (ws^.invTbl)   ! descI
  , descC  <- (ws^.coinsTbl) ! descI = case (not . null $ descIs, descC /= mempty) of
    (False, False) -> wrapUnlines cols (descI == i ? dudeYourHandsAreEmpty :? "The " <> descS <> " is empty.")
    (True,  False) -> header <> mkEntsInInvDesc i cols ws descIs
    (False, True ) -> header <>                                     mkCoinsSummary cols descC
    (True,  True ) -> header <> mkEntsInInvDesc i cols ws descIs <> mkCoinsSummary cols descC
  where
    header | descI == i = nl "You are carrying:"
           | otherwise  = wrapUnlines cols $ "The " <> descS <> " contains:"


dudeYourHandsAreEmpty :: T.Text
dudeYourHandsAreEmpty = "You aren't carrying anything."


mkEntsInInvDesc :: Id -> Cols -> WorldState -> Inv -> T.Text
mkEntsInInvDesc i cols ws = T.unlines . concatMap (wrapIndent ind cols . helper) . mkStyledName_Count_BothList i ws
  where
    helper (pad ind -> en, c, (s, _)) | c == 1 = en <> "1 " <> s
    helper (pad ind -> en, c, b     )          = T.concat [ en, showText c, " ", mkPlurFromBoth b ]
    ind = 11


mkStyledName_Count_BothList :: Id -> WorldState -> Inv -> [(T.Text, Int, BothGramNos)]
mkStyledName_Count_BothList i ws is | ens   <- styleAbbrevs DoBracket [ getEffName        i ws i' | i' <- is ]
                                    , ebgns <-                        [ getEffBothGramNos i ws i' | i' <- is ]
                                    , cs    <- mkCountList ebgns = nub . zip3 ens cs $ ebgns


mkCoinsSummary :: Cols -> Coins -> T.Text
mkCoinsSummary cols c = helper . zipWith mkNameAmt coinNames . mkListFromCoins $ c
  where
    mkNameAmt cn a = Sum a |!| showText a <> " " <> bracketQuote (abbrevColor <> cn <> dfltColor)
    helper         = T.unlines . wrapIndent 2 cols . T.intercalate ", " . filter (not . T.null)


mkEqDesc :: Id -> Cols -> WorldState -> Id -> Ent -> Type -> T.Text
mkEqDesc i cols ws descI (view sing -> descS) descT | descs <- descI == i ? mkDescsSelf :? mkDescsOther =
    case descs of [] -> none
                  _  -> (header <>) . T.unlines . concatMap (wrapIndent 15 cols) $ descs
  where
    mkDescsSelf | (ss, is) <- unzip . M.toList $ (ws^.eqTbl) ! i
                , sns      <- [ pp s                  | s  <- ss ]
                , es       <- [ (ws^.entTbl) ! ei     | ei <- is ]
                , ess      <- [ e^.sing               | e  <- es ]
                , ens      <- [ fromJust $ e^.entName | e  <- es ]
                , styleds  <- styleAbbrevs DoBracket ens = map helper . zip3 sns ess $ styleds
      where
        helper (T.breakOn " finger" -> (sn, _), es, styled) = T.concat [ parensPad 15 sn, es, " ", styled ]
    mkDescsOther | (ss, is) <- unzip . M.toList $ (ws^.eqTbl) ! descI
                 , sns      <- [ pp s | s <- ss ]
                 , ess      <- [ e^.sing | ei <- is, let e = (ws^.entTbl) ! ei ] = zipWith helper sns ess
      where
        helper (T.breakOn " finger" -> (sn, _)) es = parensPad 15 sn <> es
    none = wrapUnlines cols $ if
      | descI == i      -> dudeYou'reNaked
      | descT == PCType -> parsePCDesig i ws $ d <> " doesn't have anything readied."
      | otherwise       -> theOnLowerCap descS   <> " doesn't have anything readied."
    header = wrapUnlines cols $ if
      | descI == i      -> "You have readied the following equipment:"
      | descT == PCType -> parsePCDesig i ws $ d <> " has readied the following equipment:"
      | otherwise       -> theOnLowerCap descS   <> " has readied the following equipment:"
    d = mkSerializedNonStdDesig descI ws descS The


dudeYou'reNaked :: T.Text
dudeYou'reNaked = "You don't have anything readied. You're naked!"


-----


mkExitsSummary :: Cols -> Rm -> T.Text
mkExitsSummary cols (view rmLinks -> rls)
  | stdNames    <- [ exitsColor <> rl^.linkDir.to linkDirToCmdName <> dfltColor | rl <- rls
                                                                                , not . isNonStdLink $ rl ]
  , customNames <- [ exitsColor <> rl^.linkName                    <> dfltColor | rl <- rls
                                                                                ,       isNonStdLink   rl ]
  = T.unlines . wrapIndent 2 cols . ("Obvious exits: " <>) . summarize stdNames $ customNames
  where
    summarize []  []  = "None!"
    summarize std cus = T.intercalate ", " . (std ++) $ cus


linkDirToCmdName :: LinkDir -> CmdName
linkDirToCmdName North     = "n"
linkDirToCmdName Northeast = "ne"
linkDirToCmdName East      = "e"
linkDirToCmdName Southeast = "se"
linkDirToCmdName South     = "s"
linkDirToCmdName Southwest = "sw"
linkDirToCmdName West      = "w"
linkDirToCmdName Northwest = "nw"
linkDirToCmdName Up        = "u"
linkDirToCmdName Down      = "d"


isNonStdLink :: RmLink -> Bool
isNonStdLink (NonStdLink {}) = True
isNonStdLink _               = False


-----


mkGetLookBindings :: Id -> WorldState -> (PCDesig, Id, Inv, Inv, Coins)
mkGetLookBindings i ws | (d, _, _, ri, ris@((i `delete`) -> ris')) <- mkCapStdDesig i ws
                       , rc                                        <- (ws^.coinsTbl) ! ri = (d, ri, ris, ris', rc)


-----


type IsConInRm  = Bool
type InvWithCon = Inv


mkMaybeNthOfM :: IsConInRm -> WorldState -> Id -> Ent -> InvWithCon -> Maybe NthOfM
mkMaybeNthOfM icir ws i (view sing -> s) is = guard icir >> (return . helper . dup $ matches)
  where
    helper  = succ . fromJust . elemIndex i *** length
    matches = filter (\i' -> views sing (== s) $ (ws^.entTbl) ! i') is


-----


mkPossPro :: Sex -> T.Text
mkPossPro Male   = "his"
mkPossPro Female = "her"
mkPossPro s      = patternMatchFail "mkPossPro" [ showText s ]


-----


mkPutRemBindings :: Id -> WorldState -> Args -> (PCDesig, Inv, Coins, Inv, Coins, ConName, Args)
mkPutRemBindings i ws as = let (d, _, _, ri, (i `delete`) -> ris) = mkCapStdDesig i ws
                               pis                                = (ws^.invTbl) ! i
                               (pc, rc)                           = over both ((ws^.coinsTbl) !) (i, ri)
                               cn                                 = last as
                               (init -> argsWithoutCon)           = case as of
                                                                      [_, _] -> as
                                                                      _      -> (++ [cn]) . nub . init $ as
                           in (d, ris, rc, pis, pc, cn, argsWithoutCon)


-----


mkReflexPro :: Sex -> T.Text
mkReflexPro Male   = "himself"
mkReflexPro Female = "herself"
mkReflexPro s      = patternMatchFail "mkReflexPro" [ showText s ]


-----


mkThrPerPro :: Sex -> T.Text
mkThrPerPro Male   = "he"
mkThrPerPro Female = "she"
mkThrPerPro s      = patternMatchFail "mkThrPerPro" [ showText s ]


-----


moveReadiedItem :: Id
                -> (WorldState, [Broadcast], [T.Text])
                -> EqMap
                -> Slot
                -> Id
                -> (T.Text, Broadcast)
                -> (WorldState, [Broadcast], [T.Text])
moveReadiedItem i a@(ws, _, _) em s ei (msg, b)
  | is  <- (ws^.invTbl) ! i
  , ws' <- ws & invTbl.at i ?~ filter (/= ei) is
              & eqTbl.at  i ?~ (em & at s ?~ ei)
  , bs  <- mkBroadcast i msg ++ [b]
  = a & _1 .~ ws' & _2 <>~ bs & _3 <>~ [msg]


-----


otherHand :: Hand -> Hand
otherHand RHand  = LHand
otherHand LHand  = RHand
otherHand NoHand = NoHand


-----


putOnMsgs :: Id -> PCDesig -> Sing -> (T.Text, Broadcast)
putOnMsgs = mkReadyMsgs "put on" "puts on"


-----


resolvePCInvCoins :: Id -> WorldState -> Args -> Inv -> Coins -> ([Either T.Text Inv], [Either [T.Text] Coins])
resolvePCInvCoins i ws as is c | (gecrs, miss, rcs) <- resolveEntCoinNames i ws as is c
                               , eiss               <- zipWith (curry procGecrMisPCInv) gecrs miss
                               , ecs                <- map procReconciledCoinsPCInv rcs = (eiss, ecs)


-----


resolveRmInvCoins :: Id -> WorldState -> Args -> Inv -> Coins -> ([Either T.Text Inv], [Either [T.Text] Coins])
resolveRmInvCoins i ws as is c | (gecrs, miss, rcs) <- resolveEntCoinNames i ws as is c
                               , eiss               <- zipWith (curry procGecrMisRm) gecrs miss
                               , ecs                <- map procReconciledCoinsRm rcs = (eiss, ecs)
