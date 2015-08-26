{-# LANGUAGE FlexibleContexts, LambdaCase, MonadComprehensions, MultiWayIf, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

-- This module contains helper functions used by multiple functions in "Mud.Cmds.Pla", as well as helper functions used
-- by both "Mud.Cmds.Pla" and "Mud.Cmds.ExpCmds".

module Mud.Cmds.Util.Pla ( armSubToSlot
                         , bugTypoLogger
                         , clothToSlot
                         , donMsgs
                         , dudeYou'reNaked
                         , dudeYou'reScrewed
                         , dudeYourHandsAreEmpty
                         , findAvailSlot
                         , helperDropEitherInv
                         , helperGetDropEitherCoins
                         , helperGetEitherInv
                         , helperLinkUnlink
                         , helperPutRemEitherCoins
                         , helperPutRemEitherInv
                         , InvWithCon
                         , IsConInRm
                         , isDblLinked
                         , isLinked
                         , isNonStdLink
                         , isRingRol
                         , isSlotAvail
                         , linkDirToCmdName
                         , maybeSingleSlot
                         , mkCoinsDesc
                         , mkCoinsSummary
                         , mkEntDescs
                         , mkEqDesc
                         , mkExitsSummary
                         , mkInvCoinsDesc
                         , mkMaybeNthOfM
                         , mkPutRemoveBindings
                         , mkReadyMsgs
                         , moveReadiedItem
                         , noCoinsInEq
                         , noContainersHere
                         , noOneHere
                         , otherHand
                         , putOnMsgs
                         , resolvePCInvCoins
                         , resolveRmInvCoins
                         , sorryConInEq
                         , sorryEquipInvLook
                         , sorryIncog ) where

import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Coins
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.Misc.Database
import Mud.Misc.NameResolution
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Padding
import Mud.Util.List
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import Prelude hiding (pi)
import qualified Mud.Misc.Logging as L (logPla, logPlaOut)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow ((***), first)
import Control.Lens (_1, _2, _3, _4, at, both, each, to, view, views)
import Control.Lens.Operators ((%~), (&), (.~), (<>~), (?~), (^.))
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\), delete, elemIndex, find, foldl', intercalate, nub)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>), Sum(..))
import qualified Data.IntMap.Lazy as IM (keys)
import qualified Data.Map.Lazy as M (notMember, toList)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.Pla"


-----


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Cmds.Util.Pla"


logPlaOut :: T.Text -> Id -> [T.Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Cmds.Util.Pla"


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
bugTypoLogger (Msg' i mq msg) wl = getState >>= \ms ->
    let s     = getSing i  ms
        ri    = getRmId i  ms
        mkLoc = parensQuote (showText ri) <> " " <> getRm ri ms ^.rmName
    in liftIO mkTimestamp >>= \ts -> do
        sequence_ $ case wl of BugLog  -> let b = BugRec ts s mkLoc msg True
                                          in [ withDbExHandler_ "bugTypoLogger" . insertDbTblBug $ b
                                             , bcastOtherAdmins i $ s <> " has logged a bug: "  <> pp b ]
                               TypoLog -> let t = TypoRec ts s mkLoc msg True
                                          in [ withDbExHandler_ "bugTypoLogger" . insertDbTblTypo $ t
                                             , bcastOtherAdmins i $ s <> " has logged a typo: " <> pp t ]
        send mq . nlnl $ "Thank you."
        logPla "bugTypoLogger" i . T.concat $ [ "logged a ", showText wl, ": ", msg ]
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


dudeYou'reScrewed :: T.Text
dudeYou'reScrewed = "You aren't carrying anything, and you don't have anything readied. You're naked!"


-----


type FromId = Id
type ToId   = Id


helperDropEitherInv :: Id
                    -> PCDesig
                    -> FromId
                    -> ToId
                    -> (MudState, [Broadcast], [T.Text])
                    -> Either T.Text Inv
                    -> (MudState, [Broadcast], [T.Text])
helperDropEitherInv i d fi ti a@(ms, _, _) = \case
  Left  (mkBroadcast i -> b) -> a & _2 <>~ b
  Right is                   -> let (bs, logMsgs) = mkGetDropInvDesc i ms d Drop is
                                in a & _1.invTbl.ind fi %~  (\\ is)
                                     & _1.invTbl.ind ti %~  (sortInv ms . (++ is))
                                     & _2               <>~ bs
                                     & _3               <>~ logMsgs


mkGetDropInvDesc :: Id -> MudState -> PCDesig -> GetOrDrop -> Inv -> ([Broadcast], [T.Text])
mkGetDropInvDesc i ms d god (mkNameCountBothList i ms -> ncbs) =
    let bs = concatMap helper ncbs in (bs, extractLogMsgs i bs)
  where
    helper (_, c, (s, _)) | c == 1 =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, " the ", s,   "." ], pure i)
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " ", aOrAn s, "." ], otherPCIds) ]
    helper (_, c, b) =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, rest ], pure i)
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, rest ], otherPCIds) ]
      where
        rest = T.concat [ " ", showText c, " ", mkPlurFromBoth b, "." ]
    otherPCIds = i `delete` pcIds d


mkNameCountBothList :: Id -> MudState -> Inv -> [(T.Text, Int, BothGramNos)]
mkNameCountBothList i ms targetIds = let ens   = [ getEffName        i ms targetId | targetId <- targetIds ]
                                         cs    = mkCountList ebgns
                                         ebgns = [ getEffBothGramNos i ms targetId | targetId <- targetIds ]
                                     in nub . zip3 ens cs $ ebgns


extractLogMsgs :: Id -> [Broadcast] -> [T.Text]
extractLogMsgs i bs = [ msg | (msg, targetIds) <- bs, targetIds == pure i ]


mkGodVerb :: GetOrDrop -> Verb -> T.Text
mkGodVerb Get  SndPer = "pick up"
mkGodVerb Get  ThrPer = "picks up"
mkGodVerb Drop SndPer = "drop"
mkGodVerb Drop ThrPer = "drops"


-----


helperGetDropEitherCoins :: Id
                         -> PCDesig
                         -> GetOrDrop
                         -> FromId
                         -> ToId
                         -> (MudState, [Broadcast], [T.Text])
                         -> [Either [T.Text] Coins]
                         -> (MudState, [Broadcast], [T.Text])
helperGetDropEitherCoins i d god fi ti (origMs, origBs, origMsgs) ecs =
    let (finalMs, finalBs, finalMsgs, canCoins) = foldl' helper (origMs, origBs, origMsgs, mempty) ecs
    in (finalMs, finalBs ++ mkGetDropCoinsDescOthers i d god canCoins, finalMsgs)
  where
    helper a@(ms, _, _, _) = \case
      Left  msgs -> a & _2 <>~ (mkBroadcast i . T.concat $ msgs)
      Right c    -> let (can, can't)  = case god of Get  -> partitionByEnc c
                                                    Drop -> (c, mempty)
                        (bs, logMsgs) = mkGetDropCoinsDescSelf i god can
                    in a & _1.coinsTbl.ind fi %~  (<> negateCoins can)
                         & _1.coinsTbl.ind ti %~  (<>             can)
                         & _2                 <>~ bs ++ mkCan'tGetCoinsDesc i can't
                         & _3                 <>~ logMsgs
                         & _4                 <>~ can
      where
        partitionByEnc c = let maxEnc           = calcMaxEnc i ms
                               w                = calcWeight i ms
                               noOfCoins        = sum . coinsToList $ c
                               totalCoinsWeight = noOfCoins * coinWeight
                           in if w + totalCoinsWeight <= maxEnc
                             then (c, mempty)
                             else let availWeight  = maxEnc - w
                                      canNoOfCoins = availWeight `quot` coinWeight
                                  in mkCanCan't c canNoOfCoins
        mkCanCan't (Coins (c, 0, 0)) n = (Coins (n, 0, 0), Coins (c - n, 0,     0    ))
        mkCanCan't (Coins (0, s, 0)) n = (Coins (0, n, 0), Coins (0,     s - n, 0    ))
        mkCanCan't (Coins (0, 0, g)) n = (Coins (0, 0, n), Coins (0,     0,     g - n))
        mkCanCan't c                 n = patternMatchFail "helperGetDropEitherCoins mkCanCan't" [ showText c
                                                                                                , showText n ]


mkGetDropCoinsDescOthers :: Id -> PCDesig -> GetOrDrop -> Coins -> [Broadcast]
mkGetDropCoinsDescOthers i d god c =
  c |!| [ (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " ", aCoinSomeCoins c, "." ], i `delete` pcIds d) ]


mkGetDropCoinsDescSelf :: Id -> GetOrDrop -> Coins -> ([Broadcast], [T.Text])
mkGetDropCoinsDescSelf i god c | bs <- mkCoinsBroadcasts c helper = (bs, extractLogMsgs i bs)
  where
    helper 1 cn = [ (T.concat [ "You ", mkGodVerb god SndPer, " ", aOrAn cn,             "." ], pure i) ]
    helper a cn = [ (T.concat [ "You ", mkGodVerb god SndPer, " ", showText a, " ", cn, "s." ], pure i) ]


mkCoinsBroadcasts :: Coins -> (Int -> T.Text -> [Broadcast]) -> [Broadcast]
mkCoinsBroadcasts (Coins (cop, sil, gol)) f = concat . catMaybes $ [ c, s, g ]
  where
    c = Sum cop |!| Just . f cop $ "copper piece"
    s = Sum sil |!| Just . f sil $ "silver piece"
    g = Sum gol |!| Just . f gol $ "gold piece"


mkCan'tGetCoinsDesc :: Id -> Coins -> [Broadcast]
mkCan'tGetCoinsDesc i = (`mkCoinsBroadcasts` helper)
  where
    helper a cn = let rest | a == 1    = " the " <> cn <> "."
                           | otherwise = T.concat [ " ", showText a, " ", cn, "s." ]
                  in mkBroadcast i $ sorryEnc <> rest


sorryEnc :: T.Text
sorryEnc = "You are too encumbered to pick up"


-----


helperGetEitherInv :: Id
                   -> PCDesig
                   -> FromId
                   -> ToId
                   -> (MudState, [Broadcast], [T.Text])
                   -> Either T.Text Inv
                   -> (MudState, [Broadcast], [T.Text])
helperGetEitherInv i d fi ti a@(ms, _, _) = \case
  Left  (mkBroadcast i -> b                  ) -> a & _2 <>~ b
  Right (sortByType    -> (pcs, mobs, others)) ->
    let (_, cans, can'ts) = foldl' (partitionByEnc (calcMaxEnc i ms)) (calcWeight i ms, [], []) others
        (bs, logMsgs)     = mkGetDropInvDesc i ms d Get cans
    in a & _1.invTbl.ind fi %~  (\\ cans)
         & _1.invTbl.ind ti %~  (sortInv ms . (++ cans))
         & _2               <>~ concat [ map sorryPC pcs, map sorryMob mobs, bs, mkCan'tGetInvDesc i ms can'ts ]
         & _3               <>~ logMsgs
  where
    sortByType             = foldr helper ([], [], [])
    helper targetId sorted = let lens = case getType targetId ms of PCType  -> _1
                                                                    MobType -> _2
                                                                    _       -> _3
                             in sorted & lens %~ (targetId :)
    partitionByEnc maxEnc acc@(w, _, _) targetId = let w' = w + calcWeight targetId ms in
        w' <= maxEnc ? (acc & _1 .~ w' & _2 <>~ pure targetId) :? (acc & _3 <>~ pure targetId)
    sorryPC     targetId   = sorryHelper . serialize . mkStdDesig targetId ms $ Don'tCap
    sorryMob    targetId   = sorryHelper . theOnLower . getSing targetId $ ms
    sorryHelper targetName = ("You can't pick up " <> targetName <> ".", pure i)


mkCan'tGetInvDesc :: Id -> MudState -> Inv -> [Broadcast]
mkCan'tGetInvDesc i ms = concatMap helper . mkNameCountBothList i ms
  where
    helper (_, c, b@(s, _)) = let rest | c == 1    = " the " <> s <> "."
                                       | otherwise = T.concat [ " ", showText c, " ", mkPlurFromBoth b, "." ]
                              in mkBroadcast i $ sorryEnc <> rest


-----


helperLinkUnlink :: MudState -> Id -> MsgQueue -> Cols -> MudStack (Maybe ([T.Text], [T.Text], [T.Text]))
helperLinkUnlink ms i mq cols =
    let s                = getSing   i ms
        othersLinkedToMe = getLinked i ms
        meLinkedToOthers = foldr buildSingList [] $ i `delete` (ms^.pcTbl.to IM.keys)
        buildSingList pi acc | s `elem` getLinked pi ms = getSing pi ms : acc
                             | otherwise                = acc
        twoWays = map fst . filter ((== 2) . snd) . countOccs $ othersLinkedToMe ++ meLinkedToOthers
    in if all (()#) [ othersLinkedToMe, meLinkedToOthers ]
      then let msg = "You haven't established a telepathic link with anyone."
           in emptied (wrapSend mq cols msg >> logPlaOut "helperLinkUnlink" i [msg])
      else unadulterated (meLinkedToOthers, othersLinkedToMe, twoWays)


-----


type NthOfM = (Int, Int)
type ToSing = Sing


helperPutRemEitherCoins :: Id
                        -> PCDesig
                        -> PutOrRem
                        -> Maybe NthOfM
                        -> FromId
                        -> ToId
                        -> ToSing
                        -> (CoinsTbl, [Broadcast], [T.Text])
                        -> [Either [T.Text] Coins]
                        -> (CoinsTbl, [Broadcast], [T.Text])
helperPutRemEitherCoins i d por mnom fi ti ts (origCoinsTbl, origBs, origMsgs) ecs =
    let (finalCoinsTbl, finalBs, finalMsgs, canCoins) = foldl' helper (origCoinsTbl, origBs, origMsgs, mempty) ecs
    in (finalCoinsTbl, finalBs ++ mkPutRemCoinsDescOthers i d por mnom canCoins ts, finalMsgs)
  where
    helper a = \case
      Left  msgs -> a & _2 <>~ (mkBroadcast i . T.concat $ msgs)
      Right c    -> let (bs, logMsgs) = mkPutRemCoinsDescsSelf i por mnom c ts
                    in a & _1.ind fi %~ (<> negateCoins c)
                         & _1.ind ti %~ (<> c)
                         & _2 <>~ bs
                         & _3 <>~ logMsgs
                         & _4 <>~ c


mkPutRemCoinsDescOthers :: Id -> PCDesig -> PutOrRem -> Maybe NthOfM -> Coins -> ToSing -> [Broadcast]
mkPutRemCoinsDescOthers i d por mnom c ts = c |!| [ ( T.concat [ serialize d
                                                               , " "
                                                               , mkPorVerb por ThrPer
                                                               , " "
                                                               , aCoinSomeCoins c
                                                               , " "
                                                               , mkPorPrep por ThrPer mnom ts
                                                               , onTheGround mnom <> "." ]
                                                    , i `delete` pcIds d ) ]


mkPutRemCoinsDescsSelf :: Id -> PutOrRem -> Maybe NthOfM -> Coins -> ToSing -> ([Broadcast], [T.Text])
mkPutRemCoinsDescsSelf i por mnom c ts | bs <- mkCoinsBroadcasts c helper = (bs, extractLogMsgs i bs)
  where
    helper a cn | a == 1 = [ (T.concat [ start, aOrAn cn,   " ",           rest ], pure i) ]
    helper a cn          = [ (T.concat [ start, showText a, " ", cn, "s ", rest ], pure i) ]
    start                = "You " <> mkPorVerb por SndPer <> " "
    rest                 = mkPorPrep por SndPer mnom ts <> onTheGround mnom <> "."


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
onTheGround = (|!| " on the ground") . ((both %~ Sum) <$>)


-----


helperPutRemEitherInv :: Id
                      -> MudState
                      -> PCDesig
                      -> PutOrRem
                      -> Maybe NthOfM
                      -> FromId
                      -> ToId
                      -> ToSing
                      -> (InvTbl, [Broadcast], [T.Text])
                      -> Either T.Text Inv
                      -> (InvTbl, [Broadcast], [T.Text])
helperPutRemEitherInv i ms d por mnom fi ti ts a@(_, bs, _) = \case
  Left  (mkBroadcast i -> b) -> a & _2 <>~ b
  Right is -> let (is', bs')      = ti `elem` is ? (filter (/= ti) is, bs ++ sorryInsideSelf) :? (is, bs)
                  (bs'', logMsgs) = mkPutRemInvDesc i ms d por mnom is' ts
              in ()# (a^._1.ind fi) ? sorryEmpty :? (a & _1.ind fi %~  (\\ is')
                                                       & _1.ind ti %~  (sortInv ms . (++ is'))
                                                       & _2        .~  (bs' ++ bs'')
                                                       & _3        <>~ logMsgs)
  where
    sorryInsideSelf = mkBroadcast i $ "You can't put the " <> ts <> " inside itself."
    sorryEmpty      = a & _2 <>~ mkBroadcast i ("The " <> getSing fi ms <> " is empty.")


mkPutRemInvDesc :: Id -> MudState -> PCDesig -> PutOrRem -> Maybe NthOfM -> Inv -> ToSing -> ([Broadcast], [T.Text])
mkPutRemInvDesc i ms d por mnom is ts =
    let bs = concatMap helper . mkNameCountBothList i ms $ is in (bs, extractLogMsgs i bs)
  where
    helper (_, c, (s, _)) | c == 1 =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , " "
                    , withArticle
                    , " "
                    , mkPorPrep por SndPer mnom ts
                    , rest ], pure i)
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " "
                    , aOrAn s
                    , " "
                    , mkPorPrep por ThrPer mnom ts
                    , rest ], otherPCIds) ]
      where
        withArticle = por == Put ? "the " <> s :? aOrAn s
    helper (_, c, b) =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , " "
                    , showText c
                    , " "
                    , mkPlurFromBoth b
                    , " "
                    , mkPorPrep por SndPer mnom ts
                    , rest ], pure i)
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


isLinked :: MudState -> (Id, Id) -> Bool
isLinked = helperIsLinked (||)


helperIsLinked :: (Bool -> Bool -> Bool) -> MudState -> (Id, Id) -> Bool
helperIsLinked f ms (i, i') = let s                = getSing i  ms
                                  s'               = getSing i' ms
                                  targetLinkedToMe = s' `elem` getLinked i  ms
                                  meLinkedToTarget = s  `elem` getLinked i' ms
                              in targetLinkedToMe `f` meLinkedToTarget


isDblLinked :: MudState -> (Id, Id) -> Bool
isDblLinked = helperIsLinked (&&)


-----


isRingRol :: RightOrLeft -> Bool
isRingRol = \case R -> False
                  L -> False
                  _ -> True


-----


isSlotAvail :: EqMap -> Slot -> Bool
isSlotAvail em s = s `M.notMember` em


findAvailSlot :: EqMap -> [Slot] -> Maybe Slot
findAvailSlot em = find (isSlotAvail em)


-----


maybeSingleSlot :: EqMap -> Slot -> Maybe Slot
maybeSingleSlot em s = toMaybe (isSlotAvail em s) s


-----


mkPutRemoveBindings :: Id -> MudState -> Args -> (PCDesig, (Inv, Coins), (Inv, Coins), ConName, Args)
mkPutRemoveBindings i ms as = let d              = mkStdDesig  i ms DoCap
                                  pcInvCoins     = getInvCoins i ms
                                  rmInvCoins     = first (i `delete`) . getPCRmNonIncogInvCoins i $ ms
                                  conName        = last as
                                  argsWithoutCon = init $ case as of
                                                     [_, _] -> as
                                                     _      -> (++ pure conName) . nub . init $ as
                              in (d, pcInvCoins, rmInvCoins, conName, argsWithoutCon)


-----


mkCoinsDesc :: Cols -> Coins -> T.Text
mkCoinsDesc cols (Coins (each %~ Sum -> (cop, sil, gol))) =
    T.unlines . intercalate [""] . map (wrap cols) . dropEmpties $ [ cop |!| copDesc
                                                                   , sil |!| silDesc
                                                                   , gol |!| golDesc ]
  where
    copDesc = "The copper piece is round and shiny."
    silDesc = "The silver piece is round and shiny."
    golDesc = "The gold piece is round and shiny."


-----


mkEntDescs :: Id -> Cols -> MudState -> Inv -> T.Text
mkEntDescs i cols ms eis = T.intercalate "\n" [ mkEntDesc i cols ms (ei, e) | ei <- eis, let e = getEnt ei ms ]


mkEntDesc :: Id -> Cols -> MudState -> (Id, Ent) -> T.Text
mkEntDesc i cols ms (ei, e) | ed <- views entDesc (wrapUnlines cols) e, s <- getSing ei ms, t <- getType ei ms =
    case t of ConType ->                 (ed <>) . mkInvCoinsDesc i cols ms ei $ s
              MobType ->                 (ed <>) . mkEqDesc       i cols ms ei   s $ t
              PCType  -> (pcHeader <>) . (ed <>) . mkEqDesc       i cols ms ei   s $ t
              _       -> ed
  where
    pcHeader = wrapUnlines cols mkPCDescHeader
    mkPCDescHeader | (pp *** pp -> (s, r)) <- getSexRace ei ms = T.concat [ "You see a ", s, " ", r, "." ]


mkInvCoinsDesc :: Id -> Cols -> MudState -> Id -> Sing -> T.Text
mkInvCoinsDesc i cols ms targetId targetSing | targetInv <- getInv targetId ms, targetCoins <- getCoins targetId ms =
    case ((()#) *** (()#)) (targetInv, targetCoins) of
      (True,  True ) -> wrapUnlines cols (targetId == i ? dudeYourHandsAreEmpty :? "The " <> targetSing <> " is empty.")
      (False, True ) -> header <> mkEntsInInvDesc i cols ms targetInv                                    <> footer
      (True,  False) -> header                                        <> mkCoinsSummary cols targetCoins <> footer
      (False, False) -> header <> mkEntsInInvDesc i cols ms targetInv <> mkCoinsSummary cols targetCoins <> footer
  where
    header = targetId == i ? nl "You are carrying:" :? wrapUnlines cols ("The " <> targetSing <> " contains:")
    footer = targetId == i |?| nl $ (showText . calcEncPer i $ ms) <> "% encumbered."


dudeYourHandsAreEmpty :: T.Text
dudeYourHandsAreEmpty = "You aren't carrying anything."


mkEntsInInvDesc :: Id -> Cols -> MudState -> Inv -> T.Text
mkEntsInInvDesc i cols ms =
    T.unlines . concatMap (wrapIndent entNamePadding cols . helper) . mkStyledName_Count_BothList i ms
  where
    helper (padEntName -> en, c, (s, _)) | c == 1 = en <> "1 " <> s
    helper (padEntName -> en, c, b     )          = T.concat [ en, showText c, " ", mkPlurFromBoth b ]


mkStyledName_Count_BothList :: Id -> MudState -> Inv -> [(T.Text, Int, BothGramNos)]
mkStyledName_Count_BothList i ms is =
    let styleds                       = styleAbbrevs DoBracket [ getEffName        i ms targetId | targetId <- is ]
        boths@(mkCountList -> counts) =                        [ getEffBothGramNos i ms targetId | targetId <- is ]
    in nub . zip3 styleds counts $ boths


mkCoinsSummary :: Cols -> Coins -> T.Text
mkCoinsSummary cols c = helper . zipWith mkNameAmt coinNames . coinsToList $ c
  where
    helper         = T.unlines . wrapIndent 2 cols . commas . filter (()!#)
    mkNameAmt cn a = Sum a |!| showText a <> " " <> bracketQuote (abbrevColor <> cn <> dfltColor)


mkEqDesc :: Id -> Cols -> MudState -> Id -> Sing -> Type -> T.Text
mkEqDesc i cols ms descId descSing descType = let descs = descId == i ? mkDescsSelf :? mkDescsOther in
    ()# descs ? none :? ((header <>) . T.unlines . concatMap (wrapIndent 15 cols) $ descs)
  where
    mkDescsSelf =
        let (slotNames,  es ) = unzip [ (pp slot, getEnt ei ms)          | (slot, ei) <- M.toList . getEqMap i $ ms ]
            (sings,      ens) = unzip [ (e^.sing, fromJust $ e^.entName) | e          <- es                         ]
        in map helper . zip3 slotNames sings . styleAbbrevs DoBracket $ ens
      where
        helper (T.breakOn " finger" -> (slotName, _), s, styled) = T.concat [ parensPad 15 slotName, s, " ", styled ]
    mkDescsOther = map helper [ (pp slot, getSing ei ms) | (slot, ei) <- M.toList . getEqMap descId $ ms ]
      where
        helper (T.breakOn " finger" -> (slotName, _), s) = parensPad 15 slotName <> s
    none = wrapUnlines cols $ if
      | descId   == i      -> dudeYou'reNaked
      | descType == PCType -> parsePCDesig i ms $ d  <> " doesn't have anything readied."
      | otherwise          -> theOnLowerCap descSing <> " doesn't have anything readied."
    header = wrapUnlines cols $ if
      | descId   == i      -> "You have readied the following equipment:"
      | descType == PCType -> parsePCDesig i ms $ d  <> " has readied the following equipment:"
      | otherwise          -> theOnLowerCap descSing <> " has readied the following equipment:"
    d = mkSerializedNonStdDesig descId ms descSing The DoCap


dudeYou'reNaked :: T.Text
dudeYou'reNaked = "You don't have anything readied. You're naked!"


-----


mkExitsSummary :: Cols -> Rm -> T.Text
mkExitsSummary cols (view rmLinks -> rls) =
    let stdNames    = [ exitsColor <> rl^.linkDir.to linkDirToCmdName <> dfltColor | rl <- rls
                                                                                   , not . isNonStdLink $ rl ]
        customNames = [ exitsColor <> rl^.linkName                    <> dfltColor | rl <- rls
                                                                                   ,       isNonStdLink   rl ]
    in T.unlines . wrapIndent 2 cols . ("Obvious exits: " <>) . summarize stdNames $ customNames
  where
    summarize []  []  = "None!"
    summarize std cus = commas . (std ++) $ cus


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


type IsConInRm  = Bool
type InvWithCon = Inv


mkMaybeNthOfM :: MudState -> IsConInRm -> Id -> Sing -> InvWithCon -> Maybe NthOfM
mkMaybeNthOfM ms icir conId conSing invWithCon = guard icir >> return helper
  where
    helper  = (succ . fromJust . elemIndex conId *** length) . dup $ matches
    matches = filter ((== conSing) . flip getSing ms) invWithCon


-----


moveReadiedItem :: Id
                -> (EqTbl, InvTbl, [Broadcast], [T.Text])
                -> Slot
                -> Id
                -> (T.Text, Broadcast)
                -> (EqTbl, InvTbl, [Broadcast], [T.Text])
moveReadiedItem i a s targetId (msg, b) = a & _1.ind i.at s ?~ targetId
                                            & _2.ind i %~ (targetId `delete`)
                                            & _3 <>~ (mkBroadcast i msg ++ pure b)
                                            & _4 <>~ pure msg


-----


noCoinsInEq :: T.Text
noCoinsInEq = "You don't have any coins among your readied equipment."


-----


noContainersHere :: T.Text
noContainersHere = "You don't see any containers here."


-----


noOneHere :: T.Text
noOneHere = "You don't see anyone here."


-----


otherHand :: Hand -> Hand
otherHand RHand  = LHand
otherHand LHand  = RHand
otherHand NoHand = NoHand


-----


putOnMsgs :: Id -> PCDesig -> Sing -> (T.Text, Broadcast)
putOnMsgs = mkReadyMsgs "put on" "puts on"


-----


resolvePCInvCoins :: Id -> MudState -> Args -> Inv -> Coins -> ([Either T.Text Inv], [Either [T.Text] Coins])
resolvePCInvCoins i ms = resolveHelper i ms procGecrMisPCInv procReconciledCoinsPCInv


resolveHelper :: Id
              -> MudState
              -> ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv)
              -> (ReconciledCoins -> Either [T.Text] Coins)
              -> Args
              -> Inv
              -> Coins
              -> ([Either T.Text Inv], [Either [T.Text] Coins])
resolveHelper i ms f g as is c | (gecrs, miss, rcs) <- resolveEntCoinNames i ms as is c
                               , eiss               <- zipWith (curry f) gecrs miss
                               , ecs                <- map g rcs = (eiss, ecs)


-----


resolveRmInvCoins :: Id -> MudState -> Args -> Inv -> Coins -> ([Either T.Text Inv], [Either [T.Text] Coins])
resolveRmInvCoins i ms = resolveHelper i ms procGecrMisRm procReconciledCoinsRm


-----


sorryConInEq :: PutOrRem -> T.Text
sorryConInEq por = let (a, b) = expand por
                   in T.concat [ "Sorry, but you can't "
                               , a
                               , " an item "
                               , b
                               , " a container in your readied equipment. Please unready the container first." ]
  where
    expand = \case Put -> ("put",    "into")
                   Rem -> ("remove", "from")


-----


sorryEquipInvLook :: Cols -> EquipInvLookCmd -> EquipInvLookCmd -> T.Text
sorryEquipInvLook cols eilcA eilcB = wrapUnlinesNl cols . T.concat $ helper
  where
    helper = [ "You can only use the "
             , dblQuote . showText $ eilcA
             , " command to examine items in your "
             , loc eilcA
             , ". To examine items in your "
             , loc eilcB
             , ", use the "
             , dblQuote . showText $ eilcB
             , " command." ]
    loc    = \case EquipCmd -> "readied equipment"
                   InvCmd   -> "inventory"
                   LookCmd  -> "current room"


-----


sorryIncog :: T.Text -> T.Text
sorryIncog cn = "You can't use the " <> dblQuote cn <> " command while incognito."
