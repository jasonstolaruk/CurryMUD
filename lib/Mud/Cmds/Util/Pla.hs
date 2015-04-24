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
                         , helperDropEitherInv
                         , helperGetDropEitherCoins
                         , helperGetEitherInv
                         , helperPutRemEitherCoins
                         , helperPutRemEitherInv
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
                         , mkGetDropCoinsDesc
                         , mkGetDropInvDesc
                         , mkInvCoinsDesc
                         , mkMaybeNthOfM
                         , mkPossPro
                         , mkPutRemCoinsDescs
                         , mkPutRemInvDesc
                         , mkPutRemoveBindings
                         , mkReadyMsgs
                         , mkReflexPro
                         , mkStdDesig
                         , mkThrPerPro
                         , moveReadiedItem
                         , otherHand
                         , putOnMsgs
                         , resolvePCInvCoins
                         , resolveRmInvCoins ) where

import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Coins
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.Misc.NameResolution
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.List (mkCountList)
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logPla)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow ((***), first)
import Control.Exception.Lifted (try)
import Control.Lens (_1, _2, _3, _4, at, both, each, to, view, views)
import Control.Lens.Operators ((%~), (&), (.~), (<>~), (?~), (^.))
import Control.Monad ((>=>), guard)
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\), delete, elemIndex, find, intercalate, nub)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>), Sum(..))
import qualified Data.Map.Lazy as M (notMember, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (appendFile)


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
bugTypoLogger (Msg i mq msg) wl@(pp -> wl') = getState >>= \ms ->
    let s  = getSing i  ms
        ri = getRmId i  ms
    in do
        logIt s ri (getRm ri ms ^.rmName) |$| liftIO . try >=> eitherRet (fileIOExHandler "bugTypoLogger")
        send mq . nlnl $ "Thank you."
        bcastAdmins $ s <> " has logged a " <> wl' <> "."
        logPla "bugTypoLogger" i . T.concat $ [ "logged a ", wl', ": ", msg ]
  where
    logIt s (showText -> ri) (dblQuote -> rn) = mkTimestamp >>= \ts ->
        T.appendFile logFile . nl . T.concat $ [ ts
                                               , " "
                                               , s
                                               , " "
                                               , parensQuote $ ri <> " " <> rn
                                               , ": "
                                               , msg ]
    logFile = case wl of BugLog  -> bugLogFile
                         TypoLog -> typoLogFile
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
isSlotAvail em s = s `M.notMember` em


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
                         -> (CoinsTbl, [Broadcast], [T.Text])
                         -> Either [T.Text] Coins
                         -> (CoinsTbl, [Broadcast], [T.Text])
helperGetDropEitherCoins i d god fi ti a = \case
  Left  msgs -> a & _2 <>~ (mkBroadcast i . T.concat $ msgs)
  Right c    -> let (bs, logMsgs) = mkGetDropCoinsDesc i d god c
                in a & _1.ind fi %~ (<> negateCoins c) & _1.ind ti %~ (<> c) & _2 <>~ bs & _3 <>~ logMsgs


mkGetDropCoinsDesc :: Id -> PCDesig -> GetOrDrop -> Coins -> ([Broadcast], [T.Text])
mkGetDropCoinsDesc i d god c | bs <- mkCoinsBroadcasts c helper = (bs, extractLogMsgs i bs)
  where
    helper 1 cn =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, " ", aOrAn cn, "." ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " ", aOrAn cn, "." ], otherPCIds) ]
    helper a cn =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, " ", showText a, " ", cn, "s." ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " ", showText a, " ", cn, "s." ], otherPCIds) ]
    otherPCIds = i `delete` pcIds d


mkCoinsBroadcasts :: Coins -> (Int -> T.Text -> [Broadcast]) -> [Broadcast]
mkCoinsBroadcasts (Coins (cop, sil, gol)) f = concat . catMaybes $ [ c, s, g ]
  where
    c = Sum cop |!| Just . f cop $ "copper piece"
    s = Sum sil |!| Just . f sil $ "silver piece"
    g = Sum gol |!| Just . f gol $ "gold piece"


extractLogMsgs :: Id -> [Broadcast] -> [T.Text]
extractLogMsgs i bs = [ msg | (msg, targetIds) <- bs, targetIds == [i] ]


mkGodVerb :: GetOrDrop -> Verb -> T.Text
mkGodVerb Get  SndPer = "pick up"
mkGodVerb Get  ThrPer = "picks up"
mkGodVerb Drop SndPer = "drop"
mkGodVerb Drop ThrPer = "drops"


-----


-- TODO: Sort functions alphabetically.


helperDropEitherInv :: Id
                    -> MudState
                    -> PCDesig
                    -> FromId
                    -> ToId
                    -> (InvTbl, [Broadcast], [T.Text])
                    -> Either T.Text Inv
                    -> (InvTbl, [Broadcast], [T.Text])
helperDropEitherInv i ms d fi ti a = \case
  Left  (mkBroadcast i -> b) -> a & _2 <>~ b
  Right is                   -> let (bs, logMsgs) = mkGetDropInvDesc i ms d Drop is
                                in a & _1.ind fi %~  (\\ is)
                                     & _1.ind ti %~  (sortInv ms . (++ is))
                                     & _2        <>~ bs
                                     & _3        <>~ logMsgs


helperGetEitherInv :: Id
                   -> MudState
                   -> PCDesig
                   -> FromId
                   -> ToId
                   -> (InvTbl, [Broadcast], [T.Text])
                   -> Either T.Text Inv
                   -> (InvTbl, [Broadcast], [T.Text])
helperGetEitherInv i ms d fi ti a = \case
  Left  (mkBroadcast i -> b                  ) -> a & _2 <>~ b
  Right (sortByType    -> (pcs, mobs, others)) -> let (bs, logMsgs) = mkGetDropInvDesc i ms d Get others
                                                  in a & _1.ind fi %~  (\\ others)
                                                       & _1.ind ti %~  (sortInv ms . (++ others))
                                                       & _2        <>~ map sorryPC pcs ++ map sorryMob mobs ++ bs
                                                       & _3        <>~ logMsgs
  where
    sortByType             = foldr helper ([], [], [])
    helper targetId sorted = let lens = case getType targetId ms of PCType  -> _1
                                                                    MobType -> _2
                                                                    _       -> _3
                             in sorted & lens %~ (targetId :)
    sorryPC     targetId   = sorryHelper . serialize . mkStdDesig targetId ms $ Don'tCap
    sorryMob    targetId   = sorryHelper . theOnLower . getSing targetId $ ms
    sorryHelper targetName = ("You can't pick up " <> targetName <> ".", [i])


mkGetDropInvDesc :: Id -> MudState -> PCDesig -> GetOrDrop -> Inv -> ([Broadcast], [T.Text])
mkGetDropInvDesc i ms d god (mkNameCountBothList i ms -> ncbs) =
    let bs = concatMap helper ncbs in (bs, extractLogMsgs i bs)
  where
    helper (_, c, (s, _)) | c == 1 =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, " the ", s,   "." ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " ", aOrAn s, "." ], otherPCIds) ]
    helper (_, c, b) =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, rest ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, rest ], otherPCIds) ]
      where
        rest = T.concat [ " ", showText c, " ", mkPlurFromBoth b, "." ]
    otherPCIds = i `delete` pcIds d


mkNameCountBothList :: Id -> MudState -> Inv -> [(T.Text, Int, BothGramNos)]
mkNameCountBothList i ms targetIds = let ens   = [ getEffName        i ms targetId | targetId <- targetIds ]
                                         cs    = mkCountList ebgns
                                         ebgns = [ getEffBothGramNos i ms targetId | targetId <- targetIds ]
                                     in nub . zip3 ens cs $ ebgns


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
                        -> Either [T.Text] Coins
                        -> (CoinsTbl, [Broadcast], [T.Text])
helperPutRemEitherCoins i d por mnom fi ti ts a = \case
  Left  msgs -> a & _2 <>~ (mkBroadcast i . T.concat $ msgs)
  Right c    -> let (bs, logMsgs) = mkPutRemCoinsDescs i d por mnom c ts
                in a & _1.ind fi %~ (<> negateCoins c) & _1.ind ti %~ (<> c) & _2 <>~ bs & _3 <>~ logMsgs


mkPutRemCoinsDescs :: Id -> PCDesig -> PutOrRem -> Maybe NthOfM -> Coins -> ToSing -> ([Broadcast], [T.Text])
mkPutRemCoinsDescs i d por mnom c ts | bs <- mkCoinsBroadcasts c helper = (bs, extractLogMsgs i bs)
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
onTheGround = (|!| " on the ground") . fmap (both %~ Sum)


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
              in null (a^._1.ind fi) ? sorryEmpty :? (a & _1.ind fi %~  (\\ is')
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


mkPutRemoveBindings :: Id -> MudState -> Args -> (PCDesig, (Inv, Coins), (Inv, Coins), ConName, Args)
mkPutRemoveBindings i ms as = let d                        = mkStdDesig  i ms DoCap
                                  pcInvCoins               = getInvCoins i ms
                                  rmInvCoins               = first (i `delete`) . getPCRmInvCoins i $ ms
                                  conName                  = last as
                                  (init -> argsWithoutCon) = case as of
                                                               [_, _] -> as
                                                               _      -> (++ [conName]) . nub . init $ as
                              in (d, pcInvCoins, rmInvCoins, conName, argsWithoutCon)


-----


mkStdDesig :: Id -> MudState -> ShouldCap -> PCDesig
mkStdDesig i ms sc = StdDesig { stdPCEntSing = Just . getSing i $ ms
                              , shouldCap    = sc
                              , pcEntName    = mkUnknownPCEntName i ms
                              , pcId         = i
                              , pcIds        = findPCIds ms . getPCRmInv i $ ms }


-----


mkCoinsDesc :: Cols -> Coins -> T.Text
mkCoinsDesc cols (Coins (each %~ Sum -> (cop, sil, gol))) =
    T.unlines . intercalate [""] . map (wrap cols) . filter (not . T.null) $ [ cop |!| copDesc
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
mkInvCoinsDesc i cols ms descId descSing | descInv <- getInv descId ms, descCoins <- getCoins descId ms =
    case (notEmpty *** notEmpty) (descInv, descCoins) of
      (False, False) -> wrapUnlines cols (descId == i ? dudeYourHandsAreEmpty :? "The " <> descSing <> " is empty.")
      (True,  False) -> header <> mkEntsInInvDesc i cols ms descInv
      (False, True ) -> header                                      <> mkCoinsSummary cols descCoins
      (True,  True ) -> header <> mkEntsInInvDesc i cols ms descInv <> mkCoinsSummary cols descCoins
  where
    header = descId == i ? nl "You are carrying:" :? wrapUnlines cols ("The " <> descSing <> " contains:")


dudeYourHandsAreEmpty :: T.Text
dudeYourHandsAreEmpty = "You aren't carrying anything."


mkEntsInInvDesc :: Id -> Cols -> MudState -> Inv -> T.Text
mkEntsInInvDesc i cols ms =
    T.unlines . concatMap (wrapIndent indent cols . helper) . mkStyledName_Count_BothList i ms
  where
    helper (pad indent -> en, c, (s, _)) | c == 1 = en <> "1 " <> s
    helper (pad indent -> en, c, b     )          = T.concat [ en, showText c, " ", mkPlurFromBoth b ]
    indent = 11


mkStyledName_Count_BothList :: Id -> MudState -> Inv -> [(T.Text, Int, BothGramNos)]
mkStyledName_Count_BothList i ms is =
    let styleds                       = styleAbbrevs DoBracket [ getEffName        i ms targetId | targetId <- is ]
        boths@(mkCountList -> counts) =                        [ getEffBothGramNos i ms targetId | targetId <- is ]
    in nub . zip3 styleds counts $ boths


mkCoinsSummary :: Cols -> Coins -> T.Text
mkCoinsSummary cols c = helper . zipWith mkNameAmt coinNames . coinsToList $ c
  where
    helper         = T.unlines . wrapIndent 2 cols . T.intercalate ", " . filter (not . T.null)
    mkNameAmt cn a = Sum a |!| showText a <> " " <> bracketQuote (abbrevColor <> cn <> dfltColor)


mkEqDesc :: Id -> Cols -> MudState -> Id -> Sing -> Type -> T.Text
mkEqDesc i cols ms descId descSing descType = let descs = descId == i ? mkDescsSelf :? mkDescsOther in
    null descs ? none :? ((header <>) . T.unlines . concatMap (wrapIndent 15 cols) $ descs)
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
    d = mkSerializedNonStdDesig descId ms descSing The


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


type IsConInRm  = Bool
type InvWithCon = Inv


mkMaybeNthOfM :: MudState -> IsConInRm -> Id -> Sing -> InvWithCon -> Maybe NthOfM
mkMaybeNthOfM ms icir conId conSing invWithCon = guard icir >> return helper
  where
    helper  = (succ . fromJust . elemIndex conId *** length) . dup $ matches
    matches = filter ((== conSing) . flip getSing ms) invWithCon


-----


mkPossPro :: Sex -> T.Text
mkPossPro Male   = "his"
mkPossPro Female = "her"
mkPossPro s      = patternMatchFail "mkPossPro" [ showText s ]


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
                -> (EqTbl, InvTbl, [Broadcast], [T.Text])
                -> Slot
                -> Id
                -> (T.Text, Broadcast)
                -> (EqTbl, InvTbl, [Broadcast], [T.Text])
moveReadiedItem i a s targetId (msg, b) =
    a & _1.ind i.at s ?~ targetId & _2.ind i %~ (targetId `delete`) & _3 <>~ (mkBroadcast i msg ++ [b]) & _4 <>~ [msg]


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
