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
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
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

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Exception.Lifted (try)
import Control.Lens (_1, _2, _3, _4, at, both, over, to)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (.~), (<>~), (?~), (^.))
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
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
bugTypoLogger (Msg i mq msg) wl@(pp -> wl') =
    ask >>= (liftIO . atomically . helperSTM) >>= \( view sing . (! i) -> s
                                                   , mt
                                                   , mqt
                                                   , pcTbl@(view rmId . (! i) -> ri)
                                                   , plaTbl
                                                   , rt ) -> do
    logPla "bugTypoLogger" i . T.concat $ [ "logged a ", wl', ": ", msg ]
    liftIO (try . logIt s ri $ (rt ! ri)^.rmName) >>= eitherRet (fileIOExHandler "bugTypoLogger")
    send mq . nlnl $ "Thank you."
    bcastAdmins mt mqt pcTbl plaTbl $ s <> " has logged a " <> wl' <> "."
  where
    helperSTM md = (,,,,,) <$> readTVar (md^.entTblTVar)
                           <*> readTVar (md^.mobTblTVar)
                           <*> readTVar (md^.msgQueueTblTVar)
                           <*> readTVar (md^.pcTblTVar)
                           <*> readTVar (md^.plaTblTVar)
                           <*> readTVar (md^.rmTblTVar)
    logIt s ri rn = mkTimestamp >>= \ts -> -- TODO: Why not just append to the end of the file?
        let newEntry = T.concat [ ts
                                , " "
                                , s
                                , " "
                                , parensQuote $ showText ri <> " " <> dblQuote rn
                                , ": "
                                , msg ]
        in T.writeFile logFile =<< [ T.unlines . sort $ newEntry : cont | cont <- getLogConts ]
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
                         -> (CoinsTbl, [Broadcast], [T.Text])
                         -> Either [T.Text] Coins
                         -> (CoinsTbl, [Broadcast], [T.Text])
helperGetDropEitherCoins i d god fi ti a@(ct, _, _) = \case
  Left  msgs -> a & _2 <>~ [ (msg, [i]) | msg <- msgs ]
  Right c | (fc, tc)      <- over both (ct !) (fi, ti)
          , ct'           <- ct & at fi ?~ fc <> negateCoins c
                                & at ti ?~ tc <> c
          , (bs, logMsgs) <- mkGetDropCoinsDesc i d god c
          -> a & _1 .~ ct' & _2 <>~ bs & _3 <>~ logMsgs


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
                       -> EntTbl
                       -> MobTbl
                       -> PCTbl
                       -> TypeTbl
                       -> PCDesig
                       -> GetOrDrop
                       -> FromId
                       -> ToId
                       -> (InvTbl, [Broadcast], [T.Text])
                       -> Either T.Text Inv
                       -> (InvTbl, [Broadcast], [T.Text])
helperGetDropEitherInv i et mt pt tt d god fi ti a@(it, _, _) = \case
  Left  (mkBroadcast i -> b) -> a & _2 <>~ b
  Right is                   -> let (fis, tis)    = over both (it !) (fi, ti)
                                    it'           = it & at fi ?~ fis \\ is
                                                       & at ti ?~ sortInv et tt (tis ++ is)
                                    (bs, logMsgs) = mkGetDropInvDesc i et mt pt d god is
                                in a & _1 .~ it' & _2 <>~ bs & _3 <>~ logMsgs


mkGetDropInvDesc :: Id -> EntTbl -> MobTbl -> PCTbl -> PCDesig -> GetOrDrop -> Inv -> ([Broadcast], [T.Text])
mkGetDropInvDesc i et mt pt d god (mkNameCountBothList i et mt pt -> ncbs) =
    let bs = concatMap helper ncbs in (bs, extractLogMsgs i bs)
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


mkNameCountBothList :: Id -> EntTbl -> MobTbl -> PCTbl -> Inv -> [(T.Text, Int, BothGramNos)]
mkNameCountBothList i et mt pt is | ens   <- [ getEffName        i et mt pt i' | i' <- is ]
                                  , ebgns <- [ getEffBothGramNos i et mt pt i' | i' <- is ]
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
                        -> (CoinsTbl, [Broadcast], [T.Text])
                        -> Either [T.Text] Coins
                        -> (CoinsTbl, [Broadcast], [T.Text])
helperPutRemEitherCoins i d por mnom fi ti te a@(ct, _, _) = \case
  Left  msgs -> a & _2 <>~ [ (msg, [i]) | msg <- msgs ]
  Right c | (fc, tc)      <- over both (ct !) (fi, ti)
          , ct'           <- ct & at fi ?~ fc <> negateCoins c
                                & at ti ?~ tc <> c
          , (bs, logMsgs) <- mkPutRemCoinsDescs i d por mnom c te
          -> a & _1 .~ ct' & _2 <>~ bs & _3 <>~ logMsgs


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
                      -> EntTbl
                      -> MobTbl
                      -> PCTbl
                      -> TypeTbl
                      -> PCDesig
                      -> PutOrRem
                      -> Maybe NthOfM
                      -> FromId
                      -> ToId
                      -> ToEnt
                      -> (InvTbl, [Broadcast], [T.Text])
                      -> Either T.Text Inv
                      -> (InvTbl, [Broadcast], [T.Text])
helperPutRemEitherInv i et mt pt tt d por mnom fi ti te a@(it, bs, _) = \case
  Left  (mkBroadcast i -> b) -> a & _2 <>~ b
  Right is -> let (is', bs')      = if ti `elem` is then (filter (/= ti) is, bs ++ [sorryInsideSelf]) else (is, bs)
                  (fis, tis)      = over both (it !) (fi, ti)
                  it'             = it & at fi ?~ fis \\ is'
                                       & at ti ?~ sortInv et tt (tis ++ is')
                  (bs'', logMsgs) = mkPutRemInvDesc i et mt pt d por mnom is' te
              in if null fis then sorryEmpty else (a & _1 .~ it' & _2 .~ (bs' ++ bs'') & _3 <>~ logMsgs)
  where
    sorryInsideSelf = ("You can't put the " <> te^.sing <> " inside itself.", [i])
    sorryEmpty      = a & _2 <>~ [("The " <> (et ! fi)^.sing <> " is empty.", [i])]


mkPutRemInvDesc :: Id
                -> EntTbl
                -> MobTbl
                -> PCTbl
                -> PCDesig
                -> PutOrRem
                -> Maybe NthOfM
                -> Inv
                -> ToEnt
                -> ([Broadcast], [T.Text])
mkPutRemInvDesc i et mt pt d por mnom is (view sing -> ts) =
    let bs = concatMap helper . mkNameCountBothList i et mt pt $ is
    in (bs, extractLogMsgs i bs)
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


mkCapStdDesig :: Id -> EntTbl -> InvTbl -> MobTbl -> PCTbl -> TypeTbl -> (PCDesig, Sing, PC, Id, Inv)
mkCapStdDesig i et it mt pt tt | s                   <- (et ! i)^.sing
                               , p@(view rmId -> ri) <- pt ! i
                               , ris                 <- it ! ri = (mkStdDesig i mt pt tt s True ris, s, p, ri, ris)


mkStdDesig :: Id -> MobTbl -> PCTbl -> TypeTbl -> Sing -> Bool -> Inv -> PCDesig
mkStdDesig i mt pt tt s ic ris = StdDesig { stdPCEntSing = Just s
                                          , isCap        = ic
                                          , pcEntName    = mkUnknownPCEntName i mt pt
                                          , pcId         = i
                                          , pcIds        = findPCIds tt ris }


-----


mkCoinsDesc :: Cols -> Coins -> T.Text
mkCoinsDesc cols (Coins (cop, sil, gol)) =
    T.unlines . intercalate [""] . map (wrap cols) . filter (not . T.null) $ [ Sum cop |!| copDesc
                                                                             , Sum sil |!| silDesc
                                                                             , Sum gol |!| golDesc ]
  where
    copDesc = "The copper piece is round and shiny."
    silDesc = "The silver piece is round and shiny."
    golDesc = "The gold piece is round and shiny."


-----


mkDropReadyBindings :: Id -> CoinsTbl -> EntTbl -> InvTbl -> MobTbl -> PCTbl -> TypeTbl -> (PCDesig, Id, Inv, Coins)
mkDropReadyBindings i ct et it mt pt tt | (d, _, _, ri, _) <- mkCapStdDesig i et it mt pt tt
                                        , is               <- it ! i
                                        , c                <- ct ! i = (d, ri, is, c)


-----


mkEntDescs :: Id -> Cols -> CoinsTbl -> EntTbl -> EqTbl -> InvTbl -> MobTbl -> PCTbl -> TypeTbl -> Inv -> T.Text
mkEntDescs i cols ct entTbl eqTbl it mt pt tt eis =
    T.intercalate "\n" [ mkEntDesc i cols ct it entTbl eqTbl mt pt tt (ei, e) | ei <- eis, let e = entTbl ! ei ]


mkEntDesc :: Id -> Cols -> CoinsTbl -> InvTbl -> EntTbl -> EqTbl -> MobTbl -> PCTbl -> TypeTbl -> (Id, Ent) -> T.Text
mkEntDesc i cols ct it entTbl eqTbl mt pt tt (ei@((tt !) -> t), e@(views entDesc (wrapUnlines cols) -> ed)) =
    case t of ConType ->                 (ed <>) . mkInvCoinsDesc i cols ct entTbl it mt pt ei $ e
              MobType ->                 (ed <>) . mkEqDesc       i cols entTbl eqTbl mt pt ei e $ t
              PCType  -> (pcHeader <>) . (ed <>) . mkEqDesc       i cols entTbl eqTbl mt pt ei e $ t
              _       -> ed
  where
    pcHeader = wrapUnlines cols mkPCDescHeader
    mkPCDescHeader | (pp *** pp -> (s, r)) <- getSexRace ei mt pt = T.concat [ "You see a ", s, " ", r, "." ]


mkInvCoinsDesc :: Id -> Cols -> CoinsTbl -> EntTbl -> InvTbl -> MobTbl -> PCTbl -> Id -> Ent -> T.Text
mkInvCoinsDesc i cols ct et it mt pt descI (view sing -> descS)
  | descIs <- it ! descI
  , descC  <- ct ! descI = case (not . null $ descIs, descC /= mempty) of
    (False, False) -> wrapUnlines cols (descI == i ? dudeYourHandsAreEmpty :? "The " <> descS <> " is empty.")
    (True,  False) -> header <> mkEntsInInvDesc i cols et mt pt descIs
    (False, True ) -> header <>                                           mkCoinsSummary cols descC
    (True,  True ) -> header <> mkEntsInInvDesc i cols et mt pt descIs <> mkCoinsSummary cols descC
  where
    header | descI == i = nl "You are carrying:"
           | otherwise  = wrapUnlines cols $ "The " <> descS <> " contains:"


dudeYourHandsAreEmpty :: T.Text
dudeYourHandsAreEmpty = "You aren't carrying anything."


mkEntsInInvDesc :: Id -> Cols -> EntTbl -> MobTbl -> PCTbl -> Inv -> T.Text
mkEntsInInvDesc i cols et mt pt =
    T.unlines . concatMap (wrapIndent ind cols . helper) . mkStyledName_Count_BothList i et mt pt
  where
    helper (pad ind -> en, c, (s, _)) | c == 1 = en <> "1 " <> s
    helper (pad ind -> en, c, b     )          = T.concat [ en, showText c, " ", mkPlurFromBoth b ]
    ind = 11


mkStyledName_Count_BothList :: Id -> EntTbl -> MobTbl -> PCTbl -> Inv -> [(T.Text, Int, BothGramNos)]
mkStyledName_Count_BothList i et mt pt is =
    let ens   = styleAbbrevs DoBracket [ getEffName        i et mt pt i' | i' <- is ]
        ebgns =                        [ getEffBothGramNos i et mt pt i' | i' <- is ]
        cs    = mkCountList ebgns
    in nub . zip3 ens cs $ ebgns


mkCoinsSummary :: Cols -> Coins -> T.Text
mkCoinsSummary cols c = helper . zipWith mkNameAmt coinNames . mkListFromCoins $ c
  where
    mkNameAmt cn a = Sum a |!| showText a <> " " <> bracketQuote (abbrevColor <> cn <> dfltColor)
    helper         = T.unlines . wrapIndent 2 cols . T.intercalate ", " . filter (not . T.null)


mkEqDesc :: Id -> Cols -> EntTbl -> EqTbl -> MobTbl -> PCTbl -> Id -> Ent -> Type -> T.Text
mkEqDesc i cols entTbl eqTbl mt pt descI (view sing -> descS) descT =
    let descs = descI == i ? mkDescsSelf :? mkDescsOther
    in case descs of [] -> none
                     _  -> (header <>) . T.unlines . concatMap (wrapIndent 15 cols) $ descs
  where
    mkDescsSelf | (ss, is) <- unzip . M.toList $ eqTbl ! i
                , sns      <- [ pp s                  | s  <- ss ]
                , es       <- [ entTbl ! ei           | ei <- is ]
                , ess      <- [ e^.sing               | e  <- es ]
                , ens      <- [ fromJust $ e^.entName | e  <- es ]
                , styleds  <- styleAbbrevs DoBracket ens = map helper . zip3 sns ess $ styleds
      where
        helper (T.breakOn " finger" -> (sn, _), es, styled) = T.concat [ parensPad 15 sn, es, " ", styled ]
    mkDescsOther | (ss, is) <- unzip . M.toList $ eqTbl ! descI
                 , sns      <- [ pp s | s <- ss ]
                 , ess      <- [ e^.sing | ei <- is, let e = entTbl ! ei ] = zipWith helper sns ess
      where
        helper (T.breakOn " finger" -> (sn, _)) es = parensPad 15 sn <> es
    none = wrapUnlines cols $ if
      | descI == i      -> dudeYou'reNaked
      | descT == PCType -> parsePCDesig i mt pt $ d <> " doesn't have anything readied."
      | otherwise       -> theOnLowerCap descS   <> " doesn't have anything readied."
    header = wrapUnlines cols $ if
      | descI == i      -> "You have readied the following equipment:"
      | descT == PCType -> parsePCDesig i mt pt $ d <> " has readied the following equipment:"
      | otherwise       -> theOnLowerCap descS   <> " has readied the following equipment:"
    d = mkSerializedNonStdDesig descI mt pt descS The


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


mkGetLookBindings :: Id
                  -> CoinsTbl
                  -> EntTbl
                  -> InvTbl
                  -> MobTbl
                  -> PCTbl
                  -> TypeTbl
                  -> (PCDesig, Id, Inv, Inv, Coins)
mkGetLookBindings i ct et it mt pt tt | (d, _, _, ri, ris@((i `delete`) -> ris')) <- mkCapStdDesig i et it mt pt tt
                                      , rc                                        <- ct ! ri = (d, ri, ris, ris', rc)


-----


type IsConInRm  = Bool
type InvWithCon = Inv


mkMaybeNthOfM :: IsConInRm -> EntTbl -> Id -> Ent -> InvWithCon -> Maybe NthOfM
mkMaybeNthOfM icir et i (view sing -> s) is = guard icir >> (return . helper . dup $ matches)
  where
    helper  = succ . fromJust . elemIndex i *** length
    matches = filter (\i' -> (et ! i')^.sing == s) is


-----


mkPossPro :: Sex -> T.Text
mkPossPro Male   = "his"
mkPossPro Female = "her"
mkPossPro s      = patternMatchFail "mkPossPro" [ showText s ]


-----


mkPutRemBindings :: Id
                 -> CoinsTbl
                 -> EntTbl
                 -> InvTbl
                 -> MobTbl
                 -> PCTbl
                 -> TypeTbl
                 -> Args
                 -> (PCDesig, Inv, Coins, Inv, Coins, ConName, Args)
mkPutRemBindings i ct et it mt pt tt as = let (d, _, _, ri, (i `delete`) -> ris) = mkCapStdDesig i et it mt pt tt
                                              pis                      = it ! i
                                              (pc, rc)                 = over both (ct !) (i, ri)
                                              cn                       = last as
                                              (init -> argsWithoutCon) = case as of
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
                -> (EqTbl, InvTbl, [Broadcast], [T.Text])
                -> EqMap
                -> Slot
                -> Id
                -> (T.Text, Broadcast)
                -> (EqTbl, InvTbl, [Broadcast], [T.Text])
moveReadiedItem i a@(et, it, _, _) em s ei (msg, b) = let et' = et & at i ?~ (em & at s ?~ ei)
                                                          it' = it & at i ?~ filter (/= ei) (it ! i)
                                                          bs  = mkBroadcast i msg ++ [b]
                                                      in a & _1 .~ et' & _2 .~ it' & _3 <>~ bs & _4 <>~ [msg]


-----


otherHand :: Hand -> Hand
otherHand RHand  = LHand
otherHand LHand  = RHand
otherHand NoHand = NoHand


-----


putOnMsgs :: Id -> PCDesig -> Sing -> (T.Text, Broadcast)
putOnMsgs = mkReadyMsgs "put on" "puts on"


-----


resolvePCInvCoins :: Id
                  -> EntTbl
                  -> MobTbl
                  -> PCTbl
                  -> Args
                  -> Inv
                  -> Coins
                  -> ([Either T.Text Inv], [Either [T.Text] Coins])
resolvePCInvCoins i et mt pt as is c | (gecrs, miss, rcs) <- resolveEntCoinNames i et mt pt as is c
                                     , eiss               <- zipWith (curry procGecrMisPCInv) gecrs miss
                                     , ecs                <- map procReconciledCoinsPCInv rcs = (eiss, ecs)


-----


resolveRmInvCoins :: Id
                  -> EntTbl
                  -> MobTbl
                  -> PCTbl
                  -> Args
                  -> Inv
                  -> Coins
                  -> ([Either T.Text Inv], [Either [T.Text] Coins])
resolveRmInvCoins i et mt pt as is c | (gecrs, miss, rcs) <- resolveEntCoinNames i et mt pt as is c
                                     , eiss               <- zipWith (curry procGecrMisRm) gecrs miss
                                     , ecs                <- map procReconciledCoinsRm rcs = (eiss, ecs)
