{-# LANGUAGE FlexibleContexts, LambdaCase, MultiWayIf, NamedFieldPuns, OverloadedStrings, RankNTypes, RecordWildCards, TupleSections, ViewPatterns #-}

-- This module contains helper functions used by multiple functions in "Mud.Cmds.Pla", as well as helper functions used
-- by both "Mud.Cmds.Pla" and "Mud.Cmds.ExpCmds".

module Mud.Cmds.Util.Pla ( adminTagTxt
                         , alertMsgHelper
                         , armSubToSlot
                         , bugTypoLogger
                         , checkMutuallyTuned
                         , clothToSlot
                         , donMsgs
                         , execIfPossessed
                         , fillerToSpcs
                         , findAvailSlot
                         , genericAction
                         , genericActionWithHooks
                         , genericSorry
                         , genericSorryWithHooks
                         , getMatchingChanWithName
                         , getRelativePCName
                         , hasFp
                         , hasHp
                         , hasMp
                         , hasPp
                         , helperDropEitherInv
                         , helperGetDropEitherCoins
                         , helperGetEitherInv
                         , helperGiveEitherCoins
                         , helperGiveEitherInv
                         , helperLinkUnlink
                         , helperPutEitherCoins
                         , helperPutEitherInv
                         , helperRemEitherCoins
                         , helperRemEitherInv
                         , inOutOnOffs
                         , InvWithCon
                         , IsConInRm
                         , isNonStdLink
                         , isRingRol
                         , isRndmName
                         , isSlotAvail
                         , maybeSingleSlot
                         , mkChanBindings
                         , mkChanNamesTunings
                         , mkCoinsDesc
                         , mkCoinsSummary
                         , mkEffDxDesc
                         , mkEffHtDesc
                         , mkEffMaDesc
                         , mkEffPsDesc
                         , mkEffStDesc
                         , mkEntDesc
                         , mkEntDescs
                         , mkEqDesc
                         , mkExitsSummary
                         , mkFpDesc
                         , mkFullDesc
                         , mkHpDesc
                         , mkInvCoinsDesc
                         , mkLastArgIsTargetBindings
                         , mkLastArgWithNubbedOthers
                         , mkMaybeNthOfM
                         , mkMpDesc
                         , mkPpDesc
                         , mkReadyMsgs
                         , moveReadiedItem
                         , notFoundSuggestAsleeps
                         , onOffs
                         , otherHand
                         , putOnMsgs
                         , resolveMobInvCoins
                         , resolveRmInvCoins
                         , sorryConHelper
                         , spiritHelper ) where

import Mud.Cmds.Msgs.Dude
import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Msgs.Sorry
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
import Mud.Misc.Misc
import Mud.Misc.NameResolution
import Mud.TheWorld.Zones.AdminZoneIds (iRoot)
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Padding
import Mud.TopLvlDefs.Vols
import Mud.TopLvlDefs.Weights
import Mud.Util.List
import Mud.Util.Misc hiding (blowUp, patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import Prelude hiding (pi)
import qualified Mud.Misc.Logging as L (logNotice, logPla, logPlaOut)
import qualified Mud.Util.Misc as U (blowUp, patternMatchFail)

import Control.Arrow ((***), (&&&), first, second)
import Control.Lens (Getter, _1, _2, _3, _4, _5, at, both, each, to, view, views)
import Control.Lens.Operators ((%~), (&), (.~), (<>~), (?~), (^.))
import Control.Monad ((>=>), forM_, guard)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Char (isLower)
import Data.Function (on)
import Data.List ((\\), delete, elemIndex, find, foldl', intercalate, nub, sortBy)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Sum(..))
import Data.Text (Text)
import qualified Data.IntMap.Lazy as IM (keys)
import qualified Data.Map.Lazy as M ((!), notMember, toList)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V (Vector)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Cmds.Util.Pla"


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.Pla"


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.Util.Pla"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Cmds.Util.Pla"


logPlaOut :: Text -> Id -> [Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Cmds.Util.Pla"


-- ==================================================


alertMsgHelper :: Id -> CmdName -> Text -> MudStack ()
alertMsgHelper i cn txt = getState >>= \ms -> if isAdminId i ms
  then unit
  else let matches = filter (`T.isInfixOf` T.toLower txt) alertMsgTriggers
       in if ()!# matches
         then liftIO mkTimestamp >>= \ts ->
             let match = head matches
                 s      = getSing i ms
                 msg    = T.concat [ s
                                   , " issued a message via the "
                                   , dblQuote cn
                                   , " command containing the word "
                                   , dblQuote match
                                   , ": "
                                   , txt ]
                 outIds = (iRoot `delete`) $ getAdminIds ms \\ getLoggedInAdminIds ms
                 rec    = AlertMsgRec ts s cn match txt
             in do logNotice        "alertMsgHelper"   msg
                   logPla           "alertMsgHelper" i msg
                   bcastAdmins msg
                   forM_ outIds (\adminId -> retainedMsg adminId ms . mkRetainedMsgFromPerson s $ msg)
                   withDbExHandler_ "alertMsgHelper" . insertDbTblAlertMsg $ rec
         else unit


-----


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
        mkLoc = parensQuote (showText ri) |<>| view rmName (getRm ri ms)
    in liftIO mkTimestamp >>= \ts -> do
        logPla "bugTypoLogger" i . T.concat $ [ "logging a ", showText wl, ": ", msg ]
        sequence_ $ case wl of BugLog  -> let b = BugRec ts s mkLoc msg
                                          in [ withDbExHandler_ "bugTypoLogger" . insertDbTblBug $ b
                                             , bcastOtherAdmins i $ s <> " has logged a bug: "  <> pp b ]
                               TypoLog -> let t = TypoRec ts s mkLoc msg
                                          in [ withDbExHandler_ "bugTypoLogger" . insertDbTblTypo $ t
                                             , bcastOtherAdmins i $ s <> " has logged a typo: " <> pp t ]
        send mq . nlnl $ "Thank you."
bugTypoLogger p _ = patternMatchFail "bugTypoLogger" . showText $ p


-----


checkMutuallyTuned :: Id -> MudState -> Sing -> Either Text Id
checkMutuallyTuned i ms targetSing = case areMutuallyTuned of
  (False, _,     _       ) -> Left . sorryTunedOutPCSelf $ targetSing
  (True,  False, _       ) -> Left . (effortsBlockedMsg <>) . sorryTunedOutPCTarget $ targetSing
  (True,  True,  targetId) -> Right targetId
  where
    areMutuallyTuned | targetId <- getIdForPCSing targetSing ms
                     , a <- (M.! targetSing) . getTeleLinkTbl i        $ ms
                     , b <- (M.! s         ) . getTeleLinkTbl targetId $ ms
                     = (a, b, targetId)
    s                = getSing i ms


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


donMsgs :: Id -> Desig -> Sing -> (Text, Broadcast)
donMsgs = mkReadyMsgs "don" "dons"


type SndPerVerb = Text
type ThrPerVerb = Text


mkReadyMsgs :: SndPerVerb -> ThrPerVerb -> Id -> Desig -> Sing -> (Text, Broadcast)
mkReadyMsgs spv tpv i d s = (  T.concat [ "You ", spv, " the ", s, "." ]
                            , (T.concat [ serialize d, spaced tpv, aOrAn s, "." ], i `delete` desigIds d) )


-----


execIfPossessed :: ActionParams -> CmdName -> ActionFun -> MudStack ()
execIfPossessed p@(WithArgs i mq cols _) cn f = getState >>= \ms ->
    let s = getSing i ms in case getPossessor i ms of
      Nothing -> wrapSend mq cols (sorryNotPossessed s cn)
      Just _  -> f p
execIfPossessed p _ _ = patternMatchFail "execIfPossessed" . showText $ p


-----


genericAction :: ActionParams
              -> (MudState -> GenericRes)
              -> Text
              -> MudStack ()
genericAction p helper fn = helper |&| modifyState >=> \(toSelfs, bs, logMsgs) ->
    genericActionHelper p fn toSelfs bs logMsgs


genericActionHelper :: ActionParams -> Text -> [Text] -> [Broadcast] -> [Text] -> MudStack ()
genericActionHelper ActionParams { .. } fn toSelfs bs logMsgs = getState >>= \ms -> do
    logMsgs |#| logPlaOut fn myId . map (parseExpandDesig myId ms)
    multiWrapSend plaMsgQueue plaCols [ parseDesig myId ms msg | msg <- toSelfs ]
    bcastIfNotIncogNl myId bs


genericActionWithHooks :: ActionParams
                       -> (V.Vector Int -> MudState -> GenericResWithHooks)
                       -> Text
                       -> MudStack ()
genericActionWithHooks p helper fn = mkRndmVector >>= \v ->
    helper v |&| modifyState >=> \(toSelfs, bs, logMsgs, fs) -> do genericActionHelper p fn toSelfs bs logMsgs
                                                                   sequence_ fs


-----


genericSorry :: MudState -> Text -> GenericRes
genericSorry ms = (ms, ) . (, [], []) . pure


genericSorryWithHooks :: MudState -> Text -> GenericResWithHooks
genericSorryWithHooks ms = (ms, ) . (, [], [], []) . pure


-----


getMatchingChanWithName :: Text -> [ChanName] -> [Chan] -> (ChanName, Chan)
getMatchingChanWithName match cns cs = let cn  = head . filter ((== match) . T.toLower) $ cns
                                           c   = head . filter (views chanName (== cn)) $ cs
                                       in (cn, c)


-----


getRelativePCName :: MudState -> (Id, Id) -> MudStack Text
getRelativePCName ms pair@(_, y)
  | isLinked ms pair = return . getSing y $ ms
  | otherwise        = underline <$> uncurry updateRndmName pair


-----


hasHp :: Id -> MudState -> Int -> Bool
hasHp = hasPoints curHp


hasMp :: Id -> MudState -> Int -> Bool
hasMp = hasPoints curMp


hasPp :: Id -> MudState -> Int -> Bool
hasPp = hasPoints curPp


hasFp :: Id -> MudState -> Int -> Bool
hasFp = hasPoints curFp


hasPoints :: Getter Mob Int -> Id -> MudState -> Int -> Bool
hasPoints lens i ms amt = views (mobTbl.ind i.lens) (>= amt) ms


-----


type FromId   = Id
type FromSing = Sing
type ToId     = Id
type ToSing   = Sing


helperDropEitherInv :: Id
                    -> Desig
                    -> FromId
                    -> ToId
                    -> GenericIntermediateRes
                    -> Either Text Inv
                    -> GenericIntermediateRes
helperDropEitherInv i d fromId toId a@(ms, _, _, _) = \case
  Left  msg -> a & _2 <>~ pure msg
  Right is  -> let (toSelfs, bs) = mkGetDropInvDescs i ms d Drop is
               in a & _1.invTbl.ind fromId %~  (\\ is)
                    & _1.invTbl.ind toId   %~  addToInv ms is
                    & _2                   <>~ toSelfs
                    & _3                   <>~ bs
                    & _4                   <>~ toSelfs


mkGetDropInvDescs :: Id -> MudState -> Desig -> GetOrDrop -> Inv -> ([Text], [Broadcast])
mkGetDropInvDescs i ms d god (mkName_maybeCorpseId_count_bothList i ms -> tuple) = unzip . map helper $ tuple
  where
    helper (_, mci, c, (s, _)) | c == 1 =
        (  T.concat [ "You ",               mkGodVerb god SndPer,   " the ", s,          "." ]
        , (T.concat [ serialize d, spaced . mkGodVerb god $ ThrPer, renderMaybeCorpseId, "." ], otherIds) )
      where
        renderMaybeCorpseId = case mci of (Just ci) -> ("the " <>) . serialize . CorpseDesig $ ci
                                          Nothing   -> aOrAn s
    helper (_, _, c, b) =
        (  T.concat [ "You ",           mkGodVerb god SndPer, rest ]
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, rest ], otherIds) )
      where
        rest = prd $ spaced (showText c) <> mkPlurFromBoth b
    otherIds = i `delete` desigIds d


mkGodVerb :: GetOrDrop -> Verb -> Text
mkGodVerb Get  SndPer = "pick up"
mkGodVerb Get  ThrPer = "picks up"
mkGodVerb Drop SndPer = "drop"
mkGodVerb Drop ThrPer = "drops"


-----


helperGetDropEitherCoins :: Id
                         -> Desig
                         -> GetOrDrop
                         -> FromId
                         -> ToId
                         -> GenericIntermediateRes
                         -> [Either [Text] Coins]
                         -> GenericIntermediateRes
helperGetDropEitherCoins i d god fromId toId (ms, toSelfs, bs, logMsgs) ecs =
    let (ms', toSelfs', logMsgs', canCoins) = foldl' helper (ms, toSelfs, logMsgs, mempty) ecs
    in (ms', toSelfs', bs ++ mkGetDropCoinsDescOthers i d god canCoins, logMsgs')
  where
    helper a@(ms', _, _, _) = \case
      Left  msgs -> a & _2 <>~ msgs
      Right c    -> let (can, can't) = case god of Get  -> partitionCoinsByEnc i ms' c
                                                   Drop -> (c, mempty)
                        toSelfs'     = mkGetDropCoinsDescsSelf god can
                    in a & _1.coinsTbl.ind fromId %~  (<> negateCoins can)
                         & _1.coinsTbl.ind toId   %~  (<>             can)
                         & _2                     <>~ toSelfs' ++ mkCan'tGetCoinsDesc can't
                         & _3                     <>~ toSelfs'
                         & _4                     <>~ can


partitionCoinsByEnc :: Id -> MudState -> Coins -> (Coins, Coins)
partitionCoinsByEnc = partitionCoinsHelper calcMaxEnc calcWeight coinWeight


partitionCoinsHelper :: (Id -> MudState -> Int)
                     -> (Id -> MudState -> Int)
                     -> Int
                     -> Id
                     -> MudState
                     -> Coins
                     -> (Coins, Coins)
partitionCoinsHelper calcMax calcCurr factor i ms coins = let maxAmt    = calcMax  i ms
                                                              currAmt   = calcCurr i ms
                                                              noOfCoins = sum . coinsToList $ coins
                                                              coinsAmt  = noOfCoins * factor
                                                          in if currAmt + coinsAmt <= maxAmt
                                                            then (coins, mempty)
                                                            else let availAmt     = maxAmt - currAmt
                                                                     canNoOfCoins = availAmt `quot` factor
                                                                 in mkCanCan'tCoins coins canNoOfCoins


mkCanCan'tCoins :: Coins -> Int -> (Coins, Coins)
mkCanCan'tCoins (Coins (c, 0, 0)) n = (Coins (n, 0, 0), Coins (c - n, 0,     0    ))
mkCanCan'tCoins (Coins (0, s, 0)) n = (Coins (0, n, 0), Coins (0,     s - n, 0    ))
mkCanCan'tCoins (Coins (0, 0, g)) n = (Coins (0, 0, n), Coins (0,     0,     g - n))
mkCanCan'tCoins c                 _ = patternMatchFail "mkCanCan'tCoins" . showText $ c


mkGetDropCoinsDescOthers :: Id -> Desig -> GetOrDrop -> Coins -> [Broadcast]
mkGetDropCoinsDescOthers i d god c =
  c |!| [ (T.concat [ serialize d, spaced . mkGodVerb god $ ThrPer, aCoinSomeCoins c, "." ], i `delete` desigIds d) ]


mkGetDropCoinsDescsSelf :: GetOrDrop -> Coins -> [Text]
mkGetDropCoinsDescsSelf god = mkCoinsMsgs helper
  where
    helper 1 cn = T.concat [ "You ", mkGodVerb god SndPer, " ", aOrAn cn,             "."  ]
    helper a cn = T.concat [ "You ", mkGodVerb god SndPer, spaced . showText $ a, cn, "s." ]


mkCan'tGetCoinsDesc :: Coins -> [Text]
mkCan'tGetCoinsDesc = mkCoinsMsgs (can'tCoinsDescHelper sorryGetEnc)


can'tCoinsDescHelper :: Text -> Int -> Text -> Text
can'tCoinsDescHelper t a cn = t <> bool (T.concat [ showText a, " ", cn, "s." ]) (prd $ "the " <> cn) (a == 1)


-----


helperGetEitherInv :: Id
                   -> Desig
                   -> FromId
                   -> GenericIntermediateRes
                   -> Either Text Inv
                   -> GenericIntermediateRes
helperGetEitherInv i d fromId a@(ms, _, _, _) = \case
  Left  msg                              -> a & _2 <>~ pure msg
  Right (sortByType -> (npcPCs, others)) ->
    let maxEnc            = calcMaxEnc i ms
        (_, cans, can'ts) = foldl' (partitionInvByEnc ms maxEnc)
                                   (calcWeight i ms, [], [])
                                   others
        (toSelfs, bs    ) = mkGetDropInvDescs i ms d Get cans
    in a & _1.invTbl.ind fromId %~  (\\ cans)
         & _1.invTbl.ind i      %~  addToInv ms cans
         & _2                   <>~ concat [ map sorryType npcPCs
                                           , toSelfs
                                           , mkCan'tGetInvDescs i ms maxEnc can'ts ]
         & _3                   <>~ bs
         & _4                   <>~ toSelfs
  where
    sortByType             = foldr helper mempties
    helper targetId sorted = let lens = case getType targetId ms of PCType  -> _1
                                                                    NpcType -> _1
                                                                    _       -> _2
                             in sorted & lens %~ (targetId :)
    sorryType targetId     = sorryGetType . serialize . mkStdDesig targetId ms $ Don'tCap


partitionInvByEnc :: MudState -> Weight -> (Weight, Inv, Inv) -> Id -> (Weight, Inv, Inv)
partitionInvByEnc = partitionInvHelper calcWeight


partitionInvHelper :: (Id -> MudState -> Int) -> MudState -> Int -> (Int, Inv, Inv) -> Id -> (Int, Inv, Inv)
partitionInvHelper f ms maxAmt acc@(x, _, _) targetId = let x' = x + f targetId ms
                                                            a  = acc & _1 .~ x'
                                                                     & _2 <>~ pure targetId
                                                            b  = acc & _3 <>~ pure targetId
                                                        in bool b a $ x' <= maxAmt


mkCan'tGetInvDescs :: Id -> MudState -> Weight -> Inv -> [Text]
mkCan'tGetInvDescs i ms maxEnc = concatMap helper
  where
    helper targetId | calcWeight targetId ms > maxEnc = pure . sorryGetWeight . getSing targetId $ ms
                    | otherwise                       = can'tInvDescsHelper sorryGetEnc i ms . pure $ targetId


can'tInvDescsHelper :: Text -> Id -> MudState -> Inv -> [Text]
can'tInvDescsHelper t i ms = map helper . mkNameCountBothList i ms
  where
    helper (_, c, b@(s, _)) = t <> bool (T.concat [ showText c, " ", mkPlurFromBoth b, "." ]) (prd $ "the " <> s) (c == 1)


-----


helperGiveEitherCoins :: Id
                      -> Desig
                      -> ToId
                      -> GenericIntermediateRes
                      -> [Either [Text] Coins]
                      -> GenericIntermediateRes
helperGiveEitherCoins i d toId (ms, toSelfs, bs, logMsgs) ecs =
    let toDesig                             = serialize . mkStdDesig toId ms $ Don'tCap
        (ms', toSelfs', logMsgs', canCoins) = foldl' (helper toDesig) (ms, toSelfs, logMsgs, mempty) ecs
    in (ms', toSelfs', bs ++ mkGiveCoinsDescOthers i d toId toDesig canCoins, logMsgs')
  where
    helper toDesig a@(ms', _, _, _) = \case
      Left  msgs -> a & _2 <>~ msgs
      Right c    -> let (can, can't) = partitionCoinsByEnc toId ms' c
                        toSelfs'     = mkGiveCoinsDescsSelf toDesig can
                    in a & _1.coinsTbl.ind i    %~  (<> negateCoins can)
                         & _1.coinsTbl.ind toId %~  (<>             can)
                         & _2                   <>~ toSelfs' ++ mkCan'tGiveCoinsDesc toDesig can't
                         & _3                   <>~ toSelfs'
                         & _4                   <>~ can


mkGiveCoinsDescOthers :: Id -> Desig -> ToId -> Text -> Coins -> [Broadcast]
mkGiveCoinsDescOthers i d toId toDesig c = c |!| toOthersBcast : [ (msg, pure toId) | msg <- toTargetMsgs ]
  where
    toOthersBcast = ( T.concat [ serialize d, " gives ", aCoinSomeCoins c, " to ", toDesig, "." ]
                    , desigIds d \\ [ i, toId ] )
    toTargetMsgs  = mkCoinsMsgs helper c
    helper 1 cn   = T.concat [ serialize d, " gives you ", aOrAn cn,                  "."  ]
    helper a cn   = T.concat [ serialize d, " gives you",  spaced . showText $ a, cn, "s." ]


mkGiveCoinsDescsSelf :: Text -> Coins -> [Text]
mkGiveCoinsDescsSelf targetDesig = mkCoinsMsgs helper
  where
    helper 1 cn = T.concat [ "You give ", aOrAn cn,                  " to ",  targetDesig, "." ]
    helper a cn = T.concat [ "You give",  spaced . showText $ a, cn, "s to ", targetDesig, "." ]


mkCan'tGiveCoinsDesc :: Text -> Coins -> [Text]
mkCan'tGiveCoinsDesc targetDesig = mkCoinsMsgs (can'tCoinsDescHelper . sorryGiveEnc $ targetDesig)


-----


helperGiveEitherInv :: Id
                    -> Desig
                    -> ToId
                    -> GenericIntermediateRes
                    -> Either Text Inv
                    -> GenericIntermediateRes
helperGiveEitherInv i d toId a@(ms, _, _, _) = \case
  Left  msg -> a & _2 <>~ pure msg
  Right is  ->
    let (_, cans, can'ts) = foldl' (partitionInvByEnc ms . calcMaxEnc toId $ ms) (calcWeight toId ms, [], []) is
        (toSelfs, bs    ) = mkGiveInvDescs i ms d toId (serialize toDesig) cans
        toDesig           = mkStdDesig toId ms Don'tCap
    in a & _1.invTbl.ind i    %~  (\\ cans)
         & _1.invTbl.ind toId %~  addToInv ms cans
         & _2                 <>~ toSelfs ++
                                  mkCan'tGiveInvDescs (serialize toDesig { desigShouldCap = DoCap }) i ms can'ts
         & _3                 <>~ bs
         & _4                 <>~ toSelfs


mkGiveInvDescs :: Id -> MudState -> Desig -> ToId -> Text -> Inv -> ([Text], [Broadcast])
mkGiveInvDescs i ms d toId toDesig = second concat . unzip . map helper . mkNameCountBothList i ms
  where
    helper (_, c, (s, _)) | c == 1 =
        ( T.concat [ "You give the ", s, " to ", toDesig, "." ]
        , [ (T.concat [ serialize d, " gives ",     aOrAn s, " to ", toDesig, "." ], otherIds )
          , (T.concat [ serialize d, " gives you ", aOrAn s,                  "." ], pure toId) ] )
    helper (_, c, b) =
        ( T.concat [ "You give", stuff, " to ", toDesig, "." ]
        , [ (T.concat [ serialize d, " gives",     stuff, " to ", toDesig, "." ], otherIds )
          , (T.concat [ serialize d, " gives you", stuff,                  "." ], pure toId) ] )
      where
        stuff = spaced (showText c) <> mkPlurFromBoth b
    otherIds = desigIds d \\ [ i, toId ]


mkCan'tGiveInvDescs :: Text -> Id -> MudState -> Inv -> [Text]
mkCan'tGiveInvDescs = can'tInvDescsHelper


-----


helperLinkUnlink :: MudState -> Id -> MsgQueue -> Cols -> MudStack (Maybe ([Text], [Text], [Text]))
helperLinkUnlink ms i mq cols =
    let s                = getSing   i ms
        othersLinkedToMe = getLinked i ms
        meLinkedToOthers = foldr buildSingList [] $ i `delete` views pcTbl IM.keys ms
        buildSingList pi acc | s `elem` getLinked pi ms = getSing pi ms : acc
                             | otherwise                = acc
        twoWays = map fst . filter ((== 2) . snd) . countOccs $ othersLinkedToMe ++ meLinkedToOthers
    in if all (()#) [ othersLinkedToMe, meLinkedToOthers ]
      then emptied $ do logPlaOut "helperLinkUnlink" i . pure $ sorryNoLinks
                        wrapSend mq cols (isSpiritId i ms ? sorryNoLinksSpirit :? sorryNoLinks)
      else unadulterated (meLinkedToOthers, othersLinkedToMe, twoWays)


-----


type NthOfM = (Int, Int)


helperPutEitherCoins :: Id
                     -> Desig
                     -> Maybe NthOfM
                     -> ToId
                     -> ToSing
                     -> GenericIntermediateRes
                     -> [Either [Text] Coins]
                     -> GenericIntermediateRes
helperPutEitherCoins i d mnom toId toSing (ms, toSelfs, bs, logMsgs) ecs =
    let (ms', toSelfs', logMsgs', canCoins) = foldl' helper (ms, toSelfs, logMsgs, mempty) ecs
    in (ms', toSelfs', bs ++ mkPutRemCoinsDescOthers i d Put mnom mci toSing canCoins, logMsgs')
  where
    mci                     = mkMaybeCorpseId toId ms
    helper a@(ms', _, _, _) = \case
      Left  msgs -> a & _2 <>~ msgs
      Right c    -> let (can, can't) = partitionCoinsByVol toId ms' c
                        toSelfs'     = mkPutRemCoinsDescsSelf Put mnom mci toSing can
                    in a & _1.coinsTbl.ind i    %~  (<> negateCoins can)
                         & _1.coinsTbl.ind toId %~  (<>             can)
                         & _2                   <>~ toSelfs' ++ mkCan'tPutCoinsDesc toSing can't
                         & _3                   <>~ toSelfs'
                         & _4                   <>~ can


partitionCoinsByVol :: Id -> MudState -> Coins -> (Coins, Coins)
partitionCoinsByVol = partitionCoinsHelper getConCapacity calcInvCoinsVol coinVol


mkPutRemCoinsDescOthers :: Id -> Desig -> PutOrRem -> Maybe NthOfM -> Maybe Id -> Sing -> Coins -> [Broadcast]
mkPutRemCoinsDescOthers i d por mnom mci conSing c = c |!| pure ( T.concat [ serialize d
                                                                           , spaced . mkPorVerb por $ ThrPer
                                                                           , aCoinSomeCoins c
                                                                           , " "
                                                                           , mkPorPrep por ThrPer mnom mci conSing
                                                                           , prd . onTheGround $ mnom ]
                                                               , i `delete` desigIds d )


mkPutRemCoinsDescsSelf :: PutOrRem -> Maybe NthOfM -> Maybe Id -> Sing -> Coins -> [Text]
mkPutRemCoinsDescsSelf por mnom mci conSing = mkCoinsMsgs helper
  where
    helper a cn | a == 1 = T.concat [ partA, aOrAn cn,   " ",           partB ]
    helper a cn          = T.concat [ partA, showText a, " ", cn, "s ", partB ]
    partA                = spcR $ "You " <> mkPorVerb por SndPer
    partB                = prd $ mkPorPrep por SndPer mnom mci conSing <> onTheGround mnom


mkPorVerb :: PutOrRem -> Verb -> Text
mkPorVerb Put SndPer = "put"
mkPorVerb Put ThrPer = "puts"
mkPorVerb Rem SndPer = "remove"
mkPorVerb Rem ThrPer = "removes"


mkPorPrep :: PutOrRem -> Verb -> Maybe NthOfM -> Maybe Id -> Sing -> Text
mkPorPrep Put SndPer Nothing       Nothing  = ("in the "   <>)
mkPorPrep Rem SndPer Nothing       Nothing  = ("from the " <>)
mkPorPrep Put ThrPer Nothing       Nothing  = ("in "       <>) . aOrAn
mkPorPrep Rem ThrPer Nothing       Nothing  = ("from "     <>) . aOrAn
mkPorPrep Put SndPer Nothing       (Just i) = const $ "on the "   <> serialize (CorpseDesig i)
mkPorPrep Rem SndPer Nothing       (Just i) = const $ "from the " <> serialize (CorpseDesig i)
mkPorPrep Put ThrPer Nothing       (Just i) = const $ "on the "   <> serialize (CorpseDesig i)
mkPorPrep Rem ThrPer Nothing       (Just i) = const $ "from the " <> serialize (CorpseDesig i)
mkPorPrep Put SndPer (Just (n, m)) Nothing  = ("in the "   <>) . (descNthOfM n m <>)
mkPorPrep Rem SndPer (Just (n, m)) Nothing  = ("from the " <>) . (descNthOfM n m <>)
mkPorPrep Put ThrPer (Just (n, m)) Nothing  = ("in the "   <>) . (descNthOfM n m <>)
mkPorPrep Rem ThrPer (Just (n, m)) Nothing  = ("from the " <>) . (descNthOfM n m <>)
mkPorPrep Put SndPer (Just (n, m)) (Just i) = const $ "on the "   <> descNthOfM n m <> serialize (CorpseDesig i)
mkPorPrep Rem SndPer (Just (n, m)) (Just i) = const $ "from the " <> descNthOfM n m <> serialize (CorpseDesig i)
mkPorPrep Put ThrPer (Just (n, m)) (Just i) = const $ "on the "   <> descNthOfM n m <> serialize (CorpseDesig i)
mkPorPrep Rem ThrPer (Just (n, m)) (Just i) = const $ "from the " <> descNthOfM n m <> serialize (CorpseDesig i)


descNthOfM :: Int -> Int -> Text
descNthOfM 1 1 = ""
descNthOfM n _ = spcR . mkOrdinal $ n


onTheGround :: Maybe NthOfM -> Text
onTheGround = (|!| " on the ground") . ((both %~ Sum) <$>)


mkCan'tPutCoinsDesc :: Sing -> Coins -> [Text]
mkCan'tPutCoinsDesc conSing = mkCoinsMsgs (can'tCoinsDescHelper . sorryPutVol $ conSing)


-----


helperPutEitherInv :: Id
                   -> Desig
                   -> Maybe NthOfM
                   -> ToId
                   -> ToSing
                   -> GenericIntermediateRes
                   -> Either Text Inv
                   -> GenericIntermediateRes
helperPutEitherInv i d mnom toId toSing a@(ms, origToSelfs, _, _) = \case
  Left  msg -> a & _2 <>~ pure msg
  Right is  ->
    let (is',      toSelfs) = onTrue (toId `elem` is) f (is, origToSelfs)
        f                   = filter (/= toId) *** (<> pure (sorryPutInsideSelf toSing))
        (_, cans,  can'ts ) = foldl' (partitionInvByVol ms . getConCapacity toId $ ms)
                                     (calcInvCoinsVol toId ms, [], [])
                                     is'
        (toSelfs', bs     ) = mkPutRemInvDescs i ms d Put mnom (mkMaybeCorpseId toId ms) toSing cans
    in a & _1.invTbl.ind i    %~  (\\ cans)
         & _1.invTbl.ind toId %~  addToInv ms cans
         & _2                 .~  concat [ toSelfs, toSelfs', mkCan'tPutInvDescs toSing i ms can'ts ]
         & _3                 <>~ bs
         & _4                 <>~ toSelfs'


partitionInvByVol :: MudState -> Vol -> (Vol, Inv, Inv) -> Id -> (Vol, Inv, Inv)
partitionInvByVol = partitionInvHelper calcVol


mkPutRemInvDescs :: Id
                 -> MudState
                 -> Desig
                 -> PutOrRem
                 -> Maybe NthOfM
                 -> Maybe Id
                 -> Sing
                 -> Inv
                 -> ([Text], [Broadcast])
mkPutRemInvDescs i ms d por mnom mci conSing = unzip . map helper . mkNameCountBothList i ms
  where
    helper (_, c, (s, _)) | c == 1 =
        (  T.concat [ "You "
                    , mkPorVerb por SndPer
                    , spaced withArticle
                    , mkPorPrep por SndPer mnom mci conSing
                    , rest ]
        , (T.concat [ serialize d
                    , spaced . mkPorVerb por $ ThrPer
                    , aOrAn s
                    , " "
                    , mkPorPrep por ThrPer mnom mci conSing
                    , rest ], otherIds) )
      where
        withArticle = bool (aOrAn s) ("the " <> s) $ por == Put
    helper (_, c, b) =
        (  T.concat [ "You "
                    , mkPorVerb por SndPer
                    , spaced . showText $ c
                    , mkPlurFromBoth b
                    , " "
                    , mkPorPrep por SndPer mnom mci conSing
                    , rest ]
        , (T.concat [ serialize d
                    , spaced . mkPorVerb por $ ThrPer
                    , showText c
                    , spaced . mkPlurFromBoth $ b
                    , mkPorPrep por ThrPer mnom mci conSing
                    , rest ], otherIds) )
    rest     = prd . onTheGround $ mnom
    otherIds = i `delete` desigIds d


mkCan'tPutInvDescs :: ToSing -> Id -> MudState -> Inv -> [Text]
mkCan'tPutInvDescs = can'tInvDescsHelper . sorryPutVol


-----


helperRemEitherCoins :: Id
                     -> Desig
                     -> Maybe NthOfM
                     -> FromId
                     -> FromSing
                     -> IsConInRm
                     -> GenericIntermediateRes
                     -> [Either [Text] Coins]
                     -> GenericIntermediateRes
helperRemEitherCoins i d mnom fromId fromSing icir (ms, toSelfs, bs, logMsgs) ecs =
    let (ms', toSelfs', logMsgs', canCoins) = foldl' helper (ms, toSelfs, logMsgs, mempty) ecs
    in (ms', toSelfs', bs ++ mkPutRemCoinsDescOthers i d Rem mnom mci fromSing canCoins, logMsgs')
  where
    mci                     = mkMaybeCorpseId fromId ms
    helper a@(ms', _, _, _) = \case
      Left  msgs -> a & _2 <>~ msgs
      Right c    -> let (can, can't)  = f c
                        f | icir      = partitionCoinsByEnc i ms'
                          | otherwise = (, mempty)
                        toSelfs'      = mkPutRemCoinsDescsSelf Rem mnom mci fromSing can
                    in a & _1.coinsTbl.ind fromId %~ (<> negateCoins can)
                         & _1.coinsTbl.ind i      %~ (<>             can)
                         & _2 <>~ toSelfs' ++ mkCan'tRemCoinsDesc can't
                         & _3 <>~ toSelfs'
                         & _4 <>~ c


mkCan'tRemCoinsDesc :: Coins -> [Text]
mkCan'tRemCoinsDesc = mkCoinsMsgs (can'tCoinsDescHelper sorryRemEnc)


-----


helperRemEitherInv :: Id
                   -> Desig
                   -> Maybe NthOfM
                   -> FromId
                   -> FromSing
                   -> IsConInRm
                   -> GenericIntermediateRes
                   -> Either Text Inv
                   -> GenericIntermediateRes
helperRemEitherInv i d mnom fromId fromSing icir a@(ms, _, _, _) = \case
  Left  msg -> a & _2 <>~ pure msg
  Right is  -> let (_, cans, can'ts) = f is
                   f | icir          = foldl' (partitionInvByEnc ms . calcMaxEnc i $ ms) (calcWeight i ms, [], [])
                     | otherwise     = (0, , [])
                   (toSelfs, bs)     = mkPutRemInvDescs i ms d Rem mnom (mkMaybeCorpseId fromId ms) fromSing cans
               in a & _1.invTbl.ind fromId %~  (\\ cans)
                    & _1.invTbl.ind i      %~  addToInv ms cans
                    & _2                   <>~ toSelfs ++ mkCan'tRemInvDescs i ms can'ts
                    & _3                   <>~ bs
                    & _4                   <>~ toSelfs


mkCan'tRemInvDescs :: Id -> MudState -> Inv -> [Text]
mkCan'tRemInvDescs = can'tInvDescsHelper sorryRemEnc


-----


inOutOnOffs :: [(Text, Bool)]
inOutOnOffs = [ ("i",   otherwise)
              , ("in",  otherwise)
              , ("o",   likewise )
              , ("of",  likewise )
              , ("off", likewise )
              , ("on",  otherwise)
              , ("ou",  likewise )
              , ("out", likewise ) ]


-----


isRingRol :: RightOrLeft -> Bool
isRingRol = \case R -> False
                  L -> False
                  _ -> True


-----


isRndmName :: Text -> Bool
isRndmName = isLower . T.head . dropANSI


-----


isSlotAvail :: EqMap -> Slot -> Bool
isSlotAvail = flip M.notMember


findAvailSlot :: EqMap -> [Slot] -> Maybe Slot
findAvailSlot em = find (isSlotAvail em)


-----


maybeSingleSlot :: EqMap -> Slot -> Maybe Slot
maybeSingleSlot em s = boolToMaybe (isSlotAvail em s) s


-----


mkChanBindings :: Id -> MudState -> ([Chan], [ChanName], Sing)
mkChanBindings i ms = let cs  = getPCChans i ms
                          cns = select chanName cs
                      in (cs, cns, getSing i ms)


-----


mkChanNamesTunings :: Id -> MudState -> ([Text], [Bool])
mkChanNamesTunings i ms = unzip . sortBy (compare `on` fst) . map helper . getPCChans i $ ms
  where
    helper = view chanName &&& views chanConnTbl (M.! getSing i ms)


-----


mkCoinsDesc :: Cols -> Coins -> Text
mkCoinsDesc cols (Coins (each %~ Sum -> (cop, sil, gol))) =
    T.unlines . intercalate [""] . map (wrap cols) . dropEmpties $ [ cop |!| copDesc
                                                                   , sil |!| silDesc
                                                                   , gol |!| golDesc ]
  where
    copDesc = "The copper piece is round and shiny."
    silDesc = "The silver piece is round and shiny."
    golDesc = "The gold piece is round and shiny."


-----


mkEffStDesc :: Id -> MudState -> Text
mkEffStDesc = mkEffDesc getBaseSt calcEffSt "weaker" "stronger"


mkEffDesc :: (Id -> MudState -> Int) -> (Id -> MudState -> Int) -> Text -> Text -> Id -> MudState -> Text
mkEffDesc f g lessAdj moreAdj i ms =
    let x    = f i ms
        y    = g i ms - x
        p    = y `percent` x
        over = let (q, r) = y `quotRem` x
                   t      = mkDescForPercent (r `percent` x) [ (19,  "00")
                                                             , (39,  "20")
                                                             , (59,  "40")
                                                             , (79,  "60")
                                                             , (99,  "80") ]
               in colorWith magenta . T.concat $ [ "You feel ", showText q, t, "% ", moreAdj, " than usual." ]
    in mkDescForPercent p [ (-84, colorWith magenta $ "You feel immensely "    <> lessAdj <> " than usual.")
                          , (-70, colorWith red     $ "You feel exceedingly "  <> lessAdj <> " than usual.")
                          , (-56,                     "You feel quite a bit "  <> lessAdj <> " than usual.")
                          , (-42,                     "You feel considerably " <> lessAdj <> " than usual.")
                          , (-28,                     "You feel moderately "   <> lessAdj <> " than usual.")
                          , (-14,                     "You feel a little "     <> lessAdj <> " than usual.")
                          , (0,                       ""                                                   )
                          , (14,                      "You feel a little "     <> moreAdj <> " than usual.")
                          , (28,                      "You feel moderately "   <> moreAdj <> " than usual.")
                          , (42,                      "You feel considerably " <> moreAdj <> " than usual.")
                          , (56,                      "You feel quite a bit "  <> moreAdj <> " than usual.")
                          , (70,  colorWith red     $ "You feel strikingly "   <> moreAdj <> " than usual.")
                          , (84,  colorWith magenta $ "You feel exceedingly "  <> moreAdj <> " than usual.")
                          , (99,  colorWith magenta $ "You feel immensely "    <> moreAdj <> " than usual.")
                          , (100, over                                                                     ) ]


mkEffDxDesc :: Id -> MudState -> Text
mkEffDxDesc = mkEffDesc getBaseDx calcEffDx "less agile" "more agile"


mkEffHtDesc :: Id -> MudState -> Text
mkEffHtDesc = mkEffDesc getBaseHt calcEffHt "less vigorous" "more vigorous"


mkEffMaDesc :: Id -> MudState -> Text
mkEffMaDesc = mkEffDesc getBaseMa calcEffMa "less proficient in magic" "more proficient in magic"


mkEffPsDesc :: Id -> MudState -> Text
mkEffPsDesc = mkEffDesc getBasePs calcEffPs "less proficient in psionics" "more proficient in psionics"


-----


mkEntDescs :: Id -> Cols -> MudState -> Inv -> Text
mkEntDescs i cols ms eis = T.intercalate theNl [ mkEntDesc i cols ms (ei, e) | ei <- eis, let e = getEnt ei ms ]


mkEntDesc :: Id -> Cols -> MudState -> (Id, Ent) -> Text
mkEntDesc i cols ms (ei, e) = let ed = views entDesc (wrapUnlines cols) e in
    case t of ConType      ->                  (ed <>) . mkInvCoinsDesc i cols ms ei $ s
              CorpseType   -> (corpseTxt <>)           . mkInvCoinsDesc i cols ms ei $ s
              NpcType      ->                  (ed <>) . (tempDescHelper <>) . mkEqDesc i cols ms ei s $ t
              PCType       -> (pcHeader  <>) . (ed <>) . (tempDescHelper <>) . mkEqDesc i cols ms ei s $ t
              VesselType   ->                  (ed <>) . mkVesselContDesc  cols ms $ ei
              WritableType ->                  (ed <>) . mkWritableMsgDesc cols ms $ ei
              _            -> ed
  where
    (s, t)              = ((,) <$> uncurry getSing <*> uncurry getType) (ei, ms)
    corpseTxt           = expandCorpseTxt (mkCorpseAppellation i ms ei) . getCorpseDesc ei $ ms
    pcHeader            = wrapUnlines cols mkPCDescHeader
    mkPCDescHeader      = let sexRace = uncurry (|<>|) . mkPrettySexRace ei $ ms
                          in T.concat [ "You see a ", sexRace, rmDescHelper, adminTagHelper, "." ]
    rmDescHelper        = case mkMobRmDesc ei ms of "" -> ""
                                                    d  -> spcL d
    adminTagHelper      | isAdminId ei ms = spcL adminTagTxt
                        | otherwise       = ""
    tempDescHelper      = maybeEmp (wrapUnlines cols . coloredBracketQuote) . getTempDesc ei $ ms
    coloredBracketQuote = quoteWith' (("[ ", " ]") & both %~ colorWith tempDescColor)


mkInvCoinsDesc :: Id -> Cols -> MudState -> Id -> Sing -> Text
mkInvCoinsDesc i cols ms i' s =
    let pair@(is, c)           = (getInv `fanUncurry` getCoins) (i', ms)
        msg | i' == i          = dudeYourHandsAreEmpty
            | t  == CorpseType = "There is nothing on the corpse."
            | otherwise        = "The " <> s <> " is empty."
    in case ((()#) *** (()#)) pair of
      (True,  True ) -> wrapUnlines cols msg
      (False, True ) -> header <> mkEntsInInvDesc i cols ms is                          <> footer
      (True,  False) -> header                                 <> mkCoinsSummary cols c <> footer
      (False, False) -> header <> mkEntsInInvDesc i cols ms is <> mkCoinsSummary cols c <> footer
  where
    t      = getType i' ms
    header = i' == i ? nl "You are carrying:" :? let n = t == CorpseType ? mkCorpseAppellation i ms i' :? s
                                                 in wrapUnlines cols $ "The " <> n <> " contains:"
    footer | i' == i   = nl $ showText (calcEncPer     i  ms) <> "% encumbered."
           | otherwise = nl $ showText (calcConPerFull i' ms) <> "% full."


mkEntsInInvDesc :: Id -> Cols -> MudState -> Inv -> Text
mkEntsInInvDesc i cols ms =
    T.unlines . concatMap (wrapIndent bracketedEntNamePadding cols . helper) . mkStyledName_count_bothList i ms
  where
    helper (padBracketedEntName -> en, c, (s, _)) | c == 1 = en <> "1 " <> s
    helper (padBracketedEntName -> en, c, b     )          = T.concat [ en, showText c, " ", mkPlurFromBoth b ]


mkStyledName_count_bothList :: Id -> MudState -> Inv -> [(Text, Int, BothGramNos)]
mkStyledName_count_bothList i ms is =
    let styleds                       = styleAbbrevs DoQuote [ getEffName        i ms targetId | targetId <- is ]
        boths@(mkCountList -> counts) =                      [ getEffBothGramNos i ms targetId | targetId <- is ]
    in nub . zip3 styleds counts $ boths


mkCoinsSummary :: Cols -> Coins -> Text
mkCoinsSummary cols = helper . zipWith mkNameAmt coinNames . coinsToList
  where
    helper         = T.unlines . wrapIndent 2 cols . commas . dropEmpties
    mkNameAmt cn a = Sum a |!| showText a |<>| bracketQuote (colorWith abbrevColor cn)


mkEqDesc :: Id -> Cols -> MudState -> Id -> Sing -> Type -> Text
mkEqDesc i cols ms descId descSing descType = let descs = bool mkDescsOther mkDescsSelf $ descId == i in
    ()# descs ? noDescs :? ((header <>) . T.unlines . concatMap (wrapIndent 15 cols) $ descs)
  where
    mkDescsSelf =
        let (slotNames,  es ) = unzip [ (pp slot, getEnt ei ms)                  | (slot, ei) <- M.toList . getEqMap i $ ms ]
            (sings,      ens) = unzip [ (view sing &&& views entName fromJust) e | e          <- es                         ]
        in map helper . zip3 slotNames sings . styleAbbrevs DoQuote $ ens
      where
        helper (T.breakOn " finger" -> (slotName, _), s, styled) = T.concat [ parensPad 15 slotName, s, " ", styled ]
    mkDescsOther = map helper [ (pp slot, getSing ei ms) | (slot, ei) <- M.toList . getEqMap descId $ ms ]
      where
        helper (T.breakOn " finger" -> (slotName, _), s) = parensPad 15 slotName <> s
    noDescs = wrapUnlines cols $ if
      | descId   == i      -> dudeYou'reNaked
      | descType == PCType -> parseDesig i ms $ d  <> " doesn't have anything readied."
      | otherwise          -> theOnLowerCap descSing <> " doesn't have anything readied."
    header = wrapUnlines cols $ if
      | descId   == i      -> "You have readied the following equipment:"
      | descType == PCType -> parseDesig i ms $ d  <> " has readied the following equipment:"
      | otherwise          -> theOnLowerCap descSing <> " has readied the following equipment:"
    d = mkSerializedNonStdDesig descId ms descSing The DoCap


mkVesselContDesc :: Cols -> MudState -> Id -> Text
mkVesselContDesc cols ms targetId =
    let s = getSing   targetId ms
        v = getVessel targetId ms
        emptyDesc         = "The " <> s <> " is empty." |&| wrapUnlines cols
        mkContDesc (l, q) = T.concat [ "The "
                                     , s
                                     , " contains "
                                     , renderLiqNoun l aOrAn
                                     , " "
                                     , parensQuote $ showText (calcVesselPerFull v q) <> "% full"
                                     , "." ] |&| wrapUnlines cols
    in views vesselCont (maybe emptyDesc mkContDesc) v


mkWritableMsgDesc :: Cols -> MudState -> Id -> Text
mkWritableMsgDesc cols ms targetId = case getWritable targetId ms of
  (Writable Nothing          _       ) -> ""
  (Writable (Just _        ) (Just _)) -> helper "a language you don't recognize"
  (Writable (Just (_, lang)) Nothing ) -> helper . pp $ lang
  where
    helper txt = wrapUnlines cols . prd $ "There is something written on it in " <> txt


adminTagTxt :: Text
adminTagTxt = colorWith adminTagColor (parensQuote "admin")


-----


mkExitsSummary :: Cols -> Rm -> Text
mkExitsSummary cols (view rmLinks -> rls) =
    let stdNames    = [ rl^.slDir  .to (colorWith exitsColor . linkDirToCmdName) | rl <- rls, not . isNonStdLink $ rl ]
        customNames = [ rl^.nslName.to (colorWith exitsColor                   ) | rl <- rls,       isNonStdLink   rl ]
    in T.unlines . wrapIndent 2 cols . ("Obvious exits: " <>) . summarize stdNames $ customNames
  where
    summarize []  []  = "None!"
    summarize std cus = commas . (std ++) $ cus


isNonStdLink :: RmLink -> Bool
isNonStdLink NonStdLink {} = True
isNonStdLink _             = False


-----


mkFpDesc :: Id -> MudState -> Text
mkFpDesc i ms = let (c, m) = getFps i ms
                in mkDescForPercent9 (c `percent` m) [ colorWith magenta "You are too exhausted to move."
                                                     , colorWith magenta "You are seriously tired."
                                                     , colorWith red     "You are extremely tired."
                                                     , "You are very tired."
                                                     , "You are quite tired."
                                                     , "You are considerably tired."
                                                     , "You are moderately tired."
                                                     , "You are slightly tired."
                                                     , "" ]


mkDescForPercent9 :: Int -> [Text] -> Text
mkDescForPercent9 x = mkDescForPercent x . zip [ 0, 14, 28, 42, 56, 70, 84, 99, 100 ]


mkDescForPercent :: Int -> [(Int, Text)] -> Text
mkDescForPercent _ []                          = blowUp "mkDescForPercent" "empty list" ""
mkDescForPercent _ [(_, txt)]                  = txt
mkDescForPercent x ((y, txt):rest) | x <= y    = txt
                                   | otherwise = mkDescForPercent x rest



-----


mkFullDesc :: Id -> MudState -> Text
mkFullDesc i ms = mkDescForPercent9 (calcStomachPerFull i ms)
    [ colorWith magenta                "You are famished."
    , colorWith red                    "You are extremely hungry."
    ,                                  "You are quite hungry."
    ,                                  "You feel a little hungry."
    ,                                  ""
    ,                                  "You feel satisfied."
    ,                                  "You are quite full."
    , colorWith red                    "You are extremely full."
    , colorWith magenta . thrice prd $ "You are profoundly satiated. You don't feel so good" ]


-----


mkHpDesc :: Id -> MudState -> Text
mkHpDesc i ms =
    let (c, m) = getHps i ms
    in mkDescForPercent (c `percent` m) [ (-15, colorWith magenta "You are dead."                            )
                                        , (0,   colorWith magenta "You are unconscious and mortally wounded.")
                                        , (14,  colorWith magenta "You are critically wounded."              )
                                        , (28,  colorWith red     "You are extremely wounded."               )
                                        , (42,                    "You are badly wounded."                   )
                                        , (56,                    "You are quite wounded."                   )
                                        , (70,                    "You are considerably wounded."            )
                                        , (84,                    "You are moderately wounded."              )
                                        , (99,                    "You are lightly wounded."                 )
                                        , (100,                   ""                                         ) ]


-----


mkLastArgIsTargetBindings :: Id -> MudState -> Args -> LastArgIsTargetBindings
mkLastArgIsTargetBindings i ms as | (lastArg, others) <- mkLastArgWithNubbedOthers as =
    LastArgIsTargetBindings { srcDesig    = mkStdDesig  i ms DoCap
                            , srcInvCoins = getInvCoins i ms
                            , rmInvCoins  = first (i `delete`) . getMobRmVisibleInvCoins i $ ms
                            , targetArg   = lastArg
                            , otherArgs   = others }


mkLastArgWithNubbedOthers :: Args -> (Text, Args)
mkLastArgWithNubbedOthers as = let lastArg = last as
                                   otherArgs = init $ case as of
                                     [_, _] -> as
                                     _      -> (++ pure lastArg) . nub . init $ as
                               in (lastArg, otherArgs)


-----


type IsConInRm  = Bool
type InvWithCon = Inv


mkMaybeNthOfM :: MudState -> IsConInRm -> Id -> Sing -> InvWithCon -> Maybe NthOfM
mkMaybeNthOfM ms icir conId conSing invWithCon = guard icir >> return helper
  where
    helper  = succ . fromJust . elemIndex conId &&& length $ matches
    matches = filter ((== conSing) . flip getSing ms) invWithCon


-----


mkMpDesc :: Id -> MudState -> Text
mkMpDesc i ms = let (c, m) = getMps i ms
                in mkDescForPercent9 (c `percent` m) [ colorWith magenta "Your mana is entirely depleted."
                                                     , colorWith magenta "Your mana is severely depleted."
                                                     , colorWith red     "Your mana is extremely depleted."
                                                     , "Your mana is very depleted."
                                                     , "Your mana is quite depleted."
                                                     , "Your mana is considerably depleted."
                                                     , "Your mana is moderately depleted."
                                                     , "Your mana is slightly depleted."
                                                     , "" ]


-----


mkPpDesc :: Id -> MudState -> Text
mkPpDesc i ms = let (c, m) = getPps i ms
                in mkDescForPercent9 (c `percent` m) [ colorWith magenta "Your psionic energy is entirely depleted."
                                                     , colorWith magenta "Your psionic energy is severely depleted."
                                                     , colorWith red     "Your psionic energy is extremely depleted."
                                                     , "Your psionic energy is very depleted."
                                                     , "Your psionic energy is quite depleted."
                                                     , "Your psionic energy is considerably depleted."
                                                     , "Your psionic energy is moderately depleted."
                                                     , "Your psionic energy is slightly depleted."
                                                     , "" ]


-----


moveReadiedItem :: Id
                -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
                -> Slot
                -> Id
                -> (Text, Broadcast)
                -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
moveReadiedItem i a s targetId (msg, b) = a & _1.ind i.at s ?~ targetId
                                            & _2.ind i %~ (targetId `delete`)
                                            & _3 <>~ pure msg
                                            & _4 <>~ pure b
                                            & _5 <>~ pure msg


-----


notFoundSuggestAsleeps :: Text -> [Sing] -> MudState -> Text
notFoundSuggestAsleeps a@(capitalize . T.toLower -> a') asleepSings ms =
    case findFullNameForAbbrev a' asleepSings of
      Just asleepTarget ->
          let heShe = mkThrPerPro . getSex (getIdForPCSing asleepTarget ms) $ ms
              guess = a' /= asleepTarget |?| ("Perhaps you mean " <> asleepTarget <> "? ")
          in T.concat [ guess
                      , "Unfortunately, "
                      , bool heShe asleepTarget $ ()# guess
                      , thrice prd " is sleeping at the moment" ]
      Nothing -> sorryTwoWayLink a


-----


onOffs :: [(Text, Bool)]
onOffs = [ ("o",   likewise )
         , ("of",  likewise )
         , ("off", likewise )
         , ("on",  otherwise) ]


-----


otherHand :: Hand -> Hand
otherHand RHand  = LHand
otherHand LHand  = RHand
otherHand NoHand = LHand


-----


putOnMsgs :: Id -> Desig -> Sing -> (Text, Broadcast)
putOnMsgs = mkReadyMsgs "put on" "puts on"


-----


resolveMobInvCoins :: Id -> MudState -> Args -> Inv -> Coins -> ([Either Text Inv], [Either [Text] Coins])
resolveMobInvCoins i ms = resolveHelper i ms procGecrMisMobInv procReconciledCoinsMobInv


resolveHelper :: Id
              -> MudState
              -> ((GetEntsCoinsRes, Maybe Inv) -> Either Text Inv)
              -> (ReconciledCoins -> Either [Text] Coins)
              -> Args
              -> Inv
              -> Coins
              -> ([Either Text Inv], [Either [Text] Coins])
resolveHelper i ms f g as is c | (gecrs, miss, rcs) <- resolveEntCoinNames i ms as is c
                               , eiss               <- zipWith (curry f) gecrs miss
                               , ecs                <- map g rcs = (eiss, ecs)


resolveRmInvCoins :: Id -> MudState -> Args -> Inv -> Coins -> ([Either Text Inv], [Either [Text] Coins])
resolveRmInvCoins i ms = resolveHelper i ms procGecrMisRm procReconciledCoinsRm


-----


sorryConHelper :: Id -> MudState -> Id -> Sing -> Text
sorryConHelper i ms conId conSing
  | isNpcPC conId ms = sorryCon . parseDesig i ms . serialize . mkStdDesig conId ms $ Don'tCap
  | otherwise        = sorryCon conSing


-----


spiritHelper :: Id -> (MudState -> MudStack ()) -> (MudState -> MudStack ()) -> MudStack ()
spiritHelper i a b = getState >>= \ms -> ms |&| bool a b (isSpiritId i ms)
