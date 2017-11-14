{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE FlexibleContexts, LambdaCase, MultiWayIf, NamedFieldPuns, OverloadedStrings, TransformListComp, TupleSections, ViewPatterns #-}

-- This module contains state-related functions used by multiple modules.

module Mud.Data.State.Util.Misc ( addToInv
                                , descMaybeId
                                , descMaybeSingId
                                , descSingId
                                , dropPrefixes
                                , dropPrefixesForHooks
                                , expandCorpseTxt
                                , findBiodegradableIds
                                , findInvContaining
                                , findMobIds
                                , findNpcIds
                                , getAdminIds
                                , getBothGramNos
                                , getCorpseDesc
                                , getEffBothGramNos
                                , getEffName
                                , getFeelingFun
                                , getFun
                                , getHookFun
                                , getInstaEffectFun
                                , getListenThreadId
                                , getLocation
                                , getLogAsyncs
                                , getLogThreadIds
                                , getLoggedInAdminIds
                                , getLoggedInPlaIds
                                , getMobRmVisibleInvCoins
                                , getNonIncogLoggedInAdminIds
                                , getNpcIds
                                , getRmActionFun
                                , getServerSettings
                                , getState
                                , getUnusedId
                                , getVisibleInv
                                , getVisibleInvCoins
                                , isAdHoc
                                , isAwake
                                , isDead
                                , isKnownLang
                                , isLitLight
                                , isLoggedIn
                                , isMobRmLit
                                , isPCCorpse
                                , isRmLit
                                , leaveParty
                                , linkDirToCmdName
                                , lookupHooks
                                , mkAdminIdSingList
                                , mkAdminPlaIdSingList
                                , mkCorpseAppellation
                                , mkCorpseTxt
                                , mkEntName
                                , mkMaybeCorpseId
                                , mkNameCountBothList
                                , mkName_maybeCorpseId_count_bothList
                                , mkPlaIdSingList
                                , mkPrettySexRace
                                , mkPrettySexRaceLvl
                                , mkSerializedNonStdDesig
                                , mkStdDesig
                                , mkUnknownPCEntName
                                , modifyState
                                , modifyStateSeq
                                , plurRace
                                , procHooks
                                , procQuoteChars
                                , removeAdHoc
                                , setInterp
                                , sortInv
                                , tweak
                                , tweaks
                                , upd ) where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Hierarchy
import           Mud.Misc.CurryTime
import           Mud.Misc.Misc
import           Mud.TheWorld.Zones.AdminZoneIds (iNecropolis, iWelcome)
import           Mud.TopLvlDefs.Chars
import           Mud.TopLvlDefs.Misc
import           Mud.Util.List hiding (countOcc)
import qualified Mud.Util.Misc as U (blowUp, pmf)
import           Mud.Util.Misc hiding (blowUp, pmf)
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Arrow ((***), (&&&), first)
import           Control.Concurrent (ThreadId)
import           Control.Concurrent.Async (asyncThreadId)
import           Control.Lens (_1, _2, at, both, to, view, views)
import           Control.Lens.Operators ((.~), (&), (%~), (^.))
import           Control.Monad ((>=>), mplus)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import           Data.Bool (bool)
import           Data.IORef (atomicModifyIORef', readIORef)
import           Data.List ((\\), delete, foldl', nub, nubBy, sortBy, zip4)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Sum(..), (<>))
import           Data.Text (Text)
import           GHC.Exts (sortWith)
import           GHC.Stack (HasCallStack)
import qualified Data.IntMap.Strict as IM ((!), filter, keys)
import qualified Data.Map.Strict as M (elems, filterWithKey, lookup)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V (Vector)
import           Text.Regex.PCRE ((=~))


blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Data.State.Util.Misc"


pmf :: PatternMatchFail
pmf = U.pmf "Mud.Data.State.Util.Misc"


-- ==================================================


addToInv :: HasCallStack => MudState -> Inv -> Inv -> Inv
addToInv ms addThese toThese = sortInv ms $ toThese ++ addThese


-----


descSingId :: HasCallStack => Id -> MudState -> Text
descSingId i ms = quoteWith' (i |&| ((`getSing` ms) &&& parensQuote . showTxt)) " "


descMaybeId :: HasCallStack => MudState -> Maybe Id -> Text
descMaybeId ms = maybe none (`descSingId` ms)


descMaybeSingId :: HasCallStack => Maybe Id -> MudState -> Text
descMaybeSingId Nothing  _  = none
descMaybeSingId (Just x) ms = descSingId x ms


-----


expandCorpseTxt :: Text -> Text -> Text
expandCorpseTxt = T.replace (T.singleton corpseNameMarker)


-----


findBiodegradableIds :: MudState -> Inv
findBiodegradableIds = views objTbl (IM.keys . IM.filter isBiodegradable)


-----


findInvContaining :: HasCallStack => Id -> MudState -> Maybe Id
findInvContaining i ms = let matches = views invTbl (IM.keys . IM.filter (i `elem`)) ms
                         in ()# matches ? Nothing :? Just (head matches)


-----


findMobIds :: HasCallStack => MudState -> Inv -> Inv
findMobIds ms haystack = [ i | i <- haystack, (||) <$> (PlaType ==) <*> (NpcType ==) $ getType i ms ]


-----


findNpcIds :: MudState -> Inv
findNpcIds = views typeTbl (IM.keys . IM.filter (== NpcType))


-----


getAdminIds :: HasCallStack => MudState -> Inv
getAdminIds = getAdminIdsHelper (const True)


getAdminIdsHelper :: HasCallStack => (Pla -> Bool) -> MudState -> Inv
getAdminIdsHelper f = views plaTbl (IM.keys . IM.filter ((&&) <$> isAdmin <*> f))


-----


getBothGramNos :: HasCallStack => Id -> MudState -> BothGramNos
getBothGramNos i = (sing `fanView` plur) . getEnt i


getEffBothGramNos :: HasCallStack => Id -> MudState -> Id -> BothGramNos
getEffBothGramNos i ms targetId =
    let targetEnt  = getEnt targetId ms
        targetSing = targetEnt^.sing
        pair       = (targetSing, targetEnt^.plur)
    in case targetEnt^.entName of
      Nothing -> let (pp -> targetSexy, targetRace) = getSexRace targetId ms
                 in if targetSing `elem` getIntroduced i ms
                   then (targetSing,    ""                 )
                   else (pp targetRace, plurRace targetRace) & both %~ ((targetSexy <>) . spcL)
      Just {} | getType targetId ms == CorpseType
              , isPCCorpse . getCorpse targetId $ ms -> pair & _1 .~ mkCorpseAppellation i ms targetId
              | otherwise                            -> pair


plurRace :: Race -> Text
plurRace Dwarf = "dwarves"
plurRace Elf   = "elves"
plurRace r     = pp r <> "s"


-----


getCorpseDesc :: HasCallStack => Id -> MudState -> Text
getCorpseDesc i ms = let c    = getCorpse i ms
                         lens = bool npcCorpseDesc pcCorpseDesc . isPCCorpse $ c
                     in c^.lens


-----


getEffName :: HasCallStack => Id -> MudState -> Id -> Text
getEffName i ms targetId = let targetEnt = getEnt targetId ms
                           in views entName (fromMaybe (views sing helper targetEnt)) targetEnt
  where
    helper targetSing
      | isNpc i ms || views (pcTbl.ind i.introduced) (targetSing `notElem`) ms = mkUnknownPCEntName targetId ms
      | otherwise                                                              = uncapitalize targetSing


mkUnknownPCEntName :: HasCallStack => Id -> MudState -> Text
mkUnknownPCEntName i ms = views entName (fromMaybe helper) . getEnt i $ ms
  where
    helper = uncurry T.cons . first T.head . mkPrettySexRace i $ ms


-----


getFeelingFun :: HasCallStack => FeelingTag -> MudState -> FeelingFun
getFeelingFun tag = views (feelingFunTbl.at tag) (fromMaybe oops)
  where
    oops = blowUp "getFeelingFun" "feeling tag not found in function table" tag


-----


getFun :: HasCallStack => FunName -> MudState -> Fun
getFun n = views (funTbl.at n) (fromMaybe oops)
  where
    oops = blowUp "getFun" "function name not found in function table" n


-----


getHookFun :: HasCallStack => HookName -> MudState -> HookFun
getHookFun n = views (hookFunTbl.at n) (fromMaybe oops)
  where
    oops = blowUp "getHookFun" "hook name not found in hook function table" n


-----


getInstaEffectFun :: HasCallStack => FunName -> MudState -> InstaEffectFun
getInstaEffectFun n = views (instaEffectFunTbl.at n) (fromMaybe oops)
  where
    oops = blowUp "getInstaEffectFun" "function name not found in instantaneous effect function table" n


-----


getListenThreadId :: HasCallStack => MudState -> Maybe ThreadId
getListenThreadId = lookupMapValue Listen . view threadTbl


-----


getLocation :: HasCallStack => Id -> MudState -> Id
getLocation i ms = fromMaybe oops $ searchInvs `mplus` searchEqs
  where
    searchInvs = views invTbl (listToMaybe . IM.keys . IM.filter ( i `elem`)           ) ms
    searchEqs  = views eqTbl  (listToMaybe . IM.keys . IM.filter ((i `elem`) . M.elems)) ms
    oops       = blowUp "getLocation" "ID is in limbo" . showTxt $ i


-----


getLogAsyncs :: HasCallStack => MudData -> Maybe (LogAsync, LogAsync)
getLogAsyncs = helper . (noticeLog `fanView` errorLog)
  where
    helper = \case (Just (noticeAsync, _), Just (errorAsync, _)) -> Just (noticeAsync, errorAsync)
                   _                                             -> Nothing


-----


getLogThreadIds :: HasCallStack => MudStack [ThreadId]
getLogThreadIds = asks getLogAsyncs >>= \case Nothing     -> mMempty
                                              Just (a, b) -> return . map asyncThreadId $ [ a, b ]


-----


getLoggedInAdminIds :: HasCallStack => MudState -> Inv
getLoggedInAdminIds = getAdminIdsHelper isLoggedIn


getLoggedInPlaIds :: HasCallStack => MudState ->  Inv
getLoggedInPlaIds = views plaTbl (IM.keys . IM.filter ((&&) <$> isLoggedIn <*> not . isAdmin))


-----


getMobRmVisibleInvCoins :: HasCallStack => Id -> MudState -> (Inv, Coins)
getMobRmVisibleInvCoins i ms = let ri = getRmId i ms in getVisibleInvCoins ri ms


-----


getNonIncogLoggedInAdminIds :: HasCallStack => MudState -> Inv
getNonIncogLoggedInAdminIds ms = let adminIds = getLoggedInAdminIds ms
                                 in [ adminId | adminId <- adminIds, not . isIncognitoId adminId $ ms ]


-----


getNpcIds :: MudState -> Inv
getNpcIds = views npcTbl IM.keys


-----


getRmActionFun :: HasCallStack => FunName -> MudState -> RmActionFun
getRmActionFun n = views (rmActionFunTbl.at n) (fromMaybe oops)
  where
    oops = blowUp "getRmActionFun" "function name not found in room action function table" n


-----


getServerSettings :: HasCallStack => MudStack ServerSettings
getServerSettings = asks (view serverSettings)


-----


getState :: HasCallStack => MudStack MudState
getState = do ior      <- asks . view $ mudStateIORef
              (ms, ct) <- liftIO $ (,) <$> readIORef ior <*> getCurryTime
              return (ms & curryTime .~ ct)


-----


getUnusedId :: HasCallStack => MudState -> Id
getUnusedId = views typeTbl (head . (enumFrom 0 \\) . IM.keys)


-----


getVisibleInv :: HasCallStack => Id -> MudState -> Inv
getVisibleInv i ms = filter isVisible . getInv i $ ms
  where
    isVisible targetId | not . isPla targetId $ ms         = otherwise
                       | isSpiritId targetId ms            = likewise
                       | not . isIncognitoId targetId $ ms = otherwise
                       | otherwise                         = likewise


-----


getVisibleInvCoins :: HasCallStack => Id -> MudState -> (Inv, Coins)
getVisibleInvCoins i = getVisibleInv i &&& getCoins i


-----


isAdHoc :: HasCallStack => Id -> MudState -> Bool
isAdHoc i = (== iWelcome) . getRmId i


-----


isAwake :: HasCallStack => Id -> MudState -> Bool
isAwake = onPla ((&&) <$> isLoggedIn <*> not . isIncognito) True


isLoggedIn :: HasCallStack => Pla -> Bool
isLoggedIn = views logoutRmId ((()#) . (Sum <$>))


-----


isDead :: HasCallStack => Id -> MudState -> Bool
isDead i = (== iNecropolis) . getRmId i


-----


isKnownLang :: HasCallStack => Id -> MudState -> Lang -> Bool
isKnownLang i ms lang | lang == CommonLang = True
                      | otherwise          = lang `elem` getKnownLangs i ms


-----


isLitLight :: Id -> MudState -> Bool
isLitLight i = ((&&) <$> ((== LightType) . uncurry getType) <*> uncurry getLightIsLit) . (i, )


-----


isPCCorpse :: Corpse -> Bool
isPCCorpse PCCorpse  {} = True
isPCCorpse NpcCorpse {} = False


-----


isRmLit :: HasCallStack => Id -> MudState -> Bool
isRmLit i ms = let env    = view rmEnv . getRm i $ ms
                   mobIds = filter ((&&) <$> (`hasMobId` ms) <*> f) . getInv i $ ms
                   f i'   = let g k v = k `elem` [ RHandS, LHandS ] && isLitLight v ms
                            in (()!#) . M.filterWithKey g . getEqMap i' $ ms
                   b      = ()!# mobIds
               in case env of InsideUnlitEnv                                    -> b
                              OutsideEnv | ms^.curryTime.to (isDay . curryHour) -> True
                                         | otherwise                            -> b
                              _                                                 -> True


isMobRmLit :: HasCallStack => Id -> MudState -> Bool
isMobRmLit i ms = isRmLit (getRmId i ms) ms


-----


leaveParty :: HasCallStack => Id -> MudState -> MudState
leaveParty i ms = let helper p = memberOfHelper . myGroupHelper . followersHelper . followingHelper $ ms
                        where
                          followingHelper ms' = views following (maybe ms' f) p
                            where
                              f followingId = upd ms' [ mobTbl.ind i          .party.following .~ Nothing
                                                      , mobTbl.ind followingId.party.followers %~ delete i ]
                          followersHelper ms' = (mobTbl.ind i.party.followers .~ []) . foldr f ms' $ p^.followers
                            where
                              f followerId = mobTbl.ind followerId.party.following .~ Nothing
                          myGroupHelper      = mobTbl.ind i.party.myGroup .~ []
                          memberOfHelper ms' = views memberOf (maybe ms' f) p
                            where
                              f memberOfId = upd ms' [ mobTbl.ind i         .party.memberOf .~ Nothing
                                                     , mobTbl.ind memberOfId.party.myGroup  %~ delete i ]
                  in helper . getParty i $ ms


-----


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


-----


lookupHooks :: HasCallStack => Id -> MudState -> CmdName -> Maybe [Hook]
lookupHooks i ms cn = views rmHookMap (M.lookup cn) . getMobRm i $ ms


-----


mkAdminIdSingList :: HasCallStack => MudState -> [(Id, Sing)]
mkAdminIdSingList = mkIdSingListHelper id


mkIdSingListHelper :: HasCallStack => (Bool -> Bool) -> MudState -> [(Id, Sing)]
mkIdSingListHelper f ms@(view plaTbl -> pt) = [ (i, s) | i <- IM.keys pt
                                                       , f . isAdmin $ pt IM.! i
                                                       , let s = getSing i ms
                                                       , then sortWith by s ]


-----


mkAdminPlaIdSingList :: HasCallStack => MudState -> [(Id, Sing)]
mkAdminPlaIdSingList = mkIdSingListHelper (const True)


-----


mkCorpseAppellation :: HasCallStack => Id -> MudState -> Id -> Text
mkCorpseAppellation i ms ci
  | isPCCorpse c, ((||) <$> (== getSing i ms) <*> (`elem` getIntroduced i ms)) cs = "corpse of " <> cs
  | otherwise = s
  where
    (c, s) = (getCorpse `fanUncurry` getSing) (ci, ms)
    cs     = c^.pcCorpseSing


-----


mkCorpseTxt :: (Text, Text) -> Text
mkCorpseTxt = uncurry (middle (<>) . T.singleton $ corpseNameMarker)


-----


mkEntName :: Ent -> Text
mkEntName = views entName (fromMaybe "unknown")


-----


mkNameCountBothList :: HasCallStack => Id -> MudState -> Inv -> [(Text, Int, BothGramNos)]
mkNameCountBothList i ms targetIds = let ens   = [ getEffName        i ms targetId | targetId <- targetIds ]
                                         cs    = mkCountList ebgns
                                         ebgns = [ getEffBothGramNos i ms targetId | targetId <- targetIds ]
                                     in nub . zip3 ens cs $ ebgns


mkName_maybeCorpseId_count_bothList :: HasCallStack => Id -> MudState -> Inv -> [(Text, Maybe Id, Int, BothGramNos)]
mkName_maybeCorpseId_count_bothList i ms targetIds =
    let ens   = [ getEffName i ms targetId        | targetId <- targetIds ]
        mcis  = [ mkMaybeCorpseId targetId ms     | targetId <- targetIds ]
        cs    = mkCountList ebgns
        ebgns = [ getEffBothGramNos i ms targetId | targetId <- targetIds ]
    in nubBy f . zip4 ens mcis cs $ ebgns
  where
    (a, _, c, d) `f` (a', _, c', d') = (a, c, d) == (a', c', d')


mkMaybeCorpseId :: HasCallStack => Id -> MudState -> Maybe Id
mkMaybeCorpseId i ms | getType i ms == CorpseType = case getCorpse i ms of PCCorpse  {} -> Just i
                                                                           NpcCorpse {} -> Nothing
                     | otherwise                  = Nothing


-----


mkPlaIdSingList :: HasCallStack => MudState -> [(Id, Sing)]
mkPlaIdSingList = mkIdSingListHelper not


-----


mkPrettySexRace :: HasCallStack => Id -> MudState -> (Text, Text)
mkPrettySexRace i = (pp *** pp) . getSexRace i


mkPrettySexRaceLvl :: HasCallStack => Id -> MudState -> (Text, Text, Text)
mkPrettySexRaceLvl i ms = let ((s, r), l) = (mkPrettySexRace `fanUncurry` getLvl) (i, ms)
                          in (s, r, showTxt l)


-----


mkSerializedNonStdDesig :: HasCallStack => Id -> MudState -> Sing -> AOrThe -> DoOrDon'tCap -> Text
mkSerializedNonStdDesig i ms s aot cap = serialize NonStdDesig { dEntSing = s, dDesc = helper, dCap = cap }
  where
    helper | isPla i ms = g . uncurry (|<>|) . mkPrettySexRace i $ ms
           | otherwise  = onFalse (isCapital s) g s
    g                   = mkCapsFun cap . (pp aot <>) . spcL


-----


mkStdDesig :: HasCallStack => Id -> MudState -> DoOrDon'tCap -> Desig
mkStdDesig i ms cap = StdDesig { desigDoExpandSing = True
                               , desigEntName      = views entName (fromMaybe (mkUnknownPCEntName i ms)) . getEnt i $ ms
                               , desigCap          = cap
                               , desigId           = i
                               , desigIds          = findMobIds ms . getMobRmInv i $ ms }


-----


modifyState :: HasCallStack => (MudState -> (MudState, a)) -> MudStack a
modifyState f = ask >>= \md -> liftIO .  atomicModifyIORef' (md^.mudStateIORef) $ f


modifyStateSeq :: HasCallStack => (MudState -> (MudState, Funs)) -> MudStack ()
modifyStateSeq = modifyState >=> sequence_


-----


procHooks :: HasCallStack => Id -> MudState -> V.Vector Int -> CmdName -> Args -> HookFunRes
procHooks i ms v cn as | initAcc <- (as, (ms, [], [], []), []) = case lookupHooks i ms cn of
  Nothing    -> initAcc
  Just hooks -> case as of
    -- Process hooks that match on a particular argument. These hooks need to see the other argument(s) passed as well,
    -- not just the argument that matches the hook's trigger.
    [arg] | delim <- T.singleton hookArgDelimiter
          , delim `T.isInfixOf` arg
          , (dropPrefixes -> a, T.tail -> rest) <- T.breakOn delim arg
          -> case filter (\Hook { hookTriggers } -> a `elem` hookTriggers) hooks of
               []  -> initAcc
               [h] -> getHookFun (hookName h) ms i h v (initAcc & _1 .~ pure rest)
               xs  -> pmf "procHooks" xs
    -- Process hooks whose triggers match on any single argument.
    _ -> let helper acc arg = case filter (\Hook { hookTriggers } -> arg `elem` hookTriggers) hooks of
               []        -> acc
               (match:_) -> acc ++ pure match
             as' = dropPrefixesForHooks hooks as
         in case foldl' helper [] as' of
           []      -> initAcc
           matches ->
             let xformedArgs    = foldr (\Hook { hookTriggers } -> dropSynonyms hookTriggers) as' matches
                 hookHelper a h = getHookFun (hookName h) (a^._2._1) i h v a
             in foldl' hookHelper (initAcc & _1 .~ xformedArgs) . nub $ matches


dropPrefixesForHooks :: HasCallStack => [Hook] -> Args -> Args
dropPrefixesForHooks hs = let helper _     []     = []
                              helper trigs (a:as) | a' <- dropPrefixes a, a' `elem` trigs = a' : rest
                                                  | otherwise                             = a  : rest
                                where
                                  rest = helper trigs as
                          in helper (concatMap hookTriggers hs)


dropPrefixes :: HasCallStack => Text -> Text
dropPrefixes     (T.uncons -> Just (x, xs)) | x == allChar, ()!# xs = xs
dropPrefixes arg@(T.unpack -> arg'        )
  | triple@(_, _, c) <- arg' =~ mkRegex indexChar,  isMatch triple = T.pack c
  | triple@(_, _, c) <- arg' =~ mkRegex amountChar, isMatch triple = T.pack c
  | otherwise                                                      = arg
  where
    isMatch :: (String, String, String) -> Bool
    isMatch (a, b, c) = and [ ()# a, ()!# b, ()!# c ]
    mkRegex c         = "^[0-9]+\\" ++ pure c :: String


-----


procQuoteChars :: HasCallStack => Args -> Maybe Args
procQuoteChars []                                                = Just []
procQuoteChars as@(T.unwords -> txt) | not $ q `T.isInfixOf` txt = Just as
                                     | odd . countOcc q $ txt    = Nothing
                                     | otherwise                 = Just [ fillerToSpcs w | w <- T.words . helper $ txt ]
  where
    q                             = T.singleton quoteChar
    helper ""                     = ""
    helper t  | q `T.isInfixOf` t = let (left,   T.tail -> rest ) = T.breakOn q t
                                        (quoted, T.tail -> right) = T.breakOn q rest
                                    in left <> spcsToFiller quoted <> helper right
              | otherwise         = t


-----


removeAdHoc :: HasCallStack => Id -> MudState -> MudState
removeAdHoc i = flip upd [ coinsTbl           .at  i        .~ Nothing
                         , durationalEffectTbl.at  i        .~ Nothing
                         , entTbl             .at  i        .~ Nothing
                         , eqTbl              .at  i        .~ Nothing
                         , invTbl             .at  i        .~ Nothing
                         , invTbl             .ind iWelcome %~ (i `delete`)
                         , mobTbl             .at  i        .~ Nothing
                         , msgQueueTbl        .at  i        .~ Nothing
                         , pausedEffectTbl    .at  i        .~ Nothing
                         , pcTbl              .at  i        .~ Nothing
                         , plaTbl             .at  i        .~ Nothing
                         , rndmNamesMstrTbl   .at  i        .~ Nothing
                         , teleLinkMstrTbl    .at  i        .~ Nothing
                         , typeTbl            .at  i        .~ Nothing ]


-----


setInterp :: HasCallStack => Id -> Maybe Interp -> MudStack ()
setInterp i mi = tweak $ mobTbl.ind i.interp .~ mi


-----


sortInv :: HasCallStack => MudState -> Inv -> Inv
sortInv ms is = let (foldr helper mempties -> (pcs, others)) = [ (i, getType i ms) | i <- is ]
                in (pcs ++) . sortOthers $ others
  where
    helper (i, t) acc                  = let consTo lens = acc & lens %~ (i :)
                                         in t == PlaType ? consTo _1 :? consTo _2
    sortOthers                         = select _1 . sortBy nameThenSing . zipped
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped others                      = [ (i, mkEntName e, e^.sing) | i <- others, let e = getEnt i ms ]


-----


tweak :: HasCallStack => (MudState -> MudState) -> MudStack ()
tweak f = modifyState $ (, ()) . f


tweaks :: HasCallStack => [MudState -> MudState] -> MudStack ()
tweaks fs = tweak $ \ms -> foldl' (&) ms fs


-----


upd :: MudState -> [MudState -> MudState] -> MudState
upd = foldl' (|&|)
