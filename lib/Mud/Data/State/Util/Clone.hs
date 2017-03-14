{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Clone where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Make
import Mud.Data.State.Util.Misc
import Mud.TheWorld.Zones.AdminZoneIds (iLoggedOut, iWelcome)
import Mud.Threads.NpcServer
import Mud.Util.Misc
import Mud.Util.Text

import Control.Lens (_1, _2, _3, view, views)
import Control.Lens.Operators ((.~), (&), (%~), (^.), (<>~))
import Data.List ((\\), find, foldl')
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import GHC.Stack (HasCallStack)
import Prelude hiding (exp)
import qualified Data.IntMap.Strict as IM (keys)
import qualified Data.Map.Strict as M (elems, empty, fromList, keys)


default (Int)


-- ==================================================


clone :: HasCallStack => Id -> (Inv, MudState, Funs) -> Inv -> (Inv, MudState, Funs)
clone destId = foldl' helper
  where
    helper p@(_, ms, _) targetId =
        let mkEntTemplate    | e <- getEnt targetId ms
                             = (EntTemplate <$> view entName
                                            <*> view sing
                                            <*> view plur
                                            <*> view entDesc
                                            <*> view entSmell
                                            <*> view entFlags) e
            mkMobTemplate    | m <- getMob targetId ms
                             = MobTemplate { mtSex              = m^.sex
                                           , mtSt               = m^.st
                                           , mtDx               = m^.dx
                                           , mtHt               = m^.ht
                                           , mtMa               = m^.ma
                                           , mtPs               = m^.ps
                                           , mtMaxHp            = m^.maxHp
                                           , mtMaxMp            = m^.maxMp
                                           , mtMaxPp            = m^.maxPp
                                           , mtMaxFp            = m^.maxFp
                                           , mtExp              = m^.exp
                                           , mtLvl              = m^.lvl
                                           , mtHand             = m^.hand
                                           , mtKnownLangs       = m^.knownLangs
                                           , mtRmId             = m^.rmId
                                           , mtSize             = m^.mobSize
                                           , mtCorpseWeight     = m^.corpseWeight
                                           , mtCorpseVol        = m^.corpseVol
                                           , mtCorpseCapacity   = m^.corpseCapacity
                                           , mtCorpseDecompSecs = m^.corpseDecompSecs
                                           , mtParty            = m^.party }
            mkObjTemplate    | o <- getObj targetId ms
                             = (ObjTemplate <$> view objWeight <*> view objVol <*> view objTaste <*> view objFlags) o
            mkPlaTemplate    | pla <- getPla targetId ms
                             = (PlaTemplate <$> view plaFlags
                                            <*> view retainedMsgs
                                            <*> views logoutRmId (fromMaybe iWelcome)) pla
            mkVesselTemplate | v <- getVessel targetId ms
                             = views vesselCont VesselTemplate v
            f             (newId,    ms', fs) = p & _1 <>~ pure newId
                                                  & _2 .~  ms'
                                                  & _3 <>~ fs
            g newId coins (is,       ms', fs) = p & _1 <>~ pure newId
                                                  & _2 .~  upd ms' [ invTbl  .ind newId .~ sortInv ms' is
                                                                   , coinsTbl.ind newId .~ coins ]
                                                  & _3 <>~ fs
            h newId coins ((is, em), ms', fs) = p & _1 <>~ pure newId
                                                  & _2 .~  upd ms' [ invTbl  .ind newId .~ sortInv ms' is
                                                                   , coinsTbl.ind newId .~ coins
                                                                   , eqTbl   .ind newId .~ em ]
                                                  & _3 <>~ fs
        in case getType targetId ms of
          ArmType    -> f . newArm   ms mkEntTemplate mkObjTemplate (getArm   targetId ms) $ destId
          ClothType  -> f . newCloth ms mkEntTemplate mkObjTemplate (getCloth targetId ms) $ destId
          ConType    -> let (con, (is, coins)) = (getCon `fanUncurry` getInvCoins) (targetId, ms)
                            mc                 = views conIsCloth (`boolToMaybe` getCloth targetId ms) con
                            (newId, ms', fs)   = newCon ms mkEntTemplate mkObjTemplate con (mempty, mempty) mc destId
                        in g newId coins . clone newId ([], ms', fs) $ is
          CorpseType     -> p -- You can't clone a corpse. You would need to know how many seconds to start the
                              -- decomposer at.
          FoodType       -> f . newFood       ms mkEntTemplate mkObjTemplate (getFood       targetId ms) $ destId
          HolySymbolType -> f . newHolySymbol ms mkEntTemplate mkObjTemplate (getHolySymbol targetId ms) $ destId
          NpcType        ->
              let ((is, coins), em) = (getInvCoins `fanUncurry` getEqMap) (targetId, ms)
                  (newId, ms', fs)  = newNpc ms mkEntTemplate (mempty, mempty) M.empty mkMobTemplate runNpcServerAsync destId
              in h newId coins . cloneEqMap em . clone newId ([], ms', fs) $ is
          ObjType        -> f . newObj ms mkEntTemplate mkObjTemplate $ destId
          PCType         ->
            let (pc, (is, coins), em, r, t) = ((,,,,) <$> uncurry getPC
                                                      <*> uncurry getInvCoins
                                                      <*> uncurry getEqMap
                                                      <*> uncurry getRndmNamesTbl
                                                      <*> uncurry getTeleLinkTbl) (targetId, ms)
                s            = etSing mkEntTemplate
                s'           = mkNewSing 1
                mkNewSing x  = let newSing = s <> showText x
                               in if views plaTbl (isNothing . find ((== newSing) . (`getSing` ms)) . IM.keys) ms
                                 then newSing
                                 else mkNewSing . succ $ x
                (newId, ms') = newPla ms
                                      mkEntTemplate { etSing = s' }
                                      (mempty, mempty)
                                      M.empty
                                      mkMobTemplate
                                      pc
                                      mkPlaTemplate
                                      r
                                      t
                                      iLoggedOut
            in h newId coins . cloneEqMap em . clone newId ([], ms', []) $ is
          RmType         -> p -- You can't clone a room.
          VesselType     -> f . newVessel   ms mkEntTemplate mkObjTemplate mkVesselTemplate          $ destId
          WpnType        -> f . newWpn      ms mkEntTemplate mkObjTemplate (getWpn      targetId ms) $ destId
          WritableType   -> f . newWritable ms mkEntTemplate mkObjTemplate (getWritable targetId ms) $ destId


cloneEqMap :: EqMap -> (Inv, MudState, Funs) -> ((Inv, EqMap), MudState, Funs)
cloneEqMap em (is, ms, fs) = let (is', ms', fs') = clone iWelcome ([], ms, fs) . M.elems $ em
                                 em'             = M.fromList . zip (M.keys em) $ is'
                             in ((is, em'), ms' & invTbl.ind iWelcome %~ (\\ is'), fs')
