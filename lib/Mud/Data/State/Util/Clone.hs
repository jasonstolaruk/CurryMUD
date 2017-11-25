{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}

module Mud.Data.State.Util.Clone where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Make
import           Mud.Data.State.Util.Misc
import           Mud.TheWorld.Zones.AdminZoneIds (iClone)
import           Mud.Threads.NpcServer
import           Mud.Util.Misc

import           Control.Lens (_1, _2, _3, view, views)
import           Control.Lens.Operators ((.~), (&), (%~), (^.), (<>~))
import           Data.List ((\\), foldl')
import           GHC.Stack (HasCallStack)
import           Prelude hiding (exp)
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
                             = (ObjTemplate <$> view objWeight
                                            <*> view objVol
                                            <*> view objTaste
                                            <*> view objVal
                                            <*> view objWear
                                            <*> view objFlags) o
            mkVesselTemplate | v <- getVessel targetId ms
                             = VesselTemplate { vtCont = v^.vesselCont
                                              , vtHoly = views vesselIsHoly (`boolToMaybe` getHolySymbol targetId ms) v }
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
                            (newId, ms', fs)   = newCon ms mkEntTemplate mkObjTemplate con mempties mc destId
                        in g newId coins . clone newId ([], ms', fs) $ is
          CorpseType     -> p -- You can't clone a corpse (you would need to know how many seconds to start the
                              -- decomposer at).
          FoodType       -> f . newFood       ms mkEntTemplate mkObjTemplate (getFood       targetId ms) $ destId
          HolySymbolType -> f . newHolySymbol ms mkEntTemplate mkObjTemplate (getHolySymbol targetId ms) $ destId
          LightType      | light <- getLight targetId ms & lightIsLit .~ False
                         -> f . newLight      ms mkEntTemplate mkObjTemplate light                       $ destId
          NpcType        ->
              let ((is, coins), em) = (getInvCoins `fanUncurry` getEqMap) (targetId, ms)
                  (newId, ms', fs)  = newNpc ms
                                             mkEntTemplate
                                             mempties
                                             M.empty
                                             mkMobTemplate
                                             runNpcServerAsync
                                             destId
              in h newId coins . cloneEqMap em . clone newId ([], ms', fs) $ is
          ObjType        -> f . newObj ms mkEntTemplate mkObjTemplate $ destId
          PlaType        -> p -- You can't clone a player.
          RmType         -> p -- You can't clone a room.
          VesselType     -> f . newVessel   ms mkEntTemplate mkObjTemplate mkVesselTemplate          $ destId
          WpnType        -> f . newWpn      ms mkEntTemplate mkObjTemplate (getWpn      targetId ms) $ destId
          WritableType   -> f . newWritable ms mkEntTemplate mkObjTemplate (getWritable targetId ms) $ destId


cloneEqMap :: EqMap -> (Inv, MudState, Funs) -> ((Inv, EqMap), MudState, Funs)
cloneEqMap em (is, ms, fs) = let (is', ms', fs') = clone iClone ([], ms, fs) . M.elems $ em
                                 em'             = M.fromList . zip (M.keys em) $ is'
                             in ((is, em'), ms' & invTbl.ind iClone %~ (\\ is'), fs')
