{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Clone where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Make
import Mud.Data.State.Util.Misc
import Mud.Util.Misc

import Control.Lens (_1, _2, _3)
import Control.Lens.Operators ((.~), (&), (^.), (<>~))
import Data.List (foldl')
import GHC.Stack (HasCallStack)


clone :: HasCallStack => Id -> (MudState, Funs, Inv) -> Inv -> (MudState, Funs, Inv)
clone destId = foldl' helper
  where
    helper p@(ms, _, _) targetId =
        let mkEntTemplate  | e <- getEnt targetId ms
                           = EntTemplate (e^.entName) (e^.sing) (e^.plur) (e^.entDesc) (e^.entSmell) (e^.entFlags)
            mkObjTemplate  | o <- getObj targetId ms
                           = ObjTemplate (o^.objWeight) (o^.objVol) (o^.objTaste) (o^.objFlags)
            mkConTemtplate | c <- getCon targetId ms
                           = ConTemplate (c^.conCapacity) (c^.conFlags)
        in case getType targetId ms of
          ArmType        -> let a                = getArm targetId ms
                                (newId, ms', fs) = newArm ms mkEntTemplate mkObjTemplate a destId
                            in p & _1 .~  ms'
                                 & _2 <>~ fs
                                 & _3 <>~ pure newId
          ClothType      -> let c                = getCloth targetId ms
                                (newId, ms', fs) = newCloth ms mkEntTemplate mkObjTemplate c destId
                            in p & _1 .~  ms'
                                 & _2 <>~ fs
                                 & _3 <>~ pure newId
          ConType        -> let (is, coins)       = getInvCoins targetId ms
                                ic                = (mempty, mempty)
                                (newId, ms', fs ) = newCon ms mkEntTemplate mkObjTemplate mkConTemtplate ic destId
                                (ms'',  fs', is') = clone newId (ms', fs, []) is
                            in p & _1 .~  upd ms'' [ invTbl  .ind newId .~ is'
                                                   , coinsTbl.ind newId .~ coins ]
                                 & _2 <>~ fs'
                                 & _3 <>~ newId : is'
          CorpseType     -> let (c, (is, coins))  = (getCorpse `fanUncurry` getInvCoins) (targetId, ms)
                                ic                = (mempty, mempty)
                                (newId, ms', fs ) = newCorpse ms mkEntTemplate mkObjTemplate mkConTemtplate ic c destId
                                (ms'',  fs', is') = clone newId (ms', fs, []) is
                            in p & _1 .~  upd ms'' [ invTbl  .ind newId .~ is'
                                                   , coinsTbl.ind newId .~ coins ]
                                 & _2 <>~ fs'
                                 & _3 <>~ newId : is'
          FoodType       -> let f                = getFood targetId ms
                                (newId, ms', fs) = newFood ms mkEntTemplate mkObjTemplate f destId
                            in p & _1 .~  ms'
                                 & _2 <>~ fs
                                 & _3 <>~ pure newId
          HolySymbolType -> let h                = getHolySymbol targetId ms
                                (newId, ms', fs) = newHolySymbol ms mkEntTemplate mkObjTemplate h destId
                            in p & _1 .~  ms'
                                 & _2 <>~ fs
                                 & _3 <>~ pure newId
          NpcType        -> p
          ObjType        -> let (newId, ms', fs) = newObj ms mkEntTemplate mkObjTemplate destId
                            in p & _1 .~  ms'
                                 & _2 <>~ fs
                                 & _3 <>~ pure newId
          PCType         -> p
          RmType         -> p
          VesselType     -> p
          WpnType        -> let w                = getWpn targetId ms
                                (newId, ms', fs) = newWpn ms mkEntTemplate mkObjTemplate w destId
                            in p & _1 .~  ms'
                                 & _2 <>~ fs
                                 & _3 <>~ pure newId
          WritableType   -> let w                = getWritable targetId ms
                                (newId, ms', fs) = newWritable ms mkEntTemplate mkObjTemplate w destId
                            in p & _1 .~  ms'
                                 & _2 <>~ fs
                                 & _3 <>~ pure newId
