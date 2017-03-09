{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Clone where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Make

import Control.Lens (_1, _2, _3)
import Control.Lens.Operators ((.~), (&), (^.), (<>~))
import Data.List (foldl')
import GHC.Stack (HasCallStack)


-- TODO: Logging.
clone :: HasCallStack => Id -> (MudState, Funs, Inv) -> Inv -> (MudState, Funs, Inv)
clone destId = foldl' helper
  where
    helper p@(ms, _, _) targetId =
        let mkEntTemplate | e <- getEnt targetId ms
                          = EntTemplate (e^.entName) (e^.sing) (e^.plur) (e^.entDesc) (e^.entSmell) (e^.entFlags)
            mkObjTemplate | o <- getObj targetId ms
                          = ObjTemplate (o^.objWeight) (o^.objVol) (o^.objTaste) (o^.objFlags)
        in case getType targetId ms of
          ArmType        -> p
          ClothType      -> p
          ConType        -> p
          CorpseType     -> p
          FoodType       -> p
          HolySymbolType -> let holy = getHolySymbol targetId ms
                                (newId, ms', fs) = newHolySymbol ms mkEntTemplate mkObjTemplate holy destId
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
          WpnType        -> p
          WritableType   -> p
