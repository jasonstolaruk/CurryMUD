{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Data.State.Util.New where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Biodegrader
import Mud.Util.Misc

import Control.Lens (view)
import Control.Lens.Operators ((%~), (&), (.~), (<>~))


type InvId = Id


newObj :: MudState -> Ent -> Obj -> InvId -> MudState
newObj ms e@(view entId -> i) o invId = let ms' = ms & entTbl .ind i .~ e
                                                     & objTbl .ind i .~ o
                                                     & typeTbl.ind i .~ ObjType
                                                     & helper
                                        in ms' & invTbl.ind invId %~ addToInv ms' (pure i)
  where
    helper = onTrue (isBiodegradable o) (opList <>~ pure (runBiodegraderAsync i))
