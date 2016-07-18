{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Data.State.Util.New where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Biodegrader
import Mud.Util.Misc

import Control.Lens (view)
import Control.Lens.Operators ((%~), (&), (.~))
import Control.Monad (when)


type InvId = Id


newObj :: Ent -> Obj -> InvId -> MudStack ()
newObj e@(view entId -> i) o invId = do
    tweak $ \ms -> ms & entTbl .ind i .~ e
                      & objTbl .ind i .~ o
                      & typeTbl.ind i .~ ObjType
    tweak $ \ms -> ms & invTbl .ind invId %~ addToInv ms (pure i)
    when (isBiodegradable o) . runBiodegAsync $ i
