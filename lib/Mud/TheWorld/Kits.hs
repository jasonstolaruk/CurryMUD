module Mud.TheWorld.Kits (kit) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Clone
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.TheWorld.Zones.WarehouseIds
import Mud.Util.Misc

import GHC.Stack (HasCallStack)


kit :: HasCallStack => Id -> MudStack ()
kit i = modifyStateSeq helper
  where
    helper ms = case getRace i ms of Dwarf     -> (ms, [])
                                     Elf       -> (ms, [])
                                     Felinoid  -> (ms, [])
                                     Hobbit    -> (ms, [])
                                     Human     -> handleHuman
                                     Lagomorph -> (ms, [])
                                     Nymph     -> (ms, [])
                                     Vulpenoid -> (ms, [])
      where
        f           = dropFst . clone i ([], ms, [])
        handleHuman = let is = pure iBoots
                      in f is
