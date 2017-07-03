module Mud.TheWorld.Kits (kit) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Clone
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.TheWorld.Zones.WarehouseIds
import Mud.Util.Misc

import GHC.Stack (HasCallStack)


-- TODO: Include light source and holy symbol.
kit :: HasCallStack => Id -> MudStack ()
kit i = modifyStateSeq helper
  where
    helper ms = dropFst . cloneCommon . clone i ([], ms, []) . (`getInvHelper` ms) $ case getRace i ms of
        Dwarf     -> iDwarfKit
        Elf       -> iElfKit
        Felinoid  -> iFelinoidKit
        Hobbit    -> iHobbitKit
        Human     -> iHumanKit
        Lagomorph -> iLagomorphKit
        Nymph     -> iNymphKit
        Vulpenoid -> iVulpenoidKit
    cloneCommon triple@(_, ms, _) = clone i triple . getInvHelper iCommonKit $ ms
    getInvHelper targetId ms      = let is = getInv targetId ms
                                    in filter ((`notElem` [ CorpseType, NpcType, PlaType ]) . (`getType` ms)) is
