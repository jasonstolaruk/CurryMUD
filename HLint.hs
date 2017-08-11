import HLint.Default
import HLint.Dollar
import HLint.Generalise

ignore "Redundant where" = Mud.Cmds.Debug.debugCmds
                           Mud.Util.Misc.two

ignore "Use camelCase" = Mud.Cmds.Debug
                         Mud.Cmds.Msgs.Dude
                         Mud.Cmds.Msgs.Sorry
                         Mud.Cmds.Pla
                         Mud.Cmds.Util.Misc
                         Mud.Cmds.Util.Pla
                         Mud.Data.Misc
                         Mud.Data.State.MudData
                         Mud.Data.State.Util.Destroy
                         Mud.Data.State.Util.Misc
                         Mud.Misc.Logging
                         Mud.Misc.NameResolution
                         Mud.TheWorld.Zones.AdminZone
                         Mud.TheWorld.Zones.AdminZoneIds
                         Mud.Threads.LightTimer
                         Mud.TopLvlDefs.Misc
                         Mud.TopLvlDefs.Telnet.Chars

ignore "Use module export list"

ignore "Use ||" = Mud.Cmds.Util.Misc
                  Mud.Cmds.Util.Pla

ignore "Use &&" = Mud.Data.State.Util.Death
                  Mud.Data.State.Util.Misc

ignore = Mud.Cmds.Util.Misc.mkHolySymbolVol

ignore = Mud.TopLvlDefs.Vols.braceletVol
ignore = Mud.TopLvlDefs.Vols.swordVol

ignore = Mud.Util.Misc.strictId
