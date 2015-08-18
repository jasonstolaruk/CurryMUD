module Mud.TopLvlDefs.Padding where

import Mud.Data.State.Util.Misc
import Mud.TopLvlDefs.Misc


cmdNamePadding :: Int
cmdNamePadding = succ maxCmdLen


colorNamePadding :: Int
colorNamePadding = 8


entNamePadding :: Int
entNamePadding = 11


helpTopicPadding :: Int
helpTopicPadding = maxHelpTopicLen + 2


lvlPadding :: Int
lvlPadding = 5


namePadding :: Int
namePadding = maxNameLen + 3


racePadding :: Int
racePadding = succ maxRaceLen


settingNamePadding :: Int
settingNamePadding = 10


sexPadding :: Int
sexPadding = 7
