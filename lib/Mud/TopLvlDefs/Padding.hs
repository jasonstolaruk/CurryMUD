module Mud.TopLvlDefs.Padding ( bracketedEntNamePadding
                              , chanNamePadding
                              , cmdNamePadding
                              , colorNamePadding
                              , godNamePadding
                              , helpTopicPadding
                              , idPadding
                              , lvlPadding
                              , namePadding
                              , racePadding
                              , settingNamePadding
                              , sexPadding ) where

import Mud.Data.State.Util.Calc
import Mud.TopLvlDefs.Misc

bracketedEntNamePadding :: Int
bracketedEntNamePadding = 12

chanNamePadding :: Int
chanNamePadding = succ maxChanNameLen

cmdNamePadding :: Int
cmdNamePadding = succ maxCmdLen

colorNamePadding :: Int
colorNamePadding = 8

godNamePadding :: Int
godNamePadding = succ calcMaxGodNameLen

helpTopicPadding :: Int
helpTopicPadding = maxHelpTopicLen + 2

idPadding :: Int
idPadding = 6

lvlPadding :: Int
lvlPadding = 5

namePadding :: Int
namePadding = maxNameLen + 3

racePadding :: Int
racePadding = succ calcMaxRaceLen

settingNamePadding :: Int
settingNamePadding = 10

sexPadding :: Int
sexPadding = 7
