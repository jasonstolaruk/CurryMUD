{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Padding where

import Mud.Data.State.Util.Calc
import Mud.TopLvlDefs.Misc


chanNamePadding :: Int
chanNamePadding = succ maxChanNameLen


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
racePadding = succ calcMaxRaceLen


settingNamePadding :: Int
settingNamePadding = 10


sexPadding :: Int
sexPadding = 7
