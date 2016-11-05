{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Telnet.CodeMap where

import Mud.Data.Misc
import Mud.TopLvlDefs.Telnet.Chars

import Control.Arrow (first)
import Data.Char (ord)
import qualified Data.IntMap.Lazy as IM (IntMap, fromList)


telnetCodeMap :: IM.IntMap TelnetCode
telnetCodeMap = IM.fromList mkList
  where
    mkList = [ first ord pair | pair <- pairs ]
    pairs  = [ (telnetECHO,  TelnetECHO )
             , (telnetEOR,   TelnetEOR  )
             , (telnetGA,    TelnetGA   )
             , (telnetGMCP,  TelnetGMCP )
             , (telnetIAC,   TelnetIAC  )
             , (telnetIS,    TelnetIS   )
             , (telnetSB,    TelnetSB   )
             , (telnetSE,    TelnetSE   )
             , (telnetSEND,  TelnetSEND )
             , (telnetTTYPE, TelnetTTYPE)
             , (telnetWILL,  TelnetWILL )
             , (telnetWON'T, TelnetWON'T) ]
