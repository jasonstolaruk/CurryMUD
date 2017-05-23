module Mud.TopLvlDefs.Telnet.CodeMap where

import           Mud.Data.Misc
import           Mud.TopLvlDefs.Telnet.Chars

import           Control.Arrow (first)
import           Data.Char (ord)
import qualified Data.IntMap.Strict as IM (IntMap, fromList)


telnetCodeMap :: IM.IntMap TelnetCode
telnetCodeMap = IM.fromList mkList
  where
    mkList = [ first ord pair | pair <- pairs ]
    pairs  = [ (telnetAYT,         TelnetAYT        )
             , (telnetDO,          TelnetDO         )
             , (telnetDON'T,       TelnetDON'T      )
             , (telnetECHO,        TelnetECHO       )
             , (telnetEOR,         TelnetEOR        )
             , (telnetGA,          TelnetGA         )
             , (telnetGMCP,        TelnetGMCP       )
             , (telnetIAC,         TelnetIAC        )
             , (telnetIS,          TelnetIS         )
             , (telnetNOP,         TelnetNOP        )
             , (telnetSB,          TelnetSB         )
             , (telnetSE,          TelnetSE         )
             , (telnetSUPPRESS_GA, TelnetSUPPRESS_GA)
             , (telnetTTYPE,       TelnetTTYPE      )
             , (telnetWILL,        TelnetWILL       )
             , (telnetWON'T,       TelnetWON'T      ) ]
