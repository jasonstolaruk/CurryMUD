{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Telnet where

import Data.Text (Text)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


telnetECHO, telnetGA, telnetIAC, telnetSB, telnetSE, telnetWILL, telnetWON'T :: Char
telnetECHO  = '\x01' -- 1
telnetGA    = '\xF9' -- 249
telnetIAC   = '\xFF' -- 255
telnetSB    = '\xFA' -- 250
telnetSE    = '\xF0' -- 240
telnetWILL  = '\xFB' -- 251
telnetWON'T = '\xFC' -- 252


telnetHideInput, telnetShowInput :: Text
telnetHideInput = T.pack [ telnetIAC, telnetWILL,  telnetECHO ]
telnetShowInput = T.pack [ telnetIAC, telnetWON'T, telnetECHO ]
