{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Telnet where

import Data.Text (Text)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


telnetECHO, telnetEOR, telnetGA, telnetIAC, telnetIS, telnetSB, telnetSE, telnetSEND, telnetTTYPE, telnetWILL, telnetWON'T :: Char
telnetECHO  = '\x01' -- 1
telnetEOR   = '\xEF' -- 239
telnetGA    = '\xF9' -- 249
telnetIAC   = '\xFF' -- 255
telnetIS    = '\x0'  -- 0
telnetSB    = '\xFA' -- 250
telnetSE    = '\xF0' -- 240
telnetSEND  = '\x1'  -- 1
telnetTTYPE = '\x18' -- 24
telnetWILL  = '\xFB' -- 251
telnetWON'T = '\xFC' -- 252


telnetEndOfRecord, telnetGoAhead, telnetHideInput, telnetShowInput, telnetTTypeRequest, telnetTTypeResponseL, telnetTTypeResponseR, telnetWillTType :: Text
telnetEndOfRecord    = T.pack [ telnetIAC, telnetEOR                ]
telnetGoAhead        = T.pack [ telnetIAC, telnetGA                 ]
telnetHideInput      = T.pack [ telnetIAC, telnetWILL,  telnetECHO  ]
telnetShowInput      = T.pack [ telnetIAC, telnetWON'T, telnetECHO  ]
telnetTTypeRequest   = T.pack [ telnetIAC, telnetSB, telnetTTYPE, telnetSEND, telnetIAC, telnetSE ]
telnetTTypeResponseL = T.pack [ telnetIAC, telnetSB, telnetTTYPE, telnetIS ]
telnetTTypeResponseR = T.pack [ telnetIAC, telnetSE ]
telnetWillTType      = T.pack [ telnetIAC, telnetWILL,  telnetTTYPE ]
