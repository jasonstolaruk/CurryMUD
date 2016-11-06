{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Telnet.Chars where

import Data.Text (Text)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


telnetAYT, telnetDO, telnetDON'T, telnetECHO, telnetEOR, telnetGA, telnetGMCP, telnetIAC, telnetIS, telnetNOP, telnetSB, telnetSE, telnetSEND, telnetSUPPRESS_GA, telnetTTYPE, telnetWILL, telnetWON'T :: Char
telnetAYT         = '\xF6' -- 246
telnetDO          = '\xFD' -- 253
telnetDON'T       = '\xFE' -- 254
telnetECHO        = '\x01' -- 1
telnetEOR         = '\xEF' -- 239
telnetGA          = '\xF9' -- 249
telnetGMCP        = '\xC9' -- 201
telnetIAC         = '\xFF' -- 255 Interpret as command
telnetIS          = '\x0'  -- 0
telnetNOP         = '\xF1' -- 241
telnetSB          = '\xFA' -- 250 Begin subnegotiation
telnetSE          = '\xF0' -- 240 End subnegotiation
telnetSEND        = '\x1'  -- 1
telnetSUPPRESS_GA = '\x3'  -- 3
telnetTTYPE       = '\x18' -- 24
telnetWILL        = '\xFB' -- 251
telnetWON'T       = '\xFC' -- 252


telnetEndOfRecord, telnetGoAhead, telnetHideInput, telnetIAC_SB, telnetIAC_SE, telnetShowInput, telnetTTypeRequest, telnetTTypeResponseL, telnetTTypeResponseR, telnetWillTType :: Text
telnetEndOfRecord    = T.pack [ telnetIAC, telnetEOR                ]
telnetGoAhead        = T.pack [ telnetIAC, telnetGA                 ]
telnetHideInput      = T.pack [ telnetIAC, telnetWILL,  telnetECHO  ]
telnetIAC_SB         = T.pack [ telnetIAC, telnetSB                 ]
telnetIAC_SE         = T.pack [ telnetIAC, telnetSE                 ]
telnetShowInput      = T.pack [ telnetIAC, telnetWON'T, telnetECHO  ]
telnetTTypeRequest   = T.pack [ telnetIAC, telnetSB, telnetTTYPE, telnetSEND, telnetIAC, telnetSE ]
telnetTTypeResponseL = T.pack [ telnetIAC, telnetSB, telnetTTYPE, telnetIS ] -- TODO: Delete.
telnetTTypeResponseR = T.pack [ telnetIAC, telnetSE                        ] -- TODO: Delete.
telnetWillTType      = T.pack [ telnetIAC, telnetWILL,  telnetTTYPE        ]
