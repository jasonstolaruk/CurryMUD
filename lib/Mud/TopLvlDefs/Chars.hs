{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Chars where


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================
-- Command prefixes:


adminCmdChar, debugCmdChar :: Char
adminCmdChar = ':'
debugCmdChar = '!'


-- ==================================================
-- Denotative characters used in commands:


adverbCloseChar, adverbOpenChar, allChar, amountChar, chanTargetChar, emoteChar, emoteNameChar, emoteTargetChar, expCmdChar, indexChar, sayToChar, selectorChar, slotChar :: Char
adverbCloseChar = ']'
adverbOpenChar  = '['
allChar         = '\''
amountChar      = '/'
chanTargetChar  = '>'
emoteChar       = ';'
emoteNameChar   = '@'
emoteTargetChar = '>'
expCmdChar      = '='
indexChar       = '.'
sayToChar       = '>'
selectorChar    = '-'
slotChar        = ':'


-- ==================================================
-- Telnet characters:


telnetECHO, telnetGA, telnetIAC, telnetSB, telnetSE, telnetWILL, telnetWON'T :: Char
telnetECHO  = '\x01' -- 1
telnetGA    = '\xF9' -- 249
telnetIAC   = '\xFF' -- 255
telnetSB    = '\xFA' -- 250
telnetSE    = '\xF0' -- 240
telnetWILL  = '\xFB' -- 251
telnetWON'T = '\xFC' -- 252


-- ==================================================
-- ANSI escape sequence characters:


ansiBracket, ansiEsc, ansiSGRDelimiter :: Char
ansiBracket      = '\x5B' -- 91
ansiEsc          = '\ESC' -- 27
ansiSGRDelimiter = '\x6D' -- 109


-- ============================================================
-- Characters used in the serialization of "PCDesig":


desigDelimiter, stdDesigDelimiter, nonStdDesigDelimiter :: Char
desigDelimiter       = '\132'
nonStdDesigDelimiter = '\131'
stdDesigDelimiter    = '\130'


-- ==================================================
-- Characters concerning word wrapping:


breakMarker, indentFiller :: Char
breakMarker  = '\129'
indentFiller = '\128'


-- ==================================================
-- Denotative characters used in text files:


charTokenDelimiter, dividerToken, indentTagChar, miscTokenDelimiter, styleTokenDelimiter :: Char
charTokenDelimiter  = '#'
dividerToken        = '-'
indentTagChar       = '`'
miscTokenDelimiter  = '@'
styleTokenDelimiter = '\\'


-- ==================================================
-- Other:


fromPersonMarker :: Char
fromPersonMarker = '\133'


plaIdDelimiter :: Char
plaIdDelimiter = '\134'
