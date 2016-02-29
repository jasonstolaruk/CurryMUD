{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Chars where


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


telnetGA, telnetIAC, telnetSB, telnetSE :: Char
telnetGA  = '\249'
telnetIAC = '\255'
telnetSB  = '\250'
telnetSE  = '\240'


-- ==================================================
-- ANSI escape sequence characters:


ansiBracket, ansiEsc, ansiSGRDelimiter :: Char
ansiBracket      = '\91'
ansiEsc          = '\27'
ansiSGRDelimiter = '\109'


-- ============================================================
-- Characters used in the serialization of "PCDesig":


desigDelimiter, stdDesigDelimiter, nonStdDesigDelimiter :: Char
desigDelimiter       = '\132'
nonStdDesigDelimiter = '\131'
stdDesigDelimiter    = '\130'


-- ==================================================
-- Characters concerning word wrapping:


breakMarker, indentFiller :: Char
breakMarker  = '\128'
indentFiller = '\129'


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
