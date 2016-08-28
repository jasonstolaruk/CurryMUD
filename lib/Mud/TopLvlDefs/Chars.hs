{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Chars where


-- ==================================================
-- Command prefixes:


adminCmdChar, debugCmdChar :: Char
adminCmdChar = ':'
debugCmdChar = '!'


-- ==================================================
-- Denotative characters used in commands:


adverbCloseChar, adverbOpenChar, allChar, amountChar, chanTargetChar, emoteChar, emoteNameChar, emoteTargetChar, expCmdChar, indexChar, multiLineEndChar, quoteChar, sayToChar, selectorChar, slotChar :: Char
adverbCloseChar  = ']'
adverbOpenChar   = '['
allChar          = '\''
amountChar       = '/'
chanTargetChar   = '>'
emoteChar        = ';'
emoteNameChar    = '@'
emoteTargetChar  = '>'
expCmdChar       = '='
indexChar        = '.'
multiLineEndChar = '.'
quoteChar        = '`'
sayToChar        = '>'
selectorChar     = '-'
slotChar         = ':'


-- ==================================================
-- ANSI escape sequence characters:


ansiBracket, ansiEsc, ansiSGRDelimiter :: Char
ansiBracket      = '\x5B' -- 91
ansiEsc          = '\ESC' -- 27
ansiSGRDelimiter = '\x6D' -- 109


-- ============================================================
-- Characters used in the serialization of "PCDesig":


desigDelimiter, stdDesigDelimiter, nonStdDesigDelimiter :: Char
desigDelimiter       = '\128'
nonStdDesigDelimiter = '\129'
stdDesigDelimiter    = '\130'


-- ==================================================
-- Characters concerning word wrapping:


breakMarker, indentFiller, leadingSpaceChar :: Char
breakMarker      = '\131'
indentFiller     = '\132'
leadingSpaceChar = '\133'


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
fromPersonMarker = '\134'


hookArgDelimiter :: Char
hookArgDelimiter = '\135'


plaIdDelimiter :: Char
plaIdDelimiter = '\136'
