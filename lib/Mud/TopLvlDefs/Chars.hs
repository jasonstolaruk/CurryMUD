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
quoteChar        = '`' -- Used in ":set".
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


corpseDesigDelimiter, desigDelimiter, stdDesigDelimiter, nonStdDesigDelimiter :: Char
corpseDesigDelimiter = '\128'
desigDelimiter       = '\129'
nonStdDesigDelimiter = '\130'
stdDesigDelimiter    = '\131'


-- ==================================================
-- Characters concerning word wrapping:


breakMarker, indentFiller, leadingSpaceChar :: Char
breakMarker      = '\132'
indentFiller     = '\133'
leadingSpaceChar = '\134'


-- ==================================================
-- Denotative characters used in text files:


charTokenDelimiter, cmdToken, dividerToken, indentTagChar, miscTokenDelimiter, styleTokenDelimiter :: Char
charTokenDelimiter  = '#'
cmdToken            = '!'
dividerToken        = '-'
indentTagChar       = '`'
miscTokenDelimiter  = '@'
styleTokenDelimiter = '\\'


-- ==================================================
-- Other:


corpseNameMarker :: Char
corpseNameMarker = '\135'


fromPersonMarker :: Char
fromPersonMarker = '\136'


hookArgDelimiter :: Char
hookArgDelimiter = '\137'


plaIdDelimiter :: Char
plaIdDelimiter = '\138'
