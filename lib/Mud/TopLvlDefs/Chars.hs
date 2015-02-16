{-# OPTIONS_GHC -Wall -Werror #-}

module Mud.TopLvlDefs.Chars where


-- ==================================================
-- Command prefixes:


adminCmdChar, debugCmdChar :: Char
adminCmdChar = ':'
debugCmdChar = '!'


-- ==================================================
-- Denotative characters used in commands:


adverbCloseChar, adverbOpenChar, allChar, amountChar, emoteNameChar, indexChar, rmChar, sayToChar, slotChar :: Char
adverbCloseChar = ']'
adverbOpenChar  = '['
allChar         = '\''
amountChar      = '/'
emoteNameChar   = '@'
indexChar       = '.'
rmChar          = '-'
sayToChar       = '-'
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


charTokenDelimiter, indentTagChar, msgTokenDelimiter, styleTokenDelimiter :: Char
charTokenDelimiter  = '#'
indentTagChar       = '`'
msgTokenDelimiter   = '@'
styleTokenDelimiter = '\\'
