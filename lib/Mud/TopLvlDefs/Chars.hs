{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module Mud.TopLvlDefs.Chars where


-- ==================================================
-- Command prefixes:


debugCmdChar, wizCmdChar :: Char
debugCmdChar = '!'
wizCmdChar   = ':'


-- ==================================================
-- Denotative characters used in commands:


allChar, amountChar, indexChar, rmChar, slotChar :: Char
allChar    = '\''
amountChar = '/'
indexChar  = '.'
rmChar     = '-'
slotChar   = ':'


-- ==================================================
-- Telnet characters:


telnetGA, telnetIAC, telnetSB, telnetSE :: Char
telnetGA  = '\249'
telnetIAC = '\255'
telnetSB  = '\250'
telnetSE  = '\240'


-- ============================================================
-- Characters used in the serialization of "PCDesig":


desigDelimiter, stdDesigDelimiter, nonStdDesigDelimiter :: Char
desigDelimiter       = '\132'
nonStdDesigDelimiter = '\131'
stdDesigDelimiter    = '\130'


-- ==================================================
-- Characters concerning indentation:


indentFiller, indentTagChar :: Char
indentFiller  = '\128'
indentTagChar = '`'
