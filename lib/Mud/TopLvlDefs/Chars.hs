module Mud.TopLvlDefs.Chars where

-- ==================================================
-- Cmd prefixes:

adminCmdChar, debugCmdChar :: Char
adminCmdChar = ':'
debugCmdChar = '!'

-- ==================================================
-- Denotative characters used in cmds:

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
-- Delimiters:

corpseDesigDelimiter, nonStdDesigDelimiter, sectionDelimiter, stdDesigDelimiter, verbObjDelimiter :: Char
corpseDesigDelimiter = '\128'
nonStdDesigDelimiter = '\129'
sectionDelimiter     = '\130'
stdDesigDelimiter    = '\131'
verbObjDelimiter     = '\132'

-- ==================================================
-- Characters concerning word wrapping:

breakMarker, indentFiller, leadingSpaceChar :: Char
breakMarker      = '\133'
indentFiller     = '\134'
leadingSpaceChar = '\135'

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
corpseNameMarker = '\136'

fromPersonMarker :: Char
fromPersonMarker = '\137'

hookArgDelimiter :: Char
hookArgDelimiter = '\138'

plaIdDelimiter :: Char
plaIdDelimiter = '\139'
