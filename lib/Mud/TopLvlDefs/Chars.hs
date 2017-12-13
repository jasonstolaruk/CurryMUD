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

corpseDesigDelimiter, hookArgDelimiter, nonStdDesigDelimiter, plaIdDelimiter, sectionDelimiter, stdDesigDelimiter, verbObjDelimiter :: Char
corpseDesigDelimiter = '\128'
hookArgDelimiter     = '\129'
nonStdDesigDelimiter = '\130'
plaIdDelimiter       = '\131'
sectionDelimiter     = '\132'
stdDesigDelimiter    = '\133'
verbObjDelimiter     = '\134'

-- ==================================================
-- Characters concerning word wrapping:

breakMarker, indentFiller, leadingSpaceChar :: Char
breakMarker      = '\135'
indentFiller     = '\136'
leadingSpaceChar = '\137'

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
corpseNameMarker = '\138'

fromPersonMarker :: Char
fromPersonMarker = '\139'

forSpiritOnlyMarker :: Char
forSpiritOnlyMarker = '\140'
