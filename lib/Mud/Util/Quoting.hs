{-# LANGUAGE OverloadedStrings #-}

module Mud.Util.Quoting where

import           Data.Text (Text)
import qualified Data.Text as T

quoteWith :: Text -> Text -> Text
quoteWith quote = quoteWith' (quote, quote)

quoteWith' :: (Text, Text) -> Text -> Text
quoteWith' (a, b) t = T.concat [ a, t, b ]

unquote :: Text -> Text
unquote = T.init . T.tail

-----

angleBracketQuote :: Text -> Text
angleBracketQuote = quoteWith' ("<", ">")

asteriskQuote :: Text -> Text
asteriskQuote = quoteWith' ("*** ", " ***")

bracketQuote :: Text -> Text
bracketQuote = quoteWith' ("[", "]")

curlyQuote :: Text -> Text
curlyQuote = quoteWith' ("{", "}")

dblQuote :: Text -> Text
dblQuote = quoteWith "\""

parensQuote :: Text -> Text
parensQuote = quoteWith' ("(", ")")

singleQuote :: Text -> Text
singleQuote = quoteWith "'"

{-
Code matching either of these regex may be refactored so as to utilize the "spaced" function. (The second regex spans
lines and thus cannot be used with grep.)
\" \", .*, \" \"
" "\n.*\n *, " "
-}
spaced :: Text -> Text
spaced = quoteWith " "
