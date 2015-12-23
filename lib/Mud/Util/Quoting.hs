{-# LANGUAGE OverloadedStrings #-}

module Mud.Util.Quoting where

import Data.Text (Text)
import qualified Data.Text as T


quoteWith :: Text -> Text -> Text
quoteWith quote = quoteWith' (quote, quote)


quoteWith' :: (Text, Text) -> Text -> Text
quoteWith' (a, b) t = T.concat [ a, t, b ]


singleQuote :: Text -> Text
singleQuote = quoteWith "'"


dblQuote :: Text -> Text
dblQuote = quoteWith "\""


angleBracketQuote :: Text -> Text
angleBracketQuote = quoteWith' ("<", ">")


asteriskQuote :: Text -> Text
asteriskQuote = quoteWith' ("*** ", " ***")


backQuote :: Text -> Text
backQuote = quoteWith "`"


bracketQuote :: Text -> Text
bracketQuote = quoteWith' ("[", "]")


parensQuote :: Text -> Text
parensQuote = quoteWith' ("(", ")")


{-
Code matching either of these regex may be refactored so as to utilize the "spaced" function.
(The second regex spans lines and thus cannot be used with grep.)
\" \", .*, \" \"
" "\n.*\n *, " "
-}
spaced :: Text -> Text
spaced = quoteWith " "


unquote :: Text -> Text
unquote = T.init . T.tail
