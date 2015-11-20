{-# LANGUAGE OverloadedStrings #-}

module Mud.Util.Quoting where

import qualified Data.Text as T


quoteWith :: T.Text -> T.Text -> T.Text
quoteWith quote = quoteWith' (quote, quote)


quoteWith' :: (T.Text, T.Text) -> T.Text -> T.Text
quoteWith' (a, b) t = T.concat [ a, t, b ]


singleQuote :: T.Text -> T.Text
singleQuote = quoteWith "'"


dblQuote :: T.Text -> T.Text
dblQuote = quoteWith "\""


angleBracketQuote :: T.Text -> T.Text
angleBracketQuote = quoteWith' ("<", ">")


asteriskQuote :: T.Text -> T.Text
asteriskQuote = quoteWith' ("*** ", " ***")


backQuote :: T.Text -> T.Text
backQuote = quoteWith "`"


bracketQuote :: T.Text -> T.Text
bracketQuote = quoteWith' ("[", "]")


parensQuote :: T.Text -> T.Text
parensQuote = quoteWith' ("(", ")")


{-
Code matching either of these regex may be refactored so as to utilize the "spaced" function.
(The second regex spans lines and thus cannot be used with grep.)
\" \", .*, \" \"
" "\n.*\n *, " "
-}
spaced :: T.Text -> T.Text
spaced = quoteWith " "


unquote :: T.Text -> T.Text
unquote = T.init . T.tail
