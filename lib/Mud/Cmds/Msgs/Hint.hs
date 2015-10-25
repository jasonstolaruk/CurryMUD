{-# LANGUAGE OverloadedStrings #-}

module Mud.Cmds.Msgs.Hint where

import Mud.Data.State.MudData
import Mud.Misc.ANSI
import Mud.Util.Quoting
import Mud.Util.Text

import Data.Monoid ((<>))
import qualified Data.Text as T


hintHelper :: [T.Text] -> T.Text
hintHelper t = quoteWith' (hintANSI, noHintANSI) "Hint:" <> " " <> T.concat t


-----


hintAMsg :: Sing -> T.Text
hintAMsg s = hintHelper [ "the above is a message from "
                        , s
                        , ", a CurryMUD administrator. To reply, type "
                        , quoteColor
                        , "admin "
                        , uncapitalize s
                        , " msg"
                        , dfltColor
                        , ", where "
                        , dblQuote "msg"
                        , " is the message you want to send to "
                        , s
                        , "." ]


hintDisconnect :: T.Text
hintDisconnect = parensQuote "Note that you must specify the full name of the person you would like to disconnect."


hintGet :: T.Text
hintGet = hintHelper [ "it appears that you want to remove an object from a container. In that case, please use the "
                     , dblQuote "remove"
                     , " command. For example, to remove a ring from your sack, type "
                     , quoteColor
                     , "remove ring sack"
                     , dfltColor
                     , "." ]


hintLook :: T.Text
hintLook = hintHelper [ "use the "
                      , dblQuote "look"
                      , " command to examine one or more items in your current room. To examine items in your \
                        \inventory, use the "
                      , dblQuote "inventory"
                      , " command "
                      , parensQuote $ "for example: " <> quoteWith' (quoteColor, dfltColor) "inventory bread"
                      , ". To examine items in your readied equipment, use the "
                      , dblQuote "equipment"
                      , " command "
                      , parensQuote $ "for example: " <> quoteWith' (quoteColor, dfltColor) "equipment sword"
                      , ". "
                      , quoteColor
                      , "inventory"
                      , dfltColor
                      , " and "
                      , quoteColor
                      , "equipment"
                      , dfltColor
                      , " alone will list the items in your inventory and readied equipment, respectively." ]


hintSay :: T.Text
hintSay = hintHelper [ "to communicate with non-player characters, use the "
                     , dblQuote "ask"
                     , " command. For example, to ask a city guard about crime, type "
                     , quoteColor
                     , "ask guard crime"
                     , dfltColor
                     , "." ]
