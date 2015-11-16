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


specifyFullHelper :: T.Text -> T.Text
specifyFullHelper t = parensQuote $ "Note that you must specify the full " <> t <> "."


-----


hintAMsg :: Sing -> T.Text
hintAMsg s = hintHelper [ "the above is a message from "
                        , s
                        , ", a CurryMUD administrator. To reply, type "
                        , colorWith quoteColor $ "admin " <> uncapitalize s <> " msg"
                        , ", where "
                        , dblQuote "msg"
                        , " is the message you want to send to "
                        , s
                        , "." ]


hintABan :: T.Text
hintABan = specifyFullHelper "PC name of the player you wish to ban"


hintABoot :: T.Text
hintABoot = specifyFullHelper "PC name of the player you wish to boot"


hintASudoer :: T.Text
hintASudoer = specifyFullHelper "PC name of the player you wish to promote/demote"


-----


hintDisconnect :: T.Text
hintDisconnect = specifyFullHelper "name of the person you would like to disconnect"


hintGet :: T.Text
hintGet = hintHelper [ "it appears that you want to remove an object from a container. In that case, please use the "
                     , dblQuote "remove"
                     , " command. For example, to remove a ring from your sack, type "
                     , colorWith quoteColor "remove ring sack"
                     , "." ]


hintLook :: T.Text
hintLook = hintHelper [ "use the "
                      , dblQuote "look"
                      , " command to examine one or more items in your current room. To examine items in your \
                        \inventory, use the "
                      , dblQuote "inventory"
                      , " command "
                      , parensQuote $ "for example: " <> colorWith quoteColor "inventory bread"
                      , ". To examine items in your readied equipment, use the "
                      , dblQuote "equipment"
                      , " command "
                      , parensQuote $ "for example: " <> colorWith quoteColor "equipment sword"
                      , ". "
                      , colorWith quoteColor "inventory"
                      , " and "
                      , colorWith quoteColor "equipment"
                      , " alone will list the items in your inventory and readied equipment, respectively." ]


hintSay :: T.Text
hintSay = hintHelper [ "to communicate with non-player characters, use the "
                     , dblQuote "ask"
                     , " command. For example, to ask a city guard about crime, type "
                     , colorWith quoteColor "ask guard crime"
                     , "." ]


hintUnlink :: T.Text
hintUnlink = specifyFullHelper "name of the person with whom you would like to unlink"
