module Mud.Cmds.Util.Sorry where

import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.Util.Quoting
import Mud.Util.Text

import Data.Monoid ((<>))
import qualified Data.Text as T


sorryAdminChanName :: T.Text -> T.Text
sorryAdminChanName n = "There is no admin by the name of " <>
                       (dblQuote . capitalize $ n)         <>
                       " currently tuned in to the admin channel."


-----


sorryAdminHostIgnore :: T.Text
sorryAdminHostIgnore = sorryIgnoreLocPrefPlur "The PC names of the players whose host statistics you would like to see"


-----


sorryAdminPeepIgnore :: T.Text
sorryAdminPeepIgnore = sorryIgnoreLocPrefPlur "The PC names of the players you wish to start or stop peeping"


-----


sorryBracketedMsg :: Either T.Text a
sorryBracketedMsg = Left "You can't open or close your message with brackets."


-----


sorryDbEx :: MsgQueue -> Cols -> MudStack ()
sorryDbEx mq cols = wrapSend mq cols "There was an error when reading the database."


-----


sorryExpCmdName :: T.Text -> Either T.Text a
sorryExpCmdName cn = Left $ "There is no expressive command by the name of " <> dblQuote cn <> "."


sorryExpCmdRequiresTarget :: ExpCmdName -> T.Text
sorryExpCmdRequiresTarget cn = "The " <> dblQuote cn <> " expressive command requires a single target."


sorryExpCmdTooLong :: Either T.Text a
sorryExpCmdTooLong = Left "An expressive command sequence may not be more than 2 words long."


sorryExpCmdWithTarget :: ExpCmdName -> T.Text
sorryExpCmdWithTarget cn =  "The " <> dblQuote cn <> " expressive command cannot be used with a target."


-----


sorryIgnoreLocPref :: T.Text -> T.Text
sorryIgnoreLocPref msg = parensQuote $ msg <> " need not be given a location prefix. The location prefix you provided \
                                              \will be ignored."


sorryIgnoreLocPrefPlur :: T.Text -> T.Text
sorryIgnoreLocPrefPlur msg = parensQuote $ msg <> " need not be given location prefixes. The location prefixes you \
                                                  \provided will be ignored."


-----


sorryNoMsg :: T.Text
sorryNoMsg = "You must also provide a message to send."


-----


sorryNoOneListening :: MsgQueue -> Cols -> T.Text -> MudStack ()
sorryNoOneListening mq cols n = wrapSend mq cols $ "You are the only person tuned in to the " <> n <> " channel."


-----


sorryNotTunedICChan :: MsgQueue -> Cols -> ChanName -> MudStack ()
sorryNotTunedICChan = sorryNotTunedChan "tune"


sorryNotTunedChan :: T.Text -> MsgQueue -> Cols -> T.Text -> MudStack ()
sorryNotTunedChan x mq cols y = wrapSend mq cols . T.concat $ [ "You have tuned out the "
                                                              , dblQuote y
                                                              , " channel. Type "
                                                              , quoteColor
                                                              , x
                                                              , " "
                                                              , y
                                                              , "=in"
                                                              , dfltColor
                                                              , " to tune it back in." ]


sorryNotTunedOOCChan :: MsgQueue -> Cols -> T.Text -> MudStack ()
sorryNotTunedOOCChan = sorryNotTunedChan "set"
