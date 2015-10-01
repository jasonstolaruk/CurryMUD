{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Mud.Cmds.Util.Sorry where

import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Quoting
import Mud.Util.Text

import Data.Char (toLower)
import Data.Monoid ((<>))
import qualified Data.Text as T


sorryAlreadyWielding :: MudState -> Slot -> Id -> T.Text
sorryAlreadyWielding ms sl i = let s = getSing i ms in T.concat [ "You're already wielding "
                                                                , aOrAn s
                                                                , " with your "
                                                                , pp sl
                                                                , "." ]


-----


sorryBracketedMsg :: Either T.Text a
sorryBracketedMsg = Left "You can't open or close your message with brackets."


-----


sorryChanTargetName :: T.Text -> T.Text -> T.Text
sorryChanTargetName cn n = T.concat [ "There is no one by the name of "
                                    , dblQuote . capitalize $ n
                                    , " currently tuned in to the "
                                    , cn
                                    , " channel." ]


sorryAdminChanTargetName :: T.Text -> T.Text
sorryAdminChanTargetName = sorryChanTargetName "admin" . dblQuote . capitalize


sorryChanTargetNameFromContext :: T.Text -> ChanContext -> T.Text
sorryChanTargetNameFromContext n (ChanContext { .. }) = sorryChanTargetName (dblQuote . capitalize $ n) effChanName
  where
    effChanName = maybe someCmdName dblQuote someChanName


-----


sorryConnectIgnore :: T.Text
sorryConnectIgnore = sorryIgnoreLocPrefPlur "The names of the people you would like to connect"


-----


sorryDbEx :: MsgQueue -> Cols -> MudStack ()
sorryDbEx mq cols = wrapSend mq cols "There was an error when reading the database."


-----


sorryDisconnectIgnore :: T.Text
sorryDisconnectIgnore = sorryIgnoreLocPrefPlur "The names of the people you would like to disconnect"


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


sorryFullClothSlotsOneSide :: Cloth -> Slot -> T.Text
sorryFullClothSlotsOneSide (pp -> c) (pp -> s) = T.concat [ "You can't wear any more "
                                                          , c
                                                          , "s on your "
                                                          , s
                                                          , "." ]


-----


sorryHostIgnore :: T.Text
sorryHostIgnore = sorryIgnoreLocPrefPlur "The PC names of the players whose host statistics you would like to see"


-----


sorryIgnoreLocPref :: T.Text -> T.Text
sorryIgnoreLocPref msg = parensQuote $ msg <> " need not be given a location prefix. The location prefix you provided \
                                              \will be ignored."


sorryIgnoreLocPrefPlur :: T.Text -> T.Text
sorryIgnoreLocPrefPlur msg = parensQuote $ msg <> " need not be given location prefixes. The location prefixes you \
                                                  \provided will be ignored."


-----


sorryIllegalChanName :: T.Text -> T.Text -> [T.Text]
sorryIllegalChanName a msg = pure . T.concat $ [ dblQuote a, " is not a legal channel name ", parensQuote msg, "." ]


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


-----


sorryPeepIgnore :: T.Text
sorryPeepIgnore = sorryIgnoreLocPrefPlur "The PC names of the players you wish to start or stop peeping"


-----


sorryRemoveIgnore :: T.Text
sorryRemoveIgnore = sorryIgnoreLocPrefPlur "The names of the items to be removed from a container "


-----


sorryTwoWayTargetName :: ExpCmdName -> Sing -> Either T.Text a
sorryTwoWayTargetName cn s = Left . T.concat $ [ "In a telepathic message to "
                                               , s
                                               , ", the only possible target is "
                                               , s
                                               , ". Please try "
                                               , quoteColor
                                               , T.singleton expCmdChar
                                               , cn
                                               , " "
                                               , T.singleton . toLower . T.head $ s
                                               , dfltColor
                                               , " instead." ]


-----


sorryUnlinkIgnore :: T.Text
sorryUnlinkIgnore = sorryIgnoreLocPrefPlur "The names of the items to be removed from a container "


-----


sorryWrapLineLen :: MsgQueue -> Cols -> MudStack ()
sorryWrapLineLen mq cols = wrapSend mq cols . T.concat $ [ "The line length must be between "
                                                         , showText minCols
                                                         , " and "
                                                         , showText maxCols
                                                         , " characters." ]
