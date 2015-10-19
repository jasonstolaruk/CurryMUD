{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Cmds.Util.Advice ( adviceAAnnounceNoArgs
                            , adviceABanHostNoReason
                            , adviceABanPlaNoReason
                            , adviceABootNoArgs
                            , adviceAdminNoMsg
                            , adviceAdverbCloseChar
                            , adviceAHostNoArgs
                            , adviceAMsgNoArgs
                            , adviceAMsgNoMsg
                            , adviceAMyChansNoArgs
                            , adviceAPeepNoArgs
                            , adviceAPrintNoArgs
                            , adviceASudoerExcessArgs
                            , adviceASudoerNoArgs
                            , adviceATelePlaExcessArgs
                            , adviceATelePlaNoArgs
                            , adviceATeleRmExcessArgs
                            , adviceBugNoArgs
                            , adviceConnectNoArgs
                            , adviceConnectNoChan
                            , adviceDCinsExcessArgs
                            , adviceDCinsNoArgs
                            , adviceDIdExcessArgs
                            , adviceDIdNoArgs
                            , adviceDisconnectNoArgs
                            , adviceDisconnectNoChan
                            , adviceDNumberExcessArgs
                            , adviceDNumberNoArgs
                            , adviceDNumberNoBase
                            , adviceDropNoArgs
                            , adviceDWeightExcessArgs
                            , adviceDWeightNoArgs
                            , adviceDWrapExcessArgs
                            , adviceDWrapIndentExcessArgs
                            , adviceDWrapIndentNoAmt
                            , adviceDWrapIndentNoArgs
                            , adviceDWrapNoArgs
                            , adviceEmoteNoArgs
                            , adviceEmptyAdverb
                            , adviceEnc
                            , adviceEtc
                            , adviceEtcEmptyPoss
                            , adviceEtcHead
                            , adviceEtcInTwoWay
                            , adviceExpCmdExcessArgs
                            , adviceGetNoArgs
                            , adviceLeaveNoArgs
                            , adviceNewChanNoArgs
                            , advicePutNoArgs
                            , advicePutNoCon
                            , adviceReadyNoArgs
                            , adviceRemoveNoArgs
                            , adviceRemoveNoCon
                            , adviceSayAdverbNoUtterance
                            , adviceSayNoArgs
                            , adviceSayToNoUtterance
                            , adviceSettingsInvalid
                            , adviceShowNoArgs
                            , adviceShowNoName
                            , adviceTeleNoArgs
                            , adviceTeleNoMsg
                            , adviceTuneInvalid
                            , adviceTypoNoArgs
                            , adviceUnlinkNoArgs
                            , adviceUnreadyNoArgs
                            , adviceYouEmote
                            , adviceYouEmoteChar
                            , advise ) where

import Mud.Cmds.Util.CmdPrefixes
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.Advice"


-- ==================================================


advise :: ActionParams -> [HelpName] -> T.Text -> MudStack ()
advise (Advising mq cols) []  msg = wrapSend mq cols msg
advise (Advising mq cols) [h] msg = multiWrapSend mq cols [ msg, T.concat [ "For more information, type "
                                                                          , quoteColor
                                                                          , "help "
                                                                          , h
                                                                          , dfltColor
                                                                          , "." ] ]
advise (Advising mq cols) (dblQuote . T.intercalate (dblQuote ", ") -> helpTopics) msg =
    multiWrapSend mq cols [ msg, "For more information, see the following help articles: " <> helpTopics <> "." ]
advise p hs msg = patternMatchFail "advise" [ showText p, showText hs, msg ]


-----


adviceAAnnounceNoArgs :: T.Text
adviceAAnnounceNoArgs = T.concat [ "You must provide a message to send, as in "
                                 , quoteColor
                                 , prefixAdminCmd "announce"
                                 , " CurryMUD will be shutting down for maintenance in 30 minutes"
                                 , dfltColor
                                 , "." ]


adviceABanHostNoReason :: T.Text -> T.Text
adviceABanHostNoReason a = T.concat [ "Please also provide a reason, as in "
                                    , quoteColor
                                    , prefixAdminCmd "banhost "
                                    , a
                                    , " used by Taro"
                                    , dfltColor
                                    , "." ]


adviceABanPlaNoReason :: T.Text -> T.Text
adviceABanPlaNoReason a = T.concat [ "Please also provide a reason, as in "
                                   , quoteColor
                                   , prefixAdminCmd "banplayer "
                                   , a
                                   , " for harassing hanako"
                                   , dfltColor
                                   , "." ]


adviceABootNoArgs :: T.Text
adviceABootNoArgs = "Please specify the full PC name of the player you wish to boot, optionally followed by a custom \
                    \message."


adviceAHostNoArgs :: T.Text
adviceAHostNoArgs = "Please specify the PC names of one or more players whose host statistics you would like to see."


adviceAMsgNoArgs :: T.Text
adviceAMsgNoArgs = T.concat [ "Please specify the PC name of a regular player followed by a message, as in "
                            , quoteColor
                            , prefixAdminCmd "message"
                            , " taro thank you for reporting the bug you found"
                            , dfltColor
                            , "." ]


adviceAMsgNoMsg :: T.Text -> T.Text
adviceAMsgNoMsg a = T.concat [ "Please also provide a message to send, as in "
                             , quoteColor
                             , prefixAdminCmd "message "
                             , a
                             , " thank you for reporting the bug you found"
                             , dfltColor
                             , "." ]


adviceAMyChansNoArgs :: T.Text
adviceAMyChansNoArgs = "Please specify the PC names of one or more players whose channel information you'd like to see."


adviceAPeepNoArgs :: T.Text
adviceAPeepNoArgs = "Please specify the PC names of one or more players you wish to start or stop peeping."


adviceAPrintNoArgs :: T.Text
adviceAPrintNoArgs = T.concat [ "You must provide a message to print to the server console, as in "
                              , quoteColor
                              , prefixAdminCmd "print"
                              , " is anybody home?"
                              , dfltColor
                              , "." ]


adviceASudoerExcessArgs :: T.Text
adviceASudoerExcessArgs = "Sorry, but you can only promote/demote one player at a time."


adviceASudoerNoArgs :: T.Text
adviceASudoerNoArgs = "Please specify the full PC name of the player you wish to promote/demote."


adviceATelePlaExcessArgs :: T.Text
adviceATelePlaExcessArgs = "Please specify a single PC name."


adviceATelePlaNoArgs :: T.Text
adviceATelePlaNoArgs = "Please specify the PC name of the player to which you want to teleport."


adviceATeleRmExcessArgs :: T.Text
adviceATeleRmExcessArgs = T.concat [ "Please provide one argument: the name of the room to which you'd like to \
                                     \teleport, as in "
                                   , quoteColor
                                   , prefixAdminCmd "telerm"
                                   , " lounge"
                                   , dfltColor
                                   , "." ]


-----


adviceDCinsExcessArgs :: T.Text
adviceDCinsExcessArgs = T.concat [ "Please provide one argument: the target ID, as in "
                                 , quoteColor
                                 , prefixDebugCmd "cins"
                                 , " 100"
                                 , dfltColor
                                 , "." ]


adviceDCinsNoArgs :: T.Text
adviceDCinsNoArgs = T.concat [ "Please provide one argument: the target ID, as in "
                             , quoteColor
                             , prefixDebugCmd "cins"
                             , " 100"
                             , dfltColor
                             , "." ]


adviceDIdExcessArgs :: T.Text
adviceDIdExcessArgs = T.concat [ "Please provide one argument: the ID to search for, as in "
                               , quoteColor
                               , prefixDebugCmd "id"
                               , " 100"
                               , dfltColor
                               , "." ]


adviceDIdNoArgs :: T.Text
adviceDIdNoArgs = T.concat [ "Please specify an ID to search for, as in "
                           , quoteColor
                           , prefixDebugCmd "id"
                           , " 100"
                           , dfltColor
                           , "." ]


adviceDNumberExcessArgs :: T.Text
adviceDNumberExcessArgs = T.concat [ "Please provide two arguments: a number and its base, as in "
                                   , quoteColor
                                   , prefixDebugCmd "number"
                                   , " a 16"
                                   , dfltColor
                                   , "." ]


adviceDNumberNoArgs :: T.Text
adviceDNumberNoArgs = T.concat [ "Please specify a number followed by its base, as in "
                               , quoteColor
                               , prefixDebugCmd "number"
                               , " a 16"
                               , dfltColor
                               , "." ]


adviceDNumberNoBase :: T.Text
adviceDNumberNoBase = T.concat [ "Please also specify base, as in "
                               , quoteColor
                               , prefixDebugCmd "number"
                               , " a 16"
                               , dfltColor
                               , "." ]


adviceDWeightExcessArgs :: T.Text
adviceDWeightExcessArgs = T.concat [ "Please provide one argument: the ID for which you would like to calculate weight, as \
                                     \in "
                                   , quoteColor
                                   , prefixDebugCmd "weight"
                                   , " 100"
                                   , dfltColor
                                   , "." ]


adviceDWeightNoArgs :: T.Text
adviceDWeightNoArgs = T.concat [ "Please specify an ID for which you would like to calculate weight, as in "
                               , quoteColor
                               , prefixDebugCmd "weight"
                               , " 100"
                               , dfltColor
                               , "." ]


adviceDWrapExcessArgs :: T.Text
adviceDWrapExcessArgs = T.concat [ "Please provide one argument: line length, as in "
                                 , quoteColor
                                 , prefixDebugCmd "wrap"
                                 , " 40"
                                 , dfltColor
                                 , "." ]


adviceDWrapIndentExcessArgs :: T.Text
adviceDWrapIndentExcessArgs = T.concat [ "Please provide two arguments: line length and indent amount, as in "
                                       , quoteColor
                                       , prefixDebugCmd "wrapindent"
                                       , " 40 4"
                                       , dfltColor
                                       , "." ]


adviceDWrapIndentNoAmt :: T.Text
adviceDWrapIndentNoAmt = T.concat [ "Please also specify indent amount, as in "
                                  , quoteColor
                                  , prefixDebugCmd "wrapindent"
                                  , " 40 4"
                                  , dfltColor
                                  , "." ]


adviceDWrapIndentNoArgs :: T.Text
adviceDWrapIndentNoArgs = T.concat [ "Please specify line length followed by indent amount, as in "
                                   , quoteColor
                                   , prefixDebugCmd "wrapindent"
                                   , " 40 4"
                                   , dfltColor
                                   , "." ]


adviceDWrapNoArgs :: T.Text
adviceDWrapNoArgs =  T.concat [ "Please specify line length, as in "
                              , quoteColor
                              , prefixDebugCmd "wrap"
                              , " 40"
                              , dfltColor
                              , "." ]


-----


adviceAdminNoMsg :: T.Text -> T.Text
adviceAdminNoMsg a = T.concat [ "Please also provide a message to send, as in "
                              , quoteColor
                              , "admin "
                              , a
                              , " are you available? I need your assistance"
                              , dfltColor
                              , "." ]


adviceAdverbCloseChar :: T.Text
adviceAdverbCloseChar = "An adverbial phrase must be terminated with a " <> dblQuote acl <> adverbExample


adverbExample :: T.Text
adverbExample = T.concat [ ", as in "
                         , quoteColor
                         , "say "
                         , quoteWith' (aop, acl) "enthusiastically"
                         , " nice to meet you, too"
                         , dfltColor
                         , "." ]


adviceBugNoArgs :: T.Text
adviceBugNoArgs = T.concat [ "Please describe the bug you've found, as in "
                           , quoteColor
                           , "bug i've fallen and I can't get up!"
                           , dfltColor
                           , "." ]


adviceConnectNoArgs :: T.Text
adviceConnectNoArgs = T.concat [ "Please specify the names of one or more people followed by the name of a telepathic \
                                 \channel to connect them to, as in "
                               , quoteColor
                               , "connect taro hunt"
                               , dfltColor
                               , "." ]


adviceConnectNoChan :: T.Text -> T.Text
adviceConnectNoChan a = T.concat [ "Please also specify the name of a telepathic channel, as in "
                                 , quoteColor
                                 , "connect "
                                 , a
                                 , " hunt"
                                 , dfltColor
                                 , "." ]


adviceDisconnectNoArgs :: T.Text
adviceDisconnectNoArgs = T.concat [ "Please provide the full names of one or more people followed by the name of a \
                                    \telepathic channel to disconnect them from, as in "
                                  , quoteColor
                                  , "disconnect taro hunt"
                                  , dfltColor
                                  , "." ]


adviceDisconnectNoChan :: T.Text -> T.Text
adviceDisconnectNoChan a = T.concat [ "Please also provide the name of a telepathic channel, as in "
                                    , quoteColor
                                    , "disconnect "
                                    , a
                                    , " hunt"
                                    , dfltColor
                                    , "." ]


adviceDropNoArgs :: T.Text
adviceDropNoArgs = T.concat [ "Please specify one or more items to drop, as in "
                            , quoteColor
                            , "drop sword"
                            , dfltColor
                            , "." ]


adviceEmoteNoArgs :: T.Text
adviceEmoteNoArgs = T.concat [ "Please provide a description of an action, as in "
                             , quoteColor
                             , "emote laughs with relief as tears roll down her face"
                             , dfltColor
                             , "." ]


adviceEmptyAdverb :: T.Text
adviceEmptyAdverb = T.concat [ "Please provide an adverbial phrase between "
                             , dblQuote aop
                             , " and "
                             , dblQuote acl
                             , adverbExample ]


adviceEnc :: T.Text -> T.Text
adviceEnc cn = T.concat [ dblQuote enc
                        , " must either be used alone, or with a "
                        , dblQuote "'s"
                        , " suffix "
                        , parensQuote "to create a possessive noun"
                        , ", as in "
                        , quoteColor
                        , cn
                        , "shielding her eyes from the sun, "
                        , enc
                        , " looks out across the plains"
                        , dfltColor
                        , ", or "
                        , quoteColor
                        , cn
                        , enc
                        , "'s leg twitches involuntarily as she laughs with gusto"
                        , dfltColor
                        , "." ]


adviceEtc :: T.Text -> T.Text
adviceEtc cn = T.concat [ dblQuote etc
                        , " must be immediately followed by the name of the person you wish to target, as in "
                        , quoteColor
                        , cn
                        , "slowly turns her head to look directly at "
                        , etc
                        , "taro"
                        , dfltColor
                        , ". To create a possessive noun, append "
                        , dblQuote "'s"
                        , " to the target name, as in "
                        , quoteColor
                        , cn
                        , "places her hand firmly on "
                        , etc
                        , "taro's shoulder"
                        , dfltColor
                        , "." ]


adviceEtcEmptyPoss :: T.Text
adviceEtcEmptyPoss = T.concat [ "You must specify the name of the person you want to target between "
                              , dblQuote etc
                              , " and "
                              , dblQuote "'s"
                              , "." ]


adviceEtcHead :: T.Text
adviceEtcHead = "You can't begin an emote with a target."


adviceEtcInTwoWay :: T.Text -> T.Text -> T.Text
adviceEtcInTwoWay cn cn' = T.concat [ "Sorry, but you can't use "
                                    , dblQuote etc
                                    , " in private two-way communication, as with the "
                                    , dblQuote cn
                                    ,  " command. It is legal to use forms of the word "
                                    , dblQuote "you"
                                    , " here, so instead of "
                                    , quoteColor
                                    , cn'
                                    , "gives "
                                    , etc
                                    , "hanako a smooch!"
                                    , dfltColor
                                    , ", you should type "
                                    , quoteColor
                                    , cn'
                                    , "gives you a smooch!"
                                    , dfltColor
                                    , "." ]


adviceExpCmdExcessArgs :: T.Text
adviceExpCmdExcessArgs = "Sorry, but you can only target one person at a time with expressive commands."


adviceGetNoArgs :: T.Text
adviceGetNoArgs = T.concat [ "Please specify one or more items to pick up, as in "
                           , quoteColor
                           , "get sword"
                           , dfltColor
                           , "." ]


adviceLeaveNoArgs :: T.Text
adviceLeaveNoArgs = T.concat [ "Please specify the names of one or more channels to leave, as in "
                             , quoteColor
                             , "leave hunt"
                             , dfltColor
                             , "." ]


adviceNewChanNoArgs :: T.Text
adviceNewChanNoArgs = T.concat [ "Please specify one or more new channel names, as in "
                               , quoteColor
                               , "newchannel hunt"
                               , dfltColor
                               , "." ]


advicePutNoArgs :: T.Text
advicePutNoArgs = T.concat [ "Please specify one or more items you want to put followed by where you want to put them, \
                             \as in "
                           , quoteColor
                           , "put doll sack"
                           , dfltColor
                           , "." ]


advicePutNoCon :: T.Text -> T.Text
advicePutNoCon a = T.concat [ "Please also specify where you want to put it, as in "
                            , quoteColor
                            , "put "
                            , a
                            , " sack"
                            , dfltColor
                            , "." ]


adviceReadyNoArgs :: T.Text
adviceReadyNoArgs = T.concat [ "Please specify one or more items to ready, as in "
                             , quoteColor
                             , "ready sword"
                             , dfltColor
                             , "." ]


adviceRemoveNoArgs :: T.Text
adviceRemoveNoArgs = T.concat [ "Please specify one or more items to remove followed by the container you want to \
                                \remove them from, as in "
                              , quoteColor
                              , "remove doll sack"
                              , dfltColor
                              , "." ]


adviceRemoveNoCon :: T.Text -> T.Text
adviceRemoveNoCon a = T.concat [ "Please also specify the container you want to remove it from, as in "
                               , quoteColor
                               , "remove "
                               , a
                               , " sack"
                               , dfltColor
                               , "." ]


adviceSayAdverbNoUtterance :: T.Text
adviceSayAdverbNoUtterance = "Please also specify what you'd like to say" <> adverbExample


adviceSayNoArgs :: T.Text
adviceSayNoArgs = T.concat [ "Please specify what you'd like to say, as in "
                           , quoteColor
                           , "say nice to meet you, too"
                           , dfltColor
                           , "." ]


adviceSayToNoUtterance :: T.Text
adviceSayToNoUtterance  = T.concat [ "Please also specify what you'd like to say, as in "
                                   , quoteColor
                                   , "say "
                                   , T.singleton sayToChar
                                   , "taro nice to meet you, too"
                                   , dfltColor
                                   , "." ]


adviceSettingsInvalid :: T.Text
adviceSettingsInvalid = T.concat [ " Please specify the setting you want to change, followed immediately by "
                                 , dblQuote "="
                                 , ", followed immediately by the new value you want to assign, as in "
                                 , quoteColor
                                 , "set columns=80"
                                 , dfltColor
                                 , "." ]


adviceShowNoArgs :: T.Text
adviceShowNoArgs = T.concat [ "Please specify one or more items to show followed by the name of a person, as in "
                            , quoteColor
                            , "show ring taro"
                            , dfltColor
                            , "." ]


adviceShowNoName :: T.Text -> T.Text
adviceShowNoName a = T.concat [ "Please also provide the name of a person, as in "
                              , quoteColor
                              , "show "
                              , a
                              , " taro"
                              , dfltColor
                              , "." ]


adviceTeleNoArgs :: T.Text
adviceTeleNoArgs = T.concat [ "Please provide the name of a person followed by a message to send, as in "
                            , quoteColor
                            , "telepathy taro i'll meet you there in a few"
                            , dfltColor
                            , "." ]


adviceTeleNoMsg :: T.Text -> T.Text
adviceTeleNoMsg a = T.concat [ "Please also provide a message to send, as in "
                             , quoteColor
                             , "telepathy "
                             , a
                             , " i'll meet you there in a few"
                             , dfltColor
                             , "." ]


adviceTuneInvalid :: T.Text
adviceTuneInvalid = T.concat [ " Please specify the name of the connection you want to tune, followed immediately by "
                             , dblQuote "="
                             , ", followed immediately by "
                             , inOutOrOnOff
                             , ", as in "
                             , quoteColor
                             , "tune taro=in"
                             , dfltColor
                             , "." ]


adviceTypoNoArgs :: T.Text
adviceTypoNoArgs = T.concat [ "Please describe the typo you've found, as in "
                            , quoteColor
                            , "typo 'accross from the fireplace' should be 'across from the fireplace'"
                            , dfltColor
                            , "." ]


adviceUnlinkNoArgs :: T.Text
adviceUnlinkNoArgs = T.concat [ "Please provide the full name of the person with whom you would like to unlink, as in "
                              , quoteColor
                              , "unlink taro"
                              , dfltColor
                              , "." ]


adviceUnreadyNoArgs :: T.Text
adviceUnreadyNoArgs = T.concat [ "Please specify one or more items to unready, as in "
                               , quoteColor
                               , "unready sword"
                               , dfltColor
                               , "." ]


adviceYouEmote :: T.Text
adviceYouEmote = T.concat [ "Sorry, but you can't use a form of the word "
                          , dblQuote "you"
                          , " in an emote. Instead, you must specify who you wish to target using "
                          , dblQuote etc
                          , ", as in "
                          , quoteColor
                          , "emote slowly turns her head to look directly at "
                          , etc
                          , "taro"
                          , dfltColor
                          , "." ]


adviceYouEmoteChar :: T.Text -> T.Text
adviceYouEmoteChar cn = T.concat [ "Sorry, but you can't use a form of the word "
                                 , dblQuote "you"
                                 , " in an emote. Instead, you must specify who you wish to target using "
                                 , dblQuote etc
                                 , ", as in "
                                 , quoteColor
                                 , cn
                                 , " "
                                 , T.singleton emoteChar
                                 , "slowly turns her head to look directly at "
                                 , etc
                                 , "taro"
                                 , dfltColor
                                 , "." ]
