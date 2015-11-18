{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Cmds.Msgs.Advice ( adviceAAnnounceNoArgs
                            , adviceABanHostNoReason
                            , adviceABanPlaNoReason
                            , adviceABootNoArgs
                            , adviceAdminNoMsg
                            , adviceAdverbCloseChar
                            , adviceAExamineNoArgs
                            , adviceAHostNoArgs
                            , adviceALocateNoArgs
                            , adviceAMsgNoArgs
                            , adviceAMsgNoMsg
                            , adviceAMyChansNoArgs
                            , adviceAPeepNoArgs
                            , adviceAPrintNoArgs
                            , adviceASearchNoArgs
                            , adviceASudoerExcessArgs
                            , adviceASudoerNoArgs
                            , adviceATeleIdExcessArgs
                            , adviceATeleIdNoArgs
                            , adviceATelePCExcessArgs
                            , adviceATelePCNoArgs
                            , adviceATeleRmExcessArgs
                            , adviceAWireNoArgs
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
                            , adviceDRegenExcessArgs
                            , adviceDRegenNoArgs
                            , adviceDRntExcessArgs
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
                            , adviceQuitExcessArgs
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
advise (Advising mq cols) [h] msg = multiWrapSend mq cols [ msg, "For more information, type "       <>
                                                                 colorWith quoteColor ("help " <> h) <>
                                                                 "." ]
advise (Advising mq cols) (dblQuote . T.intercalate (dblQuote ", ") -> helpTopics) msg =
    multiWrapSend mq cols [ msg, "For more information, see the following help articles: " <> helpTopics <> "." ]
advise p hs msg = patternMatchFail "advise" [ showText p, showText hs, msg ]


-----


adviceAAnnounceNoArgs :: T.Text
adviceAAnnounceNoArgs = let msg = "CurryMUD will be shutting down for maintenance in 30 minutes" in
    "You must provide a message to send, as in "                   <>
    colorWith quoteColor (prefixAdminCmd "announce" <> " " <> msg) <>
    "."


adviceABanHostNoReason :: T.Text -> T.Text
adviceABanHostNoReason a = "Please also provide a reason, as in "                                   <>
                           colorWith quoteColor (prefixAdminCmd "banhost " <> a <> " used by Taro") <>
                           "."


adviceABanPlaNoReason :: T.Text -> T.Text
adviceABanPlaNoReason a = "Please also provide a reason, as in "                                             <>
                          colorWith quoteColor (prefixAdminCmd "banplayer " <> a <> " for harassing hanako") <>
                          "."


adviceABootNoArgs :: T.Text
adviceABootNoArgs = "Please specify the full PC name of the player you wish to boot, optionally followed by a custom \
                    \message."


adviceAExamineNoArgs :: T.Text
adviceAExamineNoArgs = "Please provide one or more IDs to examine."


adviceAHostNoArgs :: T.Text
adviceAHostNoArgs = "Please specify the PC names of one or more players whose host statistics you would like to see."


adviceALocateNoArgs :: T.Text
adviceALocateNoArgs = "Please provide one or more IDs to locate."


adviceAMsgNoArgs :: T.Text
adviceAMsgNoArgs =
    "Please specify the PC name of a regular player followed by a message, as in "                       <>
    colorWith quoteColor (prefixAdminCmd "message" <> " taro thank you for reporting the bug you found") <>
    "."


adviceAMsgNoMsg :: T.Text -> T.Text
adviceAMsgNoMsg a =
    "Please also provide a message to send, as in "                                                       <>
    colorWith quoteColor (prefixAdminCmd "message " <> a <> " thank you for reporting the bug you found") <>
    "."


adviceAMyChansNoArgs :: T.Text
adviceAMyChansNoArgs = "Please specify the PC names of one or more players whose channel information you'd like to see."


adviceAPeepNoArgs :: T.Text
adviceAPeepNoArgs = "Please specify the PC names of one or more players you wish to start or stop peeping."


adviceAPrintNoArgs :: T.Text
adviceAPrintNoArgs = "You must provide a message to print to the server console, as in "  <>
                     colorWith quoteColor (prefixAdminCmd "print" <> " is anybody home?") <>
                     "."


adviceASearchNoArgs :: T.Text
adviceASearchNoArgs = "Please provide a regular expression to search for."


adviceASudoerExcessArgs :: T.Text
adviceASudoerExcessArgs = "Sorry, but you can only promote/demote one player at a time."


adviceASudoerNoArgs :: T.Text
adviceASudoerNoArgs = "Please specify the full PC name of the player you wish to promote/demote."


adviceATeleIdExcessArgs :: T.Text
adviceATeleIdExcessArgs = "You can only teleport to one entity or room at a time."


adviceATeleIdNoArgs :: T.Text
adviceATeleIdNoArgs = "Please specify the ID of the entity or room to which you want to teleport."


adviceATelePCExcessArgs :: T.Text
adviceATelePCExcessArgs = "You can only teleport to one PC at a time."


adviceATelePCNoArgs :: T.Text
adviceATelePCNoArgs = "Please specify the name of the PC to which you want to teleport."


adviceATeleRmExcessArgs :: T.Text
adviceATeleRmExcessArgs = "You can only teleport to one room at a time."


adviceAWireNoArgs :: T.Text
adviceAWireNoArgs = "Please specify the IDs of one or more telepathic channels you wish to start or stop tapping."


-----


adviceDCinsExcessArgs :: T.Text
adviceDCinsExcessArgs = "Please provide one argument: the target ID, as in "   <>
                        colorWith quoteColor (prefixDebugCmd "cins" <> " 100") <>
                        "."


adviceDCinsNoArgs :: T.Text
adviceDCinsNoArgs = adviceDCinsExcessArgs


adviceDIdExcessArgs :: T.Text
adviceDIdExcessArgs = "Please provide one argument: the ID to search for, as in " <>
                      colorWith quoteColor (prefixDebugCmd "id" <> " 100")        <>
                      "."


adviceDIdNoArgs :: T.Text
adviceDIdNoArgs = adviceDIdExcessArgs


adviceDNumberExcessArgs :: T.Text
adviceDNumberExcessArgs = "Please provide two arguments: a number and its base, as in " <>
                          colorWith quoteColor (prefixDebugCmd "number" <> " a 16")     <>
                          "."


adviceDNumberNoArgs :: T.Text
adviceDNumberNoArgs = adviceDNumberExcessArgs


adviceDNumberNoBase :: T.Text
adviceDNumberNoBase = "Please also specify base, as in "                        <>
                      colorWith quoteColor (prefixDebugCmd "number" <> " a 16") <>
                      "."


adviceDRegenExcessArgs :: T.Text
adviceDRegenExcessArgs = "Please provide one argument: the target ID, as in "    <>
                         colorWith quoteColor (prefixDebugCmd "regen" <> " 100") <>
                         "."


adviceDRegenNoArgs :: T.Text
adviceDRegenNoArgs = adviceDRegenExcessArgs


adviceDRntExcessArgs :: T.Text
adviceDRntExcessArgs = "Sorry, but you can only generate a random name for one PC at a time."


adviceDWeightExcessArgs :: T.Text
adviceDWeightExcessArgs =
    "Please provide one argument: the ID for which you would like to calculate weight, as in " <>
    quoteWith quoteColor (prefixDebugCmd "weight" <> " 100")                                   <>
    "."


adviceDWeightNoArgs :: T.Text
adviceDWeightNoArgs = adviceDWeightExcessArgs


adviceDWrapExcessArgs :: T.Text
adviceDWrapExcessArgs = "Please provide one argument: line length, as in "    <>
                        colorWith quoteColor (prefixDebugCmd "wrap" <> " 40") <>
                        "."


adviceDWrapNoArgs :: T.Text
adviceDWrapNoArgs = adviceDWrapExcessArgs


adviceDWrapIndentExcessArgs :: T.Text
adviceDWrapIndentExcessArgs = "Please provide two arguments: line length and indent amount, as in " <>
                              colorWith quoteColor (prefixDebugCmd "wrapindent" <> " 40 4")         <>
                              "."


adviceDWrapIndentNoAmt :: T.Text
adviceDWrapIndentNoAmt = "Please also specify indent amount, as in "                   <>
                         colorWith quoteColor (prefixDebugCmd "wrapindent" <> " 40 4") <>
                         "."


adviceDWrapIndentNoArgs :: T.Text
adviceDWrapIndentNoArgs = adviceDWrapIndentExcessArgs


-----


adviceAdminNoMsg :: T.Text -> T.Text
adviceAdminNoMsg a = "Please also provide a message to send, as in "                                      <>
                     colorWith quoteColor ("admin " <> a <> " are you available? I need your assistance") <>
                     "."


adviceAdverbCloseChar :: T.Text
adviceAdverbCloseChar = "An adverbial phrase must be terminated with a " <> dblQuote acl <> adverbExample


adverbExample :: T.Text
adverbExample = ", as in "                                                     <>
                colorWith quoteColor ("say "                                   <>
                                      quoteWith' (aop, acl) "enthusiastically" <>
                                      " nice to meet you, too")                <>
                "."


adviceBugNoArgs :: T.Text
adviceBugNoArgs = "Please describe the bug you've found, as in "             <>
                  colorWith quoteColor "bug i've fallen and I can't get up!" <>
                  "."


adviceConnectNoArgs :: T.Text
adviceConnectNoArgs =
    "Please specify the names of one or more people followed by the name of a telepathic channel to connect them to, \
    \as in "                                 <>
    colorWith quoteColor "connect taro hunt" <>
    "."


adviceConnectNoChan :: T.Text -> T.Text
adviceConnectNoChan a = "Please also specify the name of a telepathic channel, as in " <>
                        colorWith quoteColor ("connect " <> a <> " hunt")              <>
                        "."


adviceDisconnectNoArgs :: T.Text
adviceDisconnectNoArgs =
    "Please provide the full names of one or more people followed by the name of a telepathic channel to disconnect \
    \them from, as in "                         <>
    colorWith quoteColor "disconnect taro hunt" <>
    "."


adviceDisconnectNoChan :: T.Text -> T.Text
adviceDisconnectNoChan a = "Please also provide the name of a telepathic channel, as in " <>
                           colorWith quoteColor ("disconnect " <> a <> " hunt")           <>
                           "."


adviceDropNoArgs :: T.Text
adviceDropNoArgs = "Please specify one or more items to drop, as in " <>
                   colorWith quoteColor "drop sword"                  <>
                   "."


adviceEmoteNoArgs :: T.Text
adviceEmoteNoArgs = "Please provide a description of an action, as in "                         <>
                    colorWith quoteColor "emote laughs with relief as tears roll down her face" <>
                    "."


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
                        , colorWith quoteColor . T.concat $ [ cn
                                                            , "shielding her eyes from the sun, "
                                                            , enc
                                                            , " looks out across the plains" ]
                        , ", or "
                        , colorWith quoteColor $ cn <> enc <> "'s leg twitches involuntarily as she laughs with gusto"
                        , "." ]


adviceEtc :: T.Text -> T.Text
adviceEtc cn = T.concat [ dblQuote etc
                        , " must be immediately followed by the name of the person you wish to target, as in "
                        , colorWith quoteColor . T.concat $ [ cn
                                                            , "slowly turns her head to look directly at "
                                                            , etc
                                                            , "taro" ]
                        , ". To create a possessive noun, append "
                        , dblQuote "'s"
                        , " to the target name, as in "
                        , colorWith quoteColor . T.concat $ [ cn
                                                            , "places her hand firmly on "
                                                            , etc
                                                            , "taro's shoulder" ]
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
                                    , colorWith quoteColor . T.concat $ [ cn'
                                                                        , "gives "
                                                                        , etc
                                                                        , "hanako a smooch!" ]
                                    , ", you should type "
                                    , colorWith quoteColor $ cn' <> "gives you a smooch!"
                                    , "." ]


adviceExpCmdExcessArgs :: T.Text
adviceExpCmdExcessArgs = "Sorry, but you can only target one person at a time with expressive commands."


adviceGetNoArgs :: T.Text
adviceGetNoArgs = "Please specify one or more items to pick up, as in " <>
                  colorWith quoteColor "get sword"                      <>
                  "."


adviceLeaveNoArgs :: T.Text
adviceLeaveNoArgs = "Please specify the names of one or more channels to leave, as in " <>
                    colorWith quoteColor "leave hunt"                                   <>
                    "."


adviceNewChanNoArgs :: T.Text
adviceNewChanNoArgs = "Please specify one or more new channel names, as in " <>
                      colorWith quoteColor "newchannel hunt"                 <>
                      "."


advicePutNoArgs :: T.Text
advicePutNoArgs =
    "Please specify one or more items you want to put followed by where you want to put them, as in " <>
    colorWith quoteColor "put doll sack"                                                              <>
    "."


advicePutNoCon :: T.Text -> T.Text
advicePutNoCon a = "Please also specify where you want to put it, as in " <>
                   colorWith quoteColor ("put " <> a <> " sack")          <>
                   "."


adviceQuitExcessArgs :: T.Text
adviceQuitExcessArgs = "Type "                     <>
                       colorWith quoteColor "quit" <>
                       " with no arguments to quit CurryMUD."


adviceReadyNoArgs :: T.Text
adviceReadyNoArgs = "Please specify one or more items to ready, as in " <>
                    quoteWith quoteColor "ready sword"                  <>
                    "."


adviceRemoveNoArgs :: T.Text
adviceRemoveNoArgs =
    "Please specify one or more items to remove followed by the container you want to remove them from, as in " <>
    colorWith quoteColor "remove doll sack"                                                                     <>
    "."


adviceRemoveNoCon :: T.Text -> T.Text
adviceRemoveNoCon a = "Please also specify the container you want to remove it from, as in " <>
                      colorWith quoteColor ("remove " <> a <> " sack")                       <>
                      "."


adviceSayAdverbNoUtterance :: T.Text
adviceSayAdverbNoUtterance = "Please also specify what you'd like to say" <> adverbExample


adviceSayNoArgs :: T.Text
adviceSayNoArgs = "Please specify what you'd like to say, as in "  <>
                  colorWith quoteColor "say nice to meet you, too" <>
                  "."


adviceSayToNoUtterance :: T.Text
adviceSayToNoUtterance  = "Please also specify what you'd like to say, as in "                                   <>
                          colorWith quoteColor ("say " <> T.singleton sayToChar <> "taro nice to meet you, too") <>
                          "."


adviceSettingsInvalid :: T.Text
adviceSettingsInvalid = T.concat [ " Please specify the setting you want to change, followed immediately by "
                                 , dblQuote "="
                                 , ", followed immediately by the new value you want to assign, as in "
                                 , colorWith quoteColor "set columns=80"
                                 , "." ]


adviceShowNoArgs :: T.Text
adviceShowNoArgs = "Please specify one or more items to show followed by the name of a person, as in " <>
                   colorWith quoteColor "show ring taro"                                               <>
                   "."


adviceShowNoName :: T.Text -> T.Text
adviceShowNoName a = "Please also provide the name of a person, as in " <>
                     colorWith quoteColor ("show " <> a <> " taro")     <>
                     "."


adviceTeleNoArgs :: T.Text
adviceTeleNoArgs = "Please provide the name of a person followed by a message to send, as in " <>
                   colorWith quoteColor "telepathy taro i'll meet you there in a few"          <>
                   "."


adviceTeleNoMsg :: T.Text -> T.Text
adviceTeleNoMsg a = "Please also provide a message to send, as in "                              <>
                    colorWith quoteColor ("telepathy " <> a <>  " i'll meet you there in a few") <>
                    "."


adviceTuneInvalid :: T.Text
adviceTuneInvalid = T.concat [ " Please specify the name of the connection you want to tune, followed immediately by "
                             , dblQuote "="
                             , ", followed immediately by "
                             , inOutOrOnOff
                             , ", as in "
                             , colorWith quoteColor "tune taro=in"
                             , "." ]
  where
    inOutOrOnOff = T.concat [ dblQuote "in"
                            , "/"
                            , dblQuote "out"
                            , " or "
                            , dblQuote "on"
                            , "/"
                            , dblQuote "off" ]


adviceTypoNoArgs :: T.Text
adviceTypoNoArgs = "Please describe the typo you've found, as in "                                                <>
                   colorWith quoteColor "typo 'accross from the fireplace' should be 'across from the fireplace'" <>
                   "."


adviceUnlinkNoArgs :: T.Text
adviceUnlinkNoArgs = "Please provide the full name of the person with whom you would like to unlink, as in " <>
                     colorWith quoteColor "unlink taro"                                                      <>
                     "."


adviceUnreadyNoArgs :: T.Text
adviceUnreadyNoArgs = "Please specify one or more items to unready, as in " <>
                      colorWith quoteColor "unready sword"                  <>
                      "."


adviceYouEmote :: T.Text
adviceYouEmote =
    T.concat [ "Sorry, but you can't use a form of the word "
             , dblQuote "you"
             , " in an emote. Instead, you must specify who you wish to target using "
             , dblQuote etc
             , ", as in "
             , colorWith quoteColor ("emote slowly turns her head to look directly at " <> etc <> "taro" )
             , "." ]


adviceYouEmoteChar :: T.Text -> T.Text
adviceYouEmoteChar cn = T.concat [ "Sorry, but you can't use a form of the word "
                                 , dblQuote "you"
                                 , " in an emote. Instead, you must specify who you wish to target using "
                                 , dblQuote etc
                                 , ", as in "
                                 , colorWith quoteColor . T.concat $ [ cn
                                                                     , " "
                                                                     , T.singleton emoteChar
                                                                     , "slowly turns her head to look directly at "
                                                                     , etc
                                                                     , "taro" ]
                                 , "." ]
