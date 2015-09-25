{-# LANGUAGE ViewPatterns #-}

module Mud.Cmds.Util.Advice ( -- TODO: Sort.
                              adviceAdminNoMsg
                            , adviceBugNoDesc
                            , adviceConnectNoArgs
                            , adviceConnectNoChan
                            , adviceDropNoArgs
                            , adviceEmoteNoDesc
                            , adviceEnc
                            , adviceEtc
                            , adviceEtcEmptyPoss
                            , adviceEtcHead
                            , adviceGetNoArgs
                            , adviceLeaveNoChans
                            , adviceNewChanNoNames
                            , advicePutNoArgs
                            , advicePutNoCon
                            , adviceReadyNoArgs
                            , adviceRemoveNoArgs
                            , adviceRemoveNoCon
                            , adviceSayNoArgs
                            , adviceYouEmote
                            , adviceYouEmoteChar
                            , advise ) where

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


adviceAdminNoMsg :: T.Text -> T.Text
adviceAdminNoMsg a = T.concat [ "Please also provide a message to send, as in "
                              , quoteColor
                              , "admin "
                              , a
                              , " are you available? I need your assistance"
                              , dfltColor
                              , "." ]

adviceBugNoDesc :: T.Text
adviceBugNoDesc = T.concat [ "Please describe the bug you've found, as in "
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


adviceDropNoArgs :: T.Text
adviceDropNoArgs = T.concat [ "Please specify one or more items to drop, as in "
                            , quoteColor
                            , "drop sword"
                            , dfltColor
                            , "." ]


adviceEmoteNoDesc :: T.Text
adviceEmoteNoDesc = T.concat [ "Please provide a description of an action, as in "
                             , quoteColor
                             , "emote laughs with relief as tears roll down her face"
                             , dfltColor
                             , "." ]


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


adviceGetNoArgs :: T.Text
adviceGetNoArgs = T.concat [ "Please specify one or more items to pick up, as in "
                           , quoteColor
                           , "get sword"
                           , dfltColor
                           , "." ]


adviceLeaveNoChans :: T.Text
adviceLeaveNoChans = T.concat [ "Please specify the names of one or more channels to leave, as in "
                              , quoteColor
                              , "leave hunt"
                              , dfltColor
                              , "." ]


adviceNewChanNoNames :: T.Text
adviceNewChanNoNames = T.concat [ "Please specify one or more new channel names, as in "
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


adviceSayNoArgs :: T.Text
adviceSayNoArgs = T.concat [ "Please specify what you'd like to say, as in "
                           , quoteColor
                           , "say nice to meet you, too"
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
