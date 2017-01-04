{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Cmds.Msgs.Advice ( adviceAAnnounceNoArgs
                            , adviceAAsNoArgs
                            , adviceAAsNoCmd
                            , adviceABanHostNoReason
                            , adviceABanPCNoReason
                            , adviceABootNoArgs
                            , adviceAdminNoMsg
                            , adviceAdverbCloseChar
                            , adviceAExamineNoArgs
                            , adviceAFarewellNoArgs
                            , adviceAHashExcessArgs
                            , adviceAHashNoArgs
                            , adviceAHashNoHash
                            , adviceAHostNoArgs
                            , adviceAKillNoArgs
                            , adviceALinkNoArgs
                            , adviceALocateNoArgs
                            , adviceAMsgNoArgs
                            , adviceAMsgNoMsg
                            , adviceAMyChansNoArgs
                            , adviceAPasswordExcessArgs
                            , adviceAPasswordNoArgs
                            , adviceAPasswordNoPw
                            , adviceAPeepNoArgs
                            , adviceAPossessExcessArgs
                            , adviceAPossessNoArgs
                            , adviceAPrintNoArgs
                            , adviceASearchNoArgs
                            , adviceASecurityNoArgs
                            , adviceASetInvalid
                            , adviceASetNoArgs
                            , adviceASetNoSettings
                            , adviceAsSelfNoArgs
                            , adviceASudoerExcessArgs
                            , adviceASudoerNoArgs
                            , adviceASummonExcessArgs
                            , adviceASummonNoArgs
                            , adviceATeleIdExcessArgs
                            , adviceATeleIdNoArgs
                            , adviceATelePCExcessArgs
                            , adviceATelePCNoArgs
                            , adviceATeleRmExcessArgs
                            , adviceAWireNoArgs
                            , adviceBlankAdverb
                            , adviceBonusExcessArgs
                            , adviceBonusNoArgs
                            , adviceBugNoArgs
                            , adviceConnectNoArgs
                            , adviceConnectNoChan
                            , adviceDBonusExcessArgs
                            , adviceDBonusNoArgs
                            , adviceDCinsExcessArgs
                            , adviceDCinsNoArgs
                            , adviceDCurryTimeExcessArgs
                            , adviceDIdExcessArgs
                            , adviceDIdNoArgs
                            , adviceDisconnectNoArgs
                            , adviceDisconnectNoChan
                            , adviceDLiqExcessArgs
                            , adviceDLiqNoArgs
                            , adviceDLiqNoId
                            , adviceDNumberExcessArgs
                            , adviceDNumberNoArgs
                            , adviceDNumberNoBase
                            , adviceDRegenExcessArgs
                            , adviceDRegenNoArgs
                            , adviceDrinkExcessArgs
                            , adviceDrinkNoArgs
                            , adviceDrinkNoVessel
                            , adviceDRntExcessArgs
                            , adviceDropNoArgs
                            , adviceDVolumeExcessArgs
                            , adviceDVolumeNoArgs
                            , adviceDWeightExcessArgs
                            , adviceDWeightNoArgs
                            , adviceDWrapExcessArgs
                            , adviceDWrapIndentExcessArgs
                            , adviceDWrapIndentNoAmt
                            , adviceDWrapIndentNoArgs
                            , adviceDWrapNoArgs
                            , adviceEmoteNoArgs
                            , adviceEmptyNoArgs
                            , adviceEnc
                            , adviceEtc
                            , adviceEtcBlankPoss
                            , adviceEtcHead
                            , adviceEtcInTwoWay
                            , adviceExpCmdExcessArgs
                            , adviceFillNoArgs
                            , adviceFillNoSource
                            , adviceGetNoArgs
                            , adviceGiveNoArgs
                            , adviceGiveNoName
                            , adviceLeaveNoArgs
                            , adviceNewChanNoArgs
                            , advicePickNoArgs
                            , advicePutNoArgs
                            , advicePutNoCon
                            , adviceQuitExcessArgs
                            , adviceReadNoArgs
                            , adviceReadyNoArgs
                            , adviceRemoveNoArgs
                            , adviceRemoveNoCon
                            , adviceSayAdverbNoUtterance
                            , adviceSayNoArgs
                            , adviceSayToNoUtterance
                            , adviceSettingsInvalid
                            , adviceShowNoArgs
                            , adviceShowNoName
                            , adviceSmellExcessArgs
                            , adviceStopExcessArgs
                            , adviceTasteExcessArgs
                            , adviceTasteNoArgs
                            , adviceTeleNoArgs
                            , adviceTeleNoMsg
                            , adviceTrashNoArgs
                            , adviceTuneInvalid
                            , adviceTypoNoArgs
                            , adviceUnlinkNoArgs
                            , adviceUnreadyNoArgs
                            , adviceWhisperNoArgs
                            , adviceWhisperNoMsg
                            , adviceYouEmote
                            , adviceYouEmoteChar
                            , adviceZoomExcessArgs
                            , advise ) where

import Mud.Cmds.Util.CmdPrefixes
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Lang
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc (PatternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Cmds.Msgs.Advice"


-- ==================================================


advise :: ActionParams -> [HelpName] -> Text -> MudStack ()
advise (Advising mq cols) []  msg = wrapSend mq cols msg
advise (Advising mq cols) [h] msg = multiWrapSend mq cols [ msg
                                                          , prd $ "For more information, type "<>
                                                                  colorWith quoteColor ("help " <> h) ]
advise (Advising mq cols) (dblQuote . T.intercalate (dblQuote ", ") -> helpTopics) msg =
    multiWrapSend mq cols [ msg, prd $ "For more information, see the following help articles: " <> helpTopics ]
advise p _ _ = patternMatchFail "advise" . showText $ p


-----


adviceAAnnounceNoArgs :: Text
adviceAAnnounceNoArgs = let msg = "CurryMUD will be shutting down for maintenance in 30 minutes"
                        in prd $ "You must provide a message to send, as in " <>
                                 colorWith quoteColor (prefixAdminCmd "announce" |<>| msg)


adviceAAsNoArgs :: Text
adviceAAsNoArgs = prd $ "Please specify an ID followed by a command, as in " <> adviceAAsEx


adviceAAsEx :: Text
adviceAAsEx = colorWith quoteColor $ prefixAdminCmd "as" <> " 100 get sack"


adviceAAsNoCmd :: Text
adviceAAsNoCmd = prd $ "Please also provide a command, as in " <> adviceAAsEx


adviceABanHostNoReason :: Text
adviceABanHostNoReason = prd $ "Please also provide a reason, as in " <>
                               colorWith quoteColor (prefixAdminCmd "banhost" <> " 127.0.0.1 used by Taro")


adviceABanPCNoReason :: Text
adviceABanPCNoReason = prd $ "Please also provide a reason, as in " <>
                             colorWith quoteColor (prefixAdminCmd "banpc" <> " taro for harassment")


adviceABootNoArgs :: Text
adviceABootNoArgs = "Please specify the full PC name of the player you wish to boot, optionally followed by a custom \
                    \message."


adviceAExamineNoArgs :: Text
adviceAExamineNoArgs = "Please provide one or more IDs to examine."


adviceAFarewellNoArgs :: Text
adviceAFarewellNoArgs = "Please specify the names of one or more PCs whose farewell stats you would like to see."


adviceAHashExcessArgs :: Text
adviceAHashExcessArgs = adviceAHashNoArgs


adviceAHashNoArgs :: Text
adviceAHashNoArgs = prd $ "Please provide two arguments: a plain-text password, followed by a hashed password, as in " <>
                          adviceAHashEx


adviceAHashEx :: Text
adviceAHashEx = colorWith quoteColor $ prefixAdminCmd "hash" |<>| t
  where
    t = "curry $2y$04$nbLFBcaGtmT.fMzBUC.sC.vj0AqQgTE6R//Nj70DKU/fN5W2K84Sm"


adviceAHashNoHash :: Text
adviceAHashNoHash = prd $ "Please also provide a hashed password, as in " <> adviceAHashEx


adviceAHostNoArgs :: Text
adviceAHostNoArgs = "Please specify the PC names of one or more players whose host statistics you would like to see."


adviceAKillNoArgs :: Text
adviceAKillNoArgs = "Please provide one or more IDs to kill."


adviceALinkNoArgs :: Text
adviceALinkNoArgs = "Please specify the names of one or more PCs whose two-way links you would like to see."


adviceALocateNoArgs :: Text
adviceALocateNoArgs = "Please provide one or more IDs to locate."


adviceAMsgNoArgs :: Text
adviceAMsgNoArgs = prd $ "Please specify the PC name of a regular player followed by a message, as in " <> adviceAMsgEx


adviceAMsgEx :: Text
adviceAMsgEx = colorWith quoteColor $ prefixAdminCmd "message" <> " taro thank you for reporting the bug you found"


adviceAMsgNoMsg :: Text
adviceAMsgNoMsg = prd $ "Please also provide a message to send, as in " <> adviceAMsgEx


adviceAMyChansNoArgs :: Text
adviceAMyChansNoArgs = "Please specify the PC names of one or more players whose channel information you'd like to see."


adviceAPasswordExcessArgs :: Text
adviceAPasswordExcessArgs = prd $ "Please provide two arguments: the full PC name of the player whose password you \
                                  \wish to change, followed by a new password, as in " <>
                                  adviceAPasswordEx


adviceAPasswordEx :: Text
adviceAPasswordEx = colorWith quoteColor $ prefixAdminCmd "password" <> " taro Aoeui1"


adviceAPasswordNoArgs :: Text
adviceAPasswordNoArgs = prd $ "Please specify the full PC name of the player whose password you wish to change, \
                              \followed by a new password, as in " <>
                              adviceAPasswordEx


adviceAPasswordNoPw :: Text
adviceAPasswordNoPw = prd $ "Please also provide a new password, as in " <> adviceAPasswordEx


adviceAPeepNoArgs :: Text
adviceAPeepNoArgs = "Please specify the PC names of one or more players you wish to start or stop peeping."


adviceAPossessExcessArgs :: Text
adviceAPossessExcessArgs = "You can only possess one NPC at a time."


adviceAPossessNoArgs :: Text
adviceAPossessNoArgs = "Please specify the ID of the NPC you wish to possess."


adviceAPrintNoArgs :: Text
adviceAPrintNoArgs = prd $ "Please provide a message to print to the server console, as in " <>
                           colorWith quoteColor (prefixAdminCmd "print" <> " is anybody home?")


adviceASearchNoArgs :: Text
adviceASearchNoArgs = "Please provide a regular expression to search for."


adviceASecurityNoArgs :: Text
adviceASecurityNoArgs = "Please specify the PC names of one or more players whose security Q&A you would like to see."


adviceASetInvalid :: Text
adviceASetInvalid = T.concat [ "Please specify the key you want to change, followed immediately by "
                             , dblQuote "="
                             , ", followed immediately by the new value you want to assign, as in "
                             , adviceASetEx
                             , "." ]


adviceASetEx :: Text
adviceASetEx = colorWith quoteColor $ prefixAdminCmd "set" <> " 100 curhp=50"


adviceASetNoArgs :: Text
adviceASetNoArgs = prd $ "Please specify a target ID followed by one or more key/value pairs, as in " <> adviceASetEx


adviceASetNoSettings :: Text
adviceASetNoSettings = prd $ "Please also specify one or more key/value pairs, as in " <> adviceASetEx


adviceASudoerExcessArgs :: Text
adviceASudoerExcessArgs = "Sorry, but you can only promote/demote one player at a time."


adviceASudoerNoArgs :: Text
adviceASudoerNoArgs = "Please specify the full PC name of the player you wish to promote/demote."


adviceASummonExcessArgs :: Text
adviceASummonExcessArgs = "You can only summon one PC at a time."


adviceASummonNoArgs :: Text
adviceASummonNoArgs = "Please specify the name of the PC you wish to summon."


adviceATeleIdExcessArgs :: Text
adviceATeleIdExcessArgs = "You can only teleport to one entity or room at a time."


adviceATeleIdNoArgs :: Text
adviceATeleIdNoArgs = "Please specify the ID of the entity or room to which you wish to teleport."


adviceATelePCExcessArgs :: Text
adviceATelePCExcessArgs = "You can only teleport to one PC at a time."


adviceATelePCNoArgs :: Text
adviceATelePCNoArgs = "Please specify the name of the PC to which you wish to teleport."


adviceATeleRmExcessArgs :: Text
adviceATeleRmExcessArgs = "You can only teleport to one room at a time."


adviceAWireNoArgs :: Text
adviceAWireNoArgs = "Please specify the IDs of one or more telepathic channels you wish to start or stop tapping."


-----


adviceDBonusExcessArgs :: Text
adviceDBonusExcessArgs = prd $ "Please provide one argument: the PC name of the player for which you would like to \
                               \calculate bonus experience, as in " <>
                               colorWith quoteColor (prefixDebugCmd "bonus" <> " hanako")


adviceDBonusNoArgs :: Text
adviceDBonusNoArgs = adviceDBonusExcessArgs


adviceDCinsExcessArgs :: Text
adviceDCinsExcessArgs = prd $ "Please provide one argument: the target ID, as in " <>
                              colorWith quoteColor (prefixDebugCmd "cins" <> " 100")


adviceDCinsNoArgs :: Text
adviceDCinsNoArgs = adviceDCinsExcessArgs


adviceDCurryTimeExcessArgs :: Text
adviceDCurryTimeExcessArgs = prd $ "Please provide one argument: the number of seconds to display in Curry Time, as in " <>
                                   colorWith quoteColor (prefixDebugCmd "currytime" <> " 60")


adviceDIdExcessArgs :: Text
adviceDIdExcessArgs = prd $ "Please provide one argument: the ID to search for, as in " <>
                            colorWith quoteColor (prefixDebugCmd "id" <> " 100")


adviceDIdNoArgs :: Text
adviceDIdNoArgs = adviceDIdExcessArgs


adviceDLiqExcessArgs :: Text
adviceDLiqExcessArgs = prd $ "Please provide two arguments: the amount in mouthfuls and the distinct liquid ID, as in " <>
                             adviceDLiqEx


adviceDLiqEx :: Text
adviceDLiqEx = colorWith quoteColor $ prefixDebugCmd "liquid" <> " 8 100"


adviceDLiqNoArgs :: Text
adviceDLiqNoArgs = adviceDLiqExcessArgs


adviceDLiqNoId :: Text
adviceDLiqNoId = prd $ "Please also specify the distinct liquid ID, as in " <> adviceDLiqEx


adviceDNumberExcessArgs :: Text
adviceDNumberExcessArgs = prd $ "Please provide two arguments: a number and its base, as in " <>
                                colorWith quoteColor (prefixDebugCmd "number" <> " a 16")


adviceDNumberNoArgs :: Text
adviceDNumberNoArgs = adviceDNumberExcessArgs


adviceDNumberNoBase :: Text
adviceDNumberNoBase = prd $ "Please also specify base, as in " <> colorWith quoteColor (prefixDebugCmd "number" <> " a 16")


adviceDRegenExcessArgs :: Text
adviceDRegenExcessArgs = prd $ "Please provide one argument: the target ID, as in " <>
                               colorWith quoteColor (prefixDebugCmd "regen" <> " 100")


adviceDRegenNoArgs :: Text
adviceDRegenNoArgs = adviceDRegenExcessArgs


adviceDRntExcessArgs :: Text
adviceDRntExcessArgs = "Sorry, but you can only generate a random name for one PC at a time."


adviceDVolumeExcessArgs :: Text
adviceDVolumeExcessArgs = prd $ "Please provide one argument: the ID for which you would like to calculate carried \
                                \volume, as in " <>
                                colorWith quoteColor (prefixDebugCmd "volume" <> " 100")


adviceDVolumeNoArgs :: Text
adviceDVolumeNoArgs = adviceDVolumeExcessArgs


adviceDWeightExcessArgs :: Text
adviceDWeightExcessArgs = prd $ "Please provide one argument: the ID for which you would like to calculate weight, as in " <>
                                colorWith quoteColor (prefixDebugCmd "weight" <> " 100")


adviceDWeightNoArgs :: Text
adviceDWeightNoArgs = adviceDWeightExcessArgs


adviceDWrapExcessArgs :: Text
adviceDWrapExcessArgs = prd $ "Please provide one argument: line length, as in " <>
                              colorWith quoteColor (prefixDebugCmd "wrap" <> " 40")


adviceDWrapNoArgs :: Text
adviceDWrapNoArgs = adviceDWrapExcessArgs


adviceDWrapIndentExcessArgs :: Text
adviceDWrapIndentExcessArgs = prd $ "Please provide two arguments: line length and indent amount, as in " <>
                                    adviceDWrapIndentEx


adviceDWrapIndentEx :: Text
adviceDWrapIndentEx = colorWith quoteColor $ prefixDebugCmd "wrapindent" <> " 40 4"


adviceDWrapIndentNoAmt :: Text
adviceDWrapIndentNoAmt = prd $ "Please also specify indent amount, as in " <> adviceDWrapIndentEx


adviceDWrapIndentNoArgs :: Text
adviceDWrapIndentNoArgs = adviceDWrapIndentExcessArgs


-----


adviceAdminNoMsg :: Text
adviceAdminNoMsg = prd $ "Please also provide a message to send, as in " <>
                         colorWith quoteColor "admin jason are you available? I need your assistance"


adviceAdverbCloseChar :: Lang -> Text
adviceAdverbCloseChar l = prd $ "An adverbial phrase must be terminated with a " <> dblQuote acl <> adverbExample l


adverbExample :: Lang -> Text
adverbExample l = ", as in " <> colorWith quoteColor (T.concat [ mkCmdNameForLang l
                                                               , " "
                                                               , quoteWith' (aop, acl) "enthusiastically"
                                                               , " nice to meet you, too" ])


adviceAsSelfNoArgs :: Text
adviceAsSelfNoArgs = prd $ "Please provide a command to execute, as in " <> colorWith quoteColor ". look"


adviceBlankAdverb :: Lang -> Text
adviceBlankAdverb l = T.concat [ "Please provide an adverbial phrase between "
                               , dblQuote aop
                               , " and "
                               , dblQuote acl
                               , adverbExample l
                               , "." ]


adviceBonusExcessArgs :: Text
adviceBonusExcessArgs = "You can only give a bonus to one player at a time."


adviceBonusNoArgs :: Text
adviceBonusNoArgs = prd $ "Please provide the name of the character whose player you would like to give a bonus to, as in " <>
                          colorWith quoteColor "bonus hanako"


adviceBugNoArgs :: Text
adviceBugNoArgs = prd $ "Please describe the bug you've found, as in " <>
                        colorWith quoteColor "bug i've fallen and I can't get up!"


adviceConnectNoArgs :: Text
adviceConnectNoArgs = prd $ "Please specify the names of one or more people followed by the name of a telepathic \
                            \channel to connect them to, as in " <>
                            adviceConnectEx


adviceConnectEx :: Text
adviceConnectEx = colorWith quoteColor "connect taro hunt"


adviceConnectNoChan :: Text
adviceConnectNoChan = prd $ "Please also specify the name of a telepathic channel, as in " <> adviceConnectEx


adviceDisconnectNoArgs :: Text
adviceDisconnectNoArgs = prd $ "Please provide the full names of one or more people followed by the name of a \
                               \telepathic channel to disconnect them from, as in " <>
                               adviceDisconnectEx


adviceDisconnectEx :: Text
adviceDisconnectEx = colorWith quoteColor "disconnect taro hunt"


adviceDisconnectNoChan :: Text
adviceDisconnectNoChan = prd $ "Please also provide the name of a telepathic channel, as in " <> adviceDisconnectEx


adviceDrinkExcessArgs :: Text
adviceDrinkExcessArgs = adviceDrinkNoArgs


adviceDrinkNoArgs :: Text
adviceDrinkNoArgs = prd $ "Please specify how many mouthfuls to drink followed by the vessel to drink from, as in " <>
                          colorWith quoteColor "drink 4 waterskin"


adviceDrinkNoVessel :: Text
adviceDrinkNoVessel = adviceDrinkNoArgs


adviceDropNoArgs :: Text
adviceDropNoArgs = prd $ "Please specify one or more items to drop, as in " <>
                         colorWith quoteColor "drop sword"


adviceEmoteNoArgs :: Text
adviceEmoteNoArgs = prd $ "Please provide a description of an action, as in " <>
                          colorWith quoteColor "emote laughs with relief as tears roll down her face"


adviceEmptyNoArgs :: Text
adviceEmptyNoArgs = prd $ "Please specify one or more vessels to empty, as in " <>
                          colorWith quoteColor "empty waterskin"


adviceEnc :: Text -> Text
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


adviceEtc :: Text -> Text
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


adviceEtcBlankPoss :: Text
adviceEtcBlankPoss = T.concat [ "You must specify the name of the person you want to target between "
                              , dblQuote etc
                              , " and "
                              , dblQuote "'s"
                              , "." ]


adviceEtcHead :: Text
adviceEtcHead = "You can't begin an emote with a target."


adviceEtcInTwoWay :: Text -> Text -> Text
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


adviceExpCmdExcessArgs :: Text
adviceExpCmdExcessArgs = "Sorry, but you can only target one person at a time with expressive commands."


adviceFillNoArgs :: Text
adviceFillNoArgs = adviceFillHelper "Please specify one or more vessels to fill followed by"


adviceFillHelper :: Text -> Text
adviceFillHelper t = T.concat [ t
                              , " the name of a) another vessel, or b) a source of liquid in your current room, as in "
                              , colorWith quoteColor "fill waterskin jug"
                              , " "
                              , parensQuote "to fill your waterskin with the contents of your jug"
                              , "." ]


adviceFillNoSource :: Text
adviceFillNoSource = adviceFillHelper "Please also provide"


adviceGetNoArgs :: Text
adviceGetNoArgs = prd $ "Please specify one or more items to pick up, as in " <>
                        colorWith quoteColor "get sword"


adviceGiveNoArgs :: Text
adviceGiveNoArgs = prd $ "Please specify one or more items to give followed by the name of a person, as in " <> adviceGiveEx


adviceGiveEx :: Text
adviceGiveEx = colorWith quoteColor "give ring taro"


adviceGiveNoName :: Text
adviceGiveNoName = prd $ "Please also provide the name of a person, as in " <> adviceGiveEx


adviceLeaveNoArgs :: Text
adviceLeaveNoArgs = prd $ "Please specify the names of one or more channels to leave, as in " <>
                          colorWith quoteColor "leave hunt"


adviceNewChanNoArgs :: Text
adviceNewChanNoArgs = prd $ "Please specify one or more new channel names, as in " <>
                            colorWith quoteColor "newchannel hunt"


advicePickNoArgs :: Text
advicePickNoArgs = prd $ "Please specify one or more items to pick, as in " <> colorWith quoteColor "pick flower"


advicePutNoArgs :: Text
advicePutNoArgs =
    prd $ "Please specify one or more items you want to put followed by where you want to put them, as in " <>
          advicePutEx


advicePutEx :: Text
advicePutEx = colorWith quoteColor "put doll sack"


advicePutNoCon :: Text
advicePutNoCon = prd $ "Please also specify where you want to put it, as in " <> advicePutEx


adviceQuitExcessArgs :: Text
adviceQuitExcessArgs = prd . T.concat $ [ "Type "
                                        , colorWith quoteColor "quit"
                                        , " with no arguments to "
                                        , dblQuote "go to sleep"
                                        , spcL . parensQuote $ "quit CurryMUD" ]


adviceReadNoArgs :: Text
adviceReadNoArgs = prd $ "Please specify the names of one or more things to read, as in " <>
                         colorWith quoteColor "read parchment"



adviceReadyNoArgs :: Text
adviceReadyNoArgs = prd $ "Please specify one or more items to ready, as in " <> colorWith quoteColor "ready sword"


adviceRemoveNoArgs :: Text
adviceRemoveNoArgs =
    prd $ "Please specify one or more items to remove followed by the container you want to remove them from, as in " <>
          adviceRemoveEx


adviceRemoveEx :: Text
adviceRemoveEx = colorWith quoteColor "remove doll sack"


adviceRemoveNoCon :: Text
adviceRemoveNoCon = prd $ "Please also specify the container you want to remove it from, as in " <> adviceRemoveEx


adviceSayAdverbNoUtterance :: Lang -> Text
adviceSayAdverbNoUtterance l = prd $ "Please also specify what you'd like to say" <>
                                     mkInLangTxtForLang l                         <>
                                     adverbExample l


adviceSayNoArgs :: Lang -> Text
adviceSayNoArgs l = T.concat [ "Please specify what you'd like to say"
                             , mkInLangTxtForLang l
                             , ", as in "
                             , colorWith quoteColor $ mkCmdNameForLang l <> " nice to meet you, too"
                             , "." ]


adviceSayToNoUtterance :: Lang -> Text
adviceSayToNoUtterance l = T.concat [ "Please also specify what you'd like to say"
                                    , mkInLangTxtForLang l
                                    , ", as in "
                                    , colorWith quoteColor . T.concat $ [ mkCmdNameForLang l
                                                                        , " "
                                                                        , T.singleton sayToChar
                                                                        , "taro nice to meet you, too" ]
                                    , "." ]


adviceSettingsInvalid :: Text
adviceSettingsInvalid = T.concat [ " Please specify the setting you want to change, followed immediately by "
                                 , dblQuote "="
                                 , ", followed immediately by the new value you want to assign, as in "
                                 , colorWith quoteColor "set columns=80"
                                 , "." ]


adviceShowNoArgs :: Text
adviceShowNoArgs = prd $ "Please specify one or more items to show followed by the name of a person, as in " <>
                         adviceShowEx


adviceShowEx :: Text
adviceShowEx = colorWith quoteColor "show ring taro"


adviceShowNoName :: Text
adviceShowNoName = prd $ "Please also provide the name of a person, as in " <> adviceShowEx


adviceSmellExcessArgs :: Text
adviceSmellExcessArgs = "Please either provide no arguments to smell the air, or specify a single item to smell."


adviceStopExcessArgs :: Text
adviceStopExcessArgs = T.concat [ "Please type "
                                , colorWith quoteColor "stop"
                                , " followed by one of the following: "
                                , colorWith quoteColor "eating"
                                , ", "
                                , colorWith quoteColor "drinking"
                                , ", "
                                , colorWith quoteColor "attacking"
                                , ", or "
                                , colorWith quoteColor "all"
                                , "." ]


adviceTasteExcessArgs :: Text
adviceTasteExcessArgs = prd $ "Please specify a single item to taste, as in " <> colorWith quoteColor "taste bread"


adviceTasteNoArgs :: Text
adviceTasteNoArgs = adviceTasteExcessArgs


adviceTeleNoArgs :: Text
adviceTeleNoArgs = prd $ "Please provide the name of a person followed by a message to send, as in " <> adviceTeleEx


adviceTeleEx :: Text
adviceTeleEx = colorWith quoteColor "telepathy taro i'll meet you there in a few"


adviceTeleNoMsg :: Text
adviceTeleNoMsg = prd $ "Please also provide a message to send, as in " <> adviceTeleEx


adviceTrashNoArgs :: Text
adviceTrashNoArgs = prd $ "Please specify one or more items to dispose of, as in " <>
                          colorWith quoteColor "trash sword"


adviceTuneInvalid :: Text
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


adviceTypoNoArgs :: Text
adviceTypoNoArgs = prd $ "Please describe the typo you've found, as in " <>
                         colorWith quoteColor "typo 'accross from the fireplace' should be 'across from the fireplace'"


adviceUnlinkNoArgs :: Text
adviceUnlinkNoArgs = prd $ "Please provide the full name of the person with whom you would like to unlink, as in " <>
                           colorWith quoteColor "unlink taro"


adviceUnreadyNoArgs :: Text
adviceUnreadyNoArgs = prd $ "Please specify one or more items to unready, as in " <> colorWith quoteColor "unready sword"


adviceWhisperNoArgs :: Text
adviceWhisperNoArgs = prd $ "Please provide the name of a person followed by what you'd like to whisper, as in " <>
                            adviceWhisperEx


adviceWhisperEx :: Text
adviceWhisperEx = colorWith quoteColor "whisper taro i have a secret to tell"


adviceWhisperNoMsg :: Text
adviceWhisperNoMsg = "Please also provide what you'd like to whisper, as in " <> adviceWhisperEx


adviceYouEmote :: Text
adviceYouEmote = T.concat [ "Sorry, but you can't use a form of the word "
                          , dblQuote "you"
                          , " in an emote. Instead, you must specify who you wish to target using "
                          , dblQuote etc
                          , ", as in "
                          , colorWith quoteColor $ "emote slowly turns her head to look directly at " <> etc <> "taro"
                          , "." ]


adviceYouEmoteChar :: Text -> Text
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


adviceZoomExcessArgs :: Text
adviceZoomExcessArgs = T.concat [ "Please either provide no arguments "
                                , parensQuote $ "to zoom to the default level of " <> showText dfltZoom
                                , ", or a single argument: the zoom level, as in "
                                , colorWith quoteColor "zoom 20"
                                , "." ]
