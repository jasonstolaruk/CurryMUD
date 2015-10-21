{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.Cmds.Util.Sorry where

import Mud.Cmds.Util.CmdPrefixes
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
import Mud.Util.Wrapping

import Data.Char (toLower)
import Data.Monoid ((<>))
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


-- TODO: Remove ----- dividers between related functions.


-- TODO: Find a home for this.
inOutOrOnOff :: T.Text
inOutOrOnOff = T.concat [ dblQuote "in"
                        , "/"
                        , dblQuote "out"
                        , " or "
                        , dblQuote "on"
                        , "/"
                        , dblQuote "off" ]


-----


sorryAdminChanTargetName :: T.Text -> T.Text
sorryAdminChanTargetName = sorryChanTargetName "admin"


-----


sorryAdminName :: T.Text -> T.Text
sorryAdminName n = "There is no administrator by the name of " <> dblQuote n <> "."


-----


sorryAlreadyThere :: T.Text
sorryAlreadyThere = "You're already there!"


-----


sorryAlreadyWearing :: T.Text -> T.Text
sorryAlreadyWearing t = "You're already wearing "  <> aOrAn t <> "."


sorryAlreadyWielding :: MudState -> Slot -> Id -> T.Text
sorryAlreadyWielding ms sl i = let s = getSing i ms in T.concat [ "You're already wielding "
                                                                , aOrAn s
                                                                , " with your "
                                                                , pp sl
                                                                , "." ]


sorryAlreadyWieldingTwoHanded :: T.Text
sorryAlreadyWieldingTwoHanded = "You're already wielding a two-handed weapon."


sorryAlreadyWieldingTwoWpns :: T.Text
sorryAlreadyWieldingTwoWpns = "You're already wielding two weapons."


-----


sorryBanAdmin :: T.Text
sorryBanAdmin = "You can't ban an admin."


-----


sorryBanSelf :: T.Text
sorryBanSelf = "You can't ban yourself."


-----


sorryBootSelf :: T.Text
sorryBootSelf = "You can't boot yourself."


-----


sorryBracketedMsg :: Either T.Text a
sorryBracketedMsg = Left "You can't open or close your message with brackets."


-----


sorryChanAlreadyConnected :: ChanName -> T.Text
sorryChanAlreadyConnected cn = "You are already connected to a channel named " <> dblQuote cn <> "."


-----


sorryChanMsg :: T.Text
sorryChanMsg = "You must also provide a message to send."


-----


sorryChanName :: ChanName -> T.Text
sorryChanName cn = "You are not connected to a channel named " <> dblQuote cn <> "."


-----



-- TODO: Is there a better name for this?
sorryChanOnlyYou :: MsgQueue -> Cols -> T.Text -> MudStack ()
sorryChanOnlyYou mq cols n = wrapSend mq cols $ "You are the only person tuned in to the " <> n <> " channel."


-----


sorryChanTargetName :: T.Text -> T.Text -> T.Text
sorryChanTargetName cn n = T.concat [ "There is no one by the name of "
                                    , dblQuote n
                                    , " currently tuned in to the "
                                    , cn
                                    , " channel." ]


sorryChanTargetNameFromContext :: T.Text -> ChanContext -> T.Text
sorryChanTargetNameFromContext n (ChanContext { .. }) = sorryChanTargetName effChanName n
  where
    effChanName = maybe someCmdName dblQuote someChanName


-----


sorryCoinsInEq :: T.Text
sorryCoinsInEq = "You don't have any coins among your readied equipment."


-----


sorryCon :: Sing -> T.Text
sorryCon s = theOnLowerCap s <> " isn't a container."


-----


sorryConInEq :: PutOrRem -> T.Text
sorryConInEq por = let (a, b) = expand por
                   in T.concat [ "Sorry, but you can't "
                               , a
                               , " an item "
                               , b
                               , " a container in your readied equipment. Please unready the container first." ]
  where
    expand = \case Put -> ("put",    "into")
                   Rem -> ("remove", "from")


-----


sorryConnectIgnore :: T.Text
sorryConnectIgnore = sorryIgnoreLocPrefPlur "The names of the people you would like to connect"


-----


sorryDemoteRoot :: T.Text
sorryDemoteRoot = "You can't demote Root."


-----


sorryDemoteSelf :: T.Text
sorryDemoteSelf = "You can't demote yourself."


-----


sorryDisconnectIgnore :: T.Text
sorryDisconnectIgnore = sorryIgnoreLocPrefPlur "The names of the people you would like to disconnect"


-----


sorryDropInEq :: T.Text
sorryDropInEq = "Sorry, but you can't drop items in your readied equipment. Please unready the item(s) first."


-----


sorryDropInRm :: T.Text
sorryDropInRm = "You can't drop an item that's already in your current room. If you're intent on dropping it, try \
                \picking it up first!"


-----


sorryEmoteExcessTargets :: T.Text
sorryEmoteExcessTargets = "Sorry, but you can only target one person at a time."


sorryEmoteIllegalTarget :: Sing -> T.Text
sorryEmoteIllegalTarget s = "You can't target " <> aOrAn s <> "."


sorryEmoteTargetCoins :: T.Text
sorryEmoteTargetCoins = "You can't target coins."


sorryEmoteTargetInEq :: T.Text
sorryEmoteTargetInEq = "You can't target an item in your readied equipment."


sorryEmoteTargetInInv :: T.Text
sorryEmoteTargetInInv = "You can't target an item in your inventory."


sorryEmoteTargetRmOnly :: T.Text
sorryEmoteTargetRmOnly = "You can only target a person in your current room."


-----


sorryEquipInvLook :: Cols -> EquipInvLookCmd -> EquipInvLookCmd -> T.Text
sorryEquipInvLook cols eilcA eilcB = wrapUnlinesNl cols . T.concat $ helper
  where
    helper = [ "You can only use the "
             , dblQuote . showText $ eilcA
             , " command to examine items in your "
             , loc eilcA
             , ". To examine items in your "
             , loc eilcB
             , ", use the "
             , dblQuote . showText $ eilcB
             , " command." ]
    loc    = \case EquipCmd -> "readied equipment"
                   InvCmd   -> "inventory"
                   LookCmd  -> "current room"


-----


sorryExpCmdLen :: Either T.Text a
sorryExpCmdLen = Left "An expressive command sequence may not be more than 2 words long."


sorryExpCmdName :: T.Text -> Either T.Text a
sorryExpCmdName cn = Left $ "There is no expressive command by the name of " <> dblQuote cn <> "."


sorryExpCmdRequiresTarget :: ExpCmdName -> T.Text
sorryExpCmdRequiresTarget cn = "The " <> dblQuote cn <> " expressive command requires a single target."


sorryExpCmdIllegalTarget :: ExpCmdName -> T.Text
sorryExpCmdIllegalTarget cn =  "The " <> dblQuote cn <> " expressive command cannot be used with a target."


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


sorryIncog :: T.Text -> T.Text
sorryIncog cn = "You can't use the " <> dblQuote cn <> " command while incognito."


-----


sorryIncogChan :: MsgQueue -> Cols -> T.Text -> MudStack ()
sorryIncogChan mq cols x = wrapSend mq cols $ "You can't send a message on " <> x <> " channel while incognito."


-----


sorryIndent :: MsgQueue -> Cols -> MudStack ()
sorryIndent mq cols = wrapSend mq cols "The indent amount must be less than the line length."


-----


sorryInvalidArg :: T.Text -> T.Text
sorryInvalidArg a = dblQuote a <> " is not a valid argument."


-----


sorryInvalidRmName :: T.Text -> T.Text
sorryInvalidRmName n = T.concat [ dblQuote n
                                , " is not a valid room name. Type "
                                , quoteColor
                                , prefixAdminCmd "telerm"
                                , dfltColor
                                , " with no arguments to get a list of valid room names." ]


-----


sorryPeepAdmin :: T.Text
sorryPeepAdmin = "You can't peep an admin."


-----


sorryPeepSelf :: T.Text
sorryPeepSelf = "You can't peep yourself."


-----


sorryPutInEq :: T.Text
sorryPutInEq = "Sorry, but you can't put items in your readied equipment into a container. Please unready the item(s) \
               \first."


sorryPutInRm :: T.Text
sorryPutInRm = "Sorry, but you can't put items in your current room into a container. Please pick up the item(s) first."


-----


sorryPutExcessCon :: T.Text
sorryPutExcessCon = "You can only put things into one container at a time."


-----


sorryQuitCan'tAbbrev :: T.Text
sorryQuitCan'tAbbrev = T.concat [ "The "
                                , dblQuote "quit"
                                , " command may not be abbreviated. Type "
                                , dblQuote "quit"
                                , " with no arguments to quit CurryMUD." ]


-----


sorryReadyClothFull :: T.Text -> T.Text
sorryReadyClothFull t = "You can't wear any more " <> t <> "s."


sorryReadyCoins :: T.Text
sorryReadyCoins = "You can't ready coins."


sorryReadyInEq :: T.Text
sorryReadyInEq = "You can't ready an item that's already in your readied equipment."


sorryReadyInRm :: T.Text
sorryReadyInRm = "Sorry, but you can't ready items in your current room. Please pick up the item(s) first."


sorryReadyRol :: Sing -> RightOrLeft -> T.Text
sorryReadyRol s rol = T.concat [ "You can't wear ", aOrAn s, " on your ", pp rol, "." ]


sorryReadyType :: Sing -> T.Text
sorryReadyType s = "You can't ready " <> aOrAn s <> "."


sorryReadyWpnHands :: Sing -> T.Text
sorryReadyWpnHands s = "Both hands are required to wield the " <> s <> "."


sorryReadyWpnRol :: Sing -> T.Text
sorryReadyWpnRol s = "You can't wield " <> aOrAn s <> " with your finger!"


-----


sorryTeleportToSelf :: T.Text
sorryTeleportToSelf = "You can't teleport to yourself."


-----


sorryMsg_LoggedInTarget_Incog :: T.Text
sorryMsg_LoggedInTarget_Incog = "You can't send a message to a player who is logged in while you are incognito."


-----


sorryMyChansIgnore :: T.Text
sorryMyChansIgnore = sorryIgnoreLocPrefPlur "The PC names of the players whose channel information you would like to \
                                            \see"


-----


sorryNameTaken :: T.Text
sorryNameTaken = "Sorry, but that name is already taken."


-----


sorryNoContainersHere :: T.Text
sorryNoContainersHere = "You don't see any containers here."


-----


sorryNoOneHere :: T.Text
sorryNoOneHere = "You don't see anyone here."


-----


sorryNotLoggedIn :: Sing -> T.Text
sorryNotLoggedIn = (<> " is not logged in.")


-----


sorryNotTunedICChan :: ChanName -> T.Text
sorryNotTunedICChan = sorryNotTunedChan "tune"


sorryNotTunedChan :: T.Text -> T.Text -> T.Text
sorryNotTunedChan x y = T.concat [ "You have tuned out the "
                                 , dblQuote y -- TODO: Shouldn't we only double quote IC chan names?
                                 , " channel. Type "
                                 , quoteColor
                                 , x
                                 , " "
                                 , y
                                 , "=in"
                                 , dfltColor
                                 , " to tune it back in." ]


sorryNotTunedOOCChan :: T.Text -> T.Text
sorryNotTunedOOCChan = sorryNotTunedChan "set"


-----


sorryPCName :: T.Text -> T.Text
sorryPCName n = "There is no PC by the name of " <> dblQuote n <> "."


-----


sorryPCNameLoggedIn :: T.Text -> T.Text
sorryPCNameLoggedIn n = "No PC by the name of " <> dblQuote n <> " is currently logged in."


-----


sorryParseBase :: MsgQueue -> Cols -> T.Text -> MudStack ()
sorryParseBase mq cols txt = wrapSend mq cols $ dblQuote txt <> " is not a valid base."


-----


sorryParseChanId :: T.Text -> T.Text
sorryParseChanId a = dblQuote a <> " is not a valid channel ID."


-----


sorryParseId :: T.Text -> T.Text
sorryParseId a = dblQuote a <> " is not a valid ID."


-----


sorryParseIndent :: MsgQueue -> Cols -> T.Text -> MudStack ()
sorryParseIndent mq cols a = wrapSend mq cols $ dblQuote a <> " is not a valid width amount."


-----


sorryParseLineLen :: MsgQueue -> Cols -> T.Text -> MudStack ()
sorryParseLineLen mq cols a = wrapSend mq cols $ dblQuote a <> " is not a valid line length."


-----


sorryParseNum :: MsgQueue -> Cols -> T.Text -> T.Text -> MudStack ()
sorryParseNum mq cols numTxt base = wrapSend mq cols . T.concat $ [ dblQuote numTxt
                                                                  , " is not a valid number in base "
                                                                  , base
                                                                  , "." ]


-----


sorryPeepIgnore :: T.Text
sorryPeepIgnore = sorryIgnoreLocPrefPlur "The PC names of the players you wish to start or stop peeping"


-----


sorryPutInCoin :: T.Text
sorryPutInCoin = "You can't put something inside a coin."


-----


sorryRegPlaName :: T.Text -> T.Text
sorryRegPlaName n = "There is no regular player by the name of " <>
                    dblQuote n                                   <>
                    "."


-----


sorryRemCoin :: T.Text
sorryRemCoin = "You can't remove something from a coin."


sorryRemExcessCon :: T.Text
sorryRemExcessCon = "You can only remove things from one container at a time."


sorryRemIgnore :: T.Text
sorryRemIgnore = sorryIgnoreLocPrefPlur "The names of the items to be removed from a container "


-----


sorrySayCoin :: T.Text
sorrySayCoin = "You're talking to coins now?"


sorrySayExcessTargets :: T.Text
sorrySayExcessTargets = "Sorry, but you can only say something to one person at a time."


sorrySayInEq :: T.Text
sorrySayInEq = "You can't talk to an item in your readied equipment. Try saying something to someone in your current \
               \room."


sorrySayInInv :: T.Text
sorrySayInInv = "You can't talk to an item in your inventory. Try saying something to someone in your current room."


sorrySayNoOneHere :: T.Text
sorrySayNoOneHere = "You don't see anyone here to talk to."


sorrySayTargetType :: Sing -> T.Text
sorrySayTargetType s = "You can't talk to " <> aOrAn s <> "."


-----


sorrySetParse :: T.Text -> T.Text -> T.Text
sorrySetParse value name = T.concat [ dblQuote value
                                    , " is not a valid value for the "
                                    , dblQuote name
                                    , " setting." ]


sorrySetParseInOut :: T.Text -> T.Text -> T.Text
sorrySetParseInOut value n = T.concat [ dblQuote value
                                      , " is not a valid value for the "
                                      , dblQuote n
                                      , " setting. Please specify one of the following: "
                                      , inOutOrOnOff
                                      , "." ]


sorrySetRange :: T.Text -> T.Text -> T.Text -> T.Text
sorrySetRange settingName minValTxt maxValTxt = T.concat [ capitalize settingName
                                                         , " must be between "
                                                         , minValTxt
                                                         , " and "
                                                         , maxValTxt
                                                         , "." ]


sorrySetSettingName :: T.Text -> T.Text
sorrySetSettingName n = dblQuote n <> " is not a valid setting name."


-----


sorryShowExcessTargets :: T.Text
sorryShowExcessTargets = "Sorry, but you can only show something to one person at a time."


sorryShowInRm :: T.Text
sorryShowInRm = "You can't show an item in your current room."


sorryShowTarget :: T.Text -> T.Text
sorryShowTarget t = "You can't show something to " <> aOrAn t <> "."


-----


sorryTuneName :: T.Text -> T.Text
sorryTuneName n = "You don't have a connection by the name of " <> dblQuote n <> "."


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


sorryUnlinkName :: T.Text -> T.Text
sorryUnlinkName t = T.concat [ "You don't have a link with "
                             , dblQuote t
                             , ". "
                             , parensQuote "Note that you must specify the full name of the person with whom you would \
                                           \like to unlink." ]


-----


sorryUnreadyCoins :: T.Text
sorryUnreadyCoins = "You can't unready coins."


sorryUnreadyInInv :: T.Text
sorryUnreadyInInv = "You can't unready items in your inventory."


sorryUnreadyInRm :: T.Text
sorryUnreadyInRm = "You can't unready items in your current room."


-----


sorryWrapLineLen :: MsgQueue -> Cols -> MudStack ()
sorryWrapLineLen mq cols = wrapSend mq cols . T.concat $ [ "The line length must be between "
                                                         , showText minCols
                                                         , " and "
                                                         , showText maxCols
                                                         , " characters." ]


----


sorryWtf :: T.Text
sorryWtf = quoteWith' (wtfColor, dfltColor) "He don't."
