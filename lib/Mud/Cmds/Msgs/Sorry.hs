{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.Cmds.Msgs.Sorry ( sorryAdminChanSelf
                           , sorryAdminChanTargetName
                           , sorryAdminKillAsleep
                           , sorryAdminKillSelf
                           , sorryAdminKillType
                           , sorryAdminName
                           , sorryAdminPasswordAdmin
                           , sorryAdminPasswordSelf
                           , sorryAdminSetKey
                           , sorryAdminSetOp
                           , sorryAdminSetType
                           , sorryAdminSetValue
                           , sorryAlreadyPossessed
                           , sorryAlreadyPossessing
                           , sorryAsAdmin
                           , sorryAsSelf
                           , sorryAsType
                           , sorryBanAdmin
                           , sorryBanSelf
                           , sorryBonusCount
                           , sorryBonusIgnore
                           , sorryBonusLvl
                           , sorryBonusName
                           , sorryBonusTime
                           , sorryBootSelf
                           , sorryBracketedMsg
                           , sorryChanIncog
                           , sorryChanMsg
                           , sorryChanName
                           , sorryChanNoOneListening
                           , sorryChanTargetName
                           , sorryChanTargetNameFromContext
                           , sorryCmdNotFound
                           , sorryCon
                           , sorryConInEq
                           , sorryConnectAlready
                           , sorryConnectChanName
                           , sorryConnectIgnore
                           , sorryDisconnectIgnore
                           , sorryDrinkAlready
                           , sorryDrinkCoins
                           , sorryDrinkEating
                           , sorryDrinkEmpty
                           , sorryDrinkEmptyRmNoHooks
                           , sorryDrinkEmptyRmWithHooks
                           , sorryDrinkExcessTargets
                           , sorryDrinkInEq
                           , sorryDrinkMouthfuls
                           , sorryDrinkRmNoHooks
                           , sorryDrinkRmWithHooks
                           , sorryDrinkType
                           , sorryDropInEq
                           , sorryDropInRm
                           , sorryEmoteExcessTargets
                           , sorryEmoteTargetCoins
                           , sorryEmoteTargetInEq
                           , sorryEmoteTargetInInv
                           , sorryEmoteTargetRmOnly
                           , sorryEmoteTargetType
                           , sorryEmptyAlready
                           , sorryEmptyCoins
                           , sorryEmptyCon
                           , sorryEmptyCorpse
                           , sorryEmptyInEq
                           , sorryEmptyInRm
                           , sorryEmptyType
                           , sorryEquipCoins
                           , sorryEquipInvLook
                           , sorryExpCmdCoins
                           , sorryExpCmdIllegalTarget
                           , sorryExpCmdInInvEq
                           , sorryExpCmdLen
                           , sorryExpCmdName
                           , sorryExpCmdRequiresTarget
                           , sorryExpCmdTargetType
                           , sorryFillAlreadyFull
                           , sorryFillCoins
                           , sorryFillEmptyRmNoHooks
                           , sorryFillEmptyRmWithHooks
                           , sorryFillEmptySource
                           , sorryFillExcessSources
                           , sorryFillInEq
                           , sorryFillInRm
                           , sorryFillLiqTypes
                           , sorryFillRmNoHooks
                           , sorryFillRmWithHooks
                           , sorryFillSelf
                           , sorryFillSourceCoins
                           , sorryFillSourceEq
                           , sorryFillSourceType
                           , sorryFillType
                           , sorryFillWaterLiqTypes
                           , sorryFull
                           , sorryGetEmptyRmNoHooks
                           , sorryGetEmptyRmWithHooks
                           , sorryGetEnc
                           , sorryGetInEq
                           , sorryGetInInv
                           , sorryGetNothingHere
                           , sorryGetType
                           , sorryGetWeight
                           , sorryGiveEnc
                           , sorryGiveExcessTargets
                           , sorryGiveInEq
                           , sorryGiveInRm
                           , sorryGiveToCoin
                           , sorryGiveToEq
                           , sorryGiveToInv
                           , sorryGiveType
                           , sorryGoExit
                           , sorryGoParseDir
                           , sorryHelpName
                           , sorryIgnoreLocPref
                           , sorryIgnoreLocPrefPlur
                           , sorryIncog
                           , sorryIndent
                           , sorryInterpNameDict
                           , sorryInterpNameExcessArgs
                           , sorryInterpNameIllegal
                           , sorryInterpNameLen
                           , sorryInterpNameProfanityBoot
                           , sorryInterpNameProfanityLogged
                           , sorryInterpNamePropName
                           , sorryInterpNameTaken
                           , sorryInterpNewPwDigit
                           , sorryInterpNewPwExcessArgs
                           , sorryInterpNewPwLen
                           , sorryInterpNewPwLower
                           , sorryInterpNewPwMatch
                           , sorryInterpNewPwUpper
                           , sorryInterpPager
                           , sorryInterpPickPtsMax
                           , sorryInterpPickPtsMin
                           , sorryInterpPickPtsPts
                           , sorryInterpPickPtsQuit
                           , sorryInterpPW
                           , sorryInterpPwBanned
                           , sorryInterpPwBoot
                           , sorryInterpPwLoggedIn
                           , sorryIntroAlready
                           , sorryIntroCoin
                           , sorryIntroInEq
                           , sorryIntroInInv
                           , sorryIntroNoOneHere
                           , sorryIntroType
                           , sorryLinkAlready
                           , sorryLinkCoin
                           , sorryLinkInEq
                           , sorryLinkInInv
                           , sorryLinkIntroSelf
                           , sorryLinkIntroTarget
                           , sorryLinkNoOneHere
                           , sorryLinkType
                           , sorryLoggedOut
                           , sorryLookEmptyRmNoHooks
                           , sorryLookEmptyRmWithHooks
                           , sorryMsgIncog
                           , sorryNewChanExisting
                           , sorryNewChanName
                           , sorryNoAdmins
                           , sorryNoConHere
                           , sorryNoLinks
                           , sorryNonexistentId
                           , sorryNoOneHere
                           , sorryNotPossessed
                           , sorryParseAmt
                           , sorryParseArg
                           , sorryParseBase
                           , sorryParseChanId
                           , sorryParseId
                           , sorryParseIndent
                           , sorryParseInOut
                           , sorryParseLineLen
                           , sorryParseMouthfuls
                           , sorryParseNum
                           , sorryParseOnOff
                           , sorryParseSetting
                           , sorryPCName
                           , sorryPCNameLoggedIn
                           , sorryPeepAdmin
                           , sorryPeepSelf
                           , sorryPickInEq
                           , sorryPickInInv
                           , sorryPickNotFlower
                           , sorryPossessType
                           , sorryPp
                           , sorryPutEmptyRmWithHooks
                           , sorryPutExcessCon
                           , sorryPutInCoin
                           , sorryPutInEq
                           , sorryPutInRm
                           , sorryPutInsideSelf
                           , sorryPutVol
                           , sorryQuitCan'tAbbrev
                           , sorryQuoteChars
                           , sorryReadCoins
                           , sorryReadInEq
                           , sorryReadLang
                           , sorryReadNoHooks
                           , sorryReadOrigLang
                           , sorryReadType
                           , sorryReadUnknownLang
                           , sorryReadWithHooks
                           , sorryReadyAlreadyWearing
                           , sorryReadyAlreadyWearingRing
                           , sorryReadyAlreadyWielding
                           , sorryReadyAlreadyWieldingTwoHanded
                           , sorryReadyAlreadyWieldingTwoWpns
                           , sorryReadyClothFull
                           , sorryReadyClothFullOneSide
                           , sorryReadyCoins
                           , sorryReadyInEq
                           , sorryReadyInRm
                           , sorryReadyRol
                           , sorryReadyType
                           , sorryReadyWpnHands
                           , sorryReadyWpnRol
                           , sorryRegPlaName
                           , sorryRemCoin
                           , sorryRemEmpty
                           , sorryRemEnc
                           , sorryRemExcessCon
                           , sorryRemIgnore
                           , sorrySayCoins
                           , sorrySayExcessTargets
                           , sorrySayInEq
                           , sorrySayInInv
                           , sorrySayNoOneHere
                           , sorrySayTargetType
                           , sorrySearch
                           , sorrySetName
                           , sorrySetRange
                           , sorryShowExcessTargets
                           , sorryShowInRm
                           , sorryShowTarget
                           , sorrySmellEmptyRmNoHooks
                           , sorrySmellEmptyRmWithHooks
                           , sorrySmellExcessTargets
                           , sorrySmellNothingToSmell
                           , sorrySmellRmCoins
                           , sorrySmellRmNoHooks
                           , sorryStopActName
                           , sorryStopNotDoing
                           , sorryStopNotDoingAnything
                           , sorrySudoerDemoteRoot
                           , sorrySudoerDemoteSelf
                           , sorrySummonAdmin
                           , sorrySummonAlready
                           , sorrySummonSelf
                           , sorryTasteExcessTargets
                           , sorryTasteInRm
                           , sorryTasteNothingToTaste
                           , sorryTasteType
                           , sorryTeleAlready
                           , sorryTeleLoggedOutRm
                           , sorryTeleRmName
                           , sorryTeleSelf
                           , sorryTrashInEq
                           , sorryTrashInRm
                           , sorryTunedOutChan
                           , sorryTunedOutICChan
                           , sorryTunedOutOOCChan
                           , sorryTunedOutPCSelf
                           , sorryTunedOutPCTarget
                           , sorryTuneName
                           , sorryTwoWayLink
                           , sorryTwoWayTargetName
                           , sorryUnlinkIgnore
                           , sorryUnlinkName
                           , sorryUnreadyCoins
                           , sorryUnreadyInInv
                           , sorryUnreadyInRm
                           , sorryWhisperCoins
                           , sorryWhisperExcessTargets
                           , sorryWhisperInEq
                           , sorryWhisperInInv
                           , sorryWhisperNoOneHere
                           , sorryWhisperTargetType
                           , sorryWireAlready
                           , sorryWrapLineLen
                           , sorryWtf
                           , sorryWut ) where

import Mud.Cmds.Util.CmdPrefixes
import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Misc.ANSI
import Mud.Misc.Misc
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Char (toLower)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Cmds.Msgs.Sorry"


-- ==================================================


sorryIgnoreLocPref :: Text -> Text
sorryIgnoreLocPref msg = parensQuote $ msg <> " need not be given a location prefix. The location prefix you provided \
                                              \will be ignored."


sorryIgnoreLocPrefPlur :: Text -> Text
sorryIgnoreLocPrefPlur msg = parensQuote $ msg <> " need not be given location prefixes. The location prefixes you \
                                                  \provided will be ignored."


-----


sorryInterpPickPtsMax :: Text -> Text
sorryInterpPickPtsMax t = T.concat [ "You can't add any more to "
                                   , t
                                   , " "
                                   , parensQuote "it's set to the maximum value of 100"
                                   , "." ]


sorryInterpPickPtsMin :: Text -> Text
sorryInterpPickPtsMin t = T.concat [ "You can't subtract any more from "
                                   , t
                                   , " "
                                   , parensQuote "it's set to the minimum value of 10"
                                   , "." ]


sorryInterpPickPtsPts :: Text
sorryInterpPickPtsPts = "You don't have any points left."


sorryInterpPickPtsQuit :: Text
sorryInterpPickPtsQuit = "You still have points remaining. Please assign all your available points before moving on."


-----


sorryAdminChanSelf :: Text
sorryAdminChanSelf = "You talk to yourself."


sorryAdminChanTargetName :: Text -> Text
sorryAdminChanTargetName = sorryChanTargetName "admin"


-----


sorryAdminKillAsleep :: Text -> Text
sorryAdminKillAsleep t = t <> " is presently asleep and cannot cannot be killed."


sorryAdminKillSelf :: Text
sorryAdminKillSelf = can't "kill yourself."


sorryAdminKillType :: Id -> Text
sorryAdminKillType i = "ID " <> showText i <> "is not a mobile."


-----


sorryAdminName :: Text -> Text
sorryAdminName n = prd $ "There is no administrator by the name of " <> dblQuote n


-----


sorryAdminPasswordAdmin :: Text
sorryAdminPasswordAdmin = can't "change an admin's password."


sorryAdminPasswordSelf :: Text
sorryAdminPasswordSelf = "Please use the " <> dblQuote "password" <> " command to change your own password."


-----


sorryAdminSetKey :: Text -> Text
sorryAdminSetKey k = dblQuote k <> " is not a valid key."


sorryAdminSetOp :: Text -> Text -> Text
sorryAdminSetOp opTxt k = T.concat [ "The ", dblQuote opTxt, " operator cannot be used with the ", dblQuote k, " key." ]


sorryAdminSetType :: Id -> Text
sorryAdminSetType i = "ID " <> showText i <> " is not of the correct type."


sorryAdminSetValue :: Text -> Text -> Text
sorryAdminSetValue k v = T.concat [ v, " is not a valid value for the ", dblQuote k, " key." ]


-----


sorryAlreadyPossessed :: Sing -> Sing -> Text
sorryAlreadyPossessed a b = but . T.concat $ [ theOnLower a, " is already possessed by ", b, "." ]


-----


sorryAlreadyPossessing :: Sing -> Text
sorryAlreadyPossessing s = prd $ "You are already possessing " <> theOnLower s


-----


sorryAsAdmin :: Text
sorryAsAdmin = can'tTarget "an admin" <> withAs


can'tTarget :: Text -> Text
can'tTarget = can't . ("target " <>)


can't :: Text -> Text
can't = ("You can't " <>)


withAs :: Text
withAs = " with the " <> dblQuote (prefixAdminCmd "as") <> " command."


sorryAsSelf :: Text
sorryAsSelf = can'tTarget "yourself" <> withAs


sorryAsType :: Sing -> Text
sorryAsType s = can'tTarget $ aOrAn s <> withAs


-----


sorryBanAdmin :: Text
sorryBanAdmin = can't "ban an admin."


sorryBanSelf :: Text
sorryBanSelf = can't "ban yourself."


-----


sorryBonusCount :: Sing -> Text
sorryBonusCount s = prd $ "You have already given the maximum number of 5 bonuses to " <> s


sorryBonusIgnore :: Text
sorryBonusIgnore = sorryIgnoreLocPref "The name of the character whose player you would like to give a bonus to"


sorryBonusLvl :: Text
sorryBonusLvl = "You must be at least level 3 to give a bonus."


sorryBonusName :: Text -> Text
sorryBonusName n = prd $ "You don't know anyone by the name of " <> dblQuote n


sorryBonusTime :: Text
sorryBonusTime = "It's too early since you last gave a bonus."


-----


sorryBootSelf :: Text
sorryBootSelf = can't "boot yourself."


-----


sorryBracketedMsg :: Text
sorryBracketedMsg = can't "open or close your message with brackets."


-----


sorryChanIncog :: Text -> Text
sorryChanIncog t = can't "send a message on " <> t <> " channel while incognito."


sorryChanMsg :: Text
sorryChanMsg = "You must also provide a message to send."


sorryChanName :: ChanName -> Text
sorryChanName cn = prd $ "You are not connected to a channel named " <> dblQuote cn


sorryChanNoOneListening :: Text -> Text
sorryChanNoOneListening t = "You are the only person tuned in to the " <> t <> " channel."


sorryChanTargetName :: Text -> Text -> Text
sorryChanTargetName cn n = T.concat [ "There is no one by the name of "
                                    , dblQuote n
                                    , " currently tuned in to the "
                                    , cn
                                    , " channel." ]


sorryChanTargetNameFromContext :: Text -> ChanContext -> Text
sorryChanTargetNameFromContext n ChanContext { .. } = sorryChanTargetName effChanName n
  where
    effChanName = maybe someCmdName dblQuote someChanName


-----


sorryCmdNotFound :: Text
sorryCmdNotFound = "What?"


-----


sorryCon :: Sing -> Text
sorryCon s = theOnLowerCap s <> " isn't a container."


sorryConInEq :: PutOrRem -> Text
sorryConInEq por =
    let (a, b) = expand por
    in butCan't . T.concat $ [ a
                             , " an item "
                             , b
                             , " a container in your readied equipment. Please unready the container first." ]
  where
    expand = \case Put -> ("put",    "into")
                   Rem -> ("remove", "from")


butCan't :: Text -> Text
butCan't = but . ("you can't " <>)


but :: Text -> Text
but = ("Sorry, but " <>)


-----


sorryConnectAlready :: Sing -> ChanName -> Text
sorryConnectAlready s cn = T.concat [ s, " is already connected to the ", dblQuote cn, " channel." ]


sorryConnectChanName :: Sing -> ChanName -> Text
sorryConnectChanName s cn = T.concat [ s, " is already connected to a channel named ", dblQuote cn, "." ]


sorryConnectIgnore :: Text
sorryConnectIgnore = sorryIgnoreLocPrefPlur "The names of the people you would like to connect"


-----


sorryDisconnectIgnore :: Text
sorryDisconnectIgnore = sorryIgnoreLocPrefPlur "The names of the people you would like to disconnect"


-----


sorryDrinkAlready :: Liq -> Sing -> Text
sorryDrinkAlready l s = T.concat [ "You are already drinking "
                                 , renderLiqNoun l aOrAn
                                 , " from "
                                 , aOrAn s
                                 , ". Please wait until you finish, or type "
                                 , colorWith quoteColor "stop drinking"
                                 , " to stop." ]


sorryDrinkCoins :: Text
sorryDrinkCoins = can't "drink from a coin."


sorryDrinkEating :: Sing -> Text
sorryDrinkEating s = T.concat [ "You are presently eating "
                              , aOrAnOnLower s
                              , ". Please wait until you finish, or type "
                              , colorWith quoteColor "stop eating"
                              , " to stop." ]



sorryDrinkEmpty :: Sing -> Text
sorryDrinkEmpty s = "The " <> s <> " is empty."


sorryDrinkEmptyRmNoHooks :: Text
sorryDrinkEmptyRmNoHooks = "You don't see anything to drink from here."


sorryDrinkEmptyRmWithHooks :: Text
sorryDrinkEmptyRmWithHooks = "You don't see any vessels on the ground here."


sorryDrinkExcessTargets :: Text
sorryDrinkExcessTargets = but "you can only drink from one vessel at a time."


sorryDrinkInEq :: Text
sorryDrinkInEq = can't "drink from an item in your readied equipment."


sorryDrinkMouthfuls :: Text
sorryDrinkMouthfuls = "Do you or do you not wish to take a drink?"


sorryDrinkRmNoHooks :: Text
sorryDrinkRmNoHooks = butCan't "drink from a vessel in your current room. Please pick up the vessel first."


sorryDrinkRmWithHooks :: Text -> Text
sorryDrinkRmWithHooks t = T.concat [ "You don't see "
                                   , aOrAn t
                                   , " here. "
                                   , parensQuote "If you'd like to drink from a vessel on the ground, please pick up \
                                                 \the vessel first." ]


sorryDrinkType :: Sing -> Text
sorryDrinkType s = prd $ can't "drink from " <> aOrAn s


-----


sorryDropInEq :: Text
sorryDropInEq = butCan't "drop an item in your readied equipment. Please unready the item(s) first."


sorryDropInRm :: Text
sorryDropInRm = can't "drop an item that's already in your current room. If you're intent on dropping it, try picking \
                      \it up first!"


-----


sorryEmoteExcessTargets :: Text
sorryEmoteExcessTargets = but "you can only target one person at a time."


sorryEmoteTargetCoins :: Text
sorryEmoteTargetCoins = can'tTarget "coins."


sorryEmoteTargetInEq :: Text
sorryEmoteTargetInEq = can'tTarget "an item in your readied equipment."


sorryEmoteTargetInInv :: Text
sorryEmoteTargetInInv = can'tTarget "an item in your inventory."


sorryEmoteTargetRmOnly :: Text
sorryEmoteTargetRmOnly = "You can only target a person in your current room."


sorryEmoteTargetType :: Sing -> Text
sorryEmoteTargetType s = prd . can'tTarget $ aOrAn s


-----


sorryEmptyAlready :: Sing -> Text
sorryEmptyAlready s = "The " <> s <> " is already empty."


sorryEmptyCoins :: Text
sorryEmptyCoins = can't "empty a coin."


sorryEmptyCon :: Text
sorryEmptyCon = sorryEmptyHelper "container"


sorryEmptyHelper :: Text -> Text
sorryEmptyHelper t = butCan't . T.concat $ [ "empty a "
                                           , t
                                           , " with the "
                                           , dblQuote "empty"
                                           , " command. Please use the "
                                           , dblQuote "remove"
                                           , " command to remove items from a "
                                           , t
                                           , "." ]


sorryEmptyCorpse :: Text
sorryEmptyCorpse = sorryEmptyHelper "corpse"


sorryEmptyInEq :: Text
sorryEmptyInEq = can't "empty an item in your readied equipment."


sorryEmptyInRm :: Text
sorryEmptyInRm = butCan't "empty a vessel in your current room. Please pick up the vessel(s) first."


sorryEmptyType :: Sing -> Text
sorryEmptyType s = prd $ can't "empty " <> aOrAn s


-----


sorryEquipCoins :: Text
sorryEquipCoins = "You don't have any coins among your readied equipment."


-----


sorryEquipInvLook :: EquipInvLookCmd -> EquipInvLookCmd -> Text
sorryEquipInvLook a b = T.concat [ "You can only use the "
                                 , dblQuote . showText $ a
                                 , " command to examine items in your "
                                 , loc a
                                 , ". To examine items in your "
                                 , loc b
                                 , ", use the "
                                 , dblQuote . showText $ b
                                 , " command." ]
  where
    loc = \case EquipCmd -> "readied equipment"
                InvCmd   -> "inventory"
                LookCmd  -> "current room"


-----


sorryExpCmdCoins :: Text
sorryExpCmdCoins = but "expressive commands cannot be used with coins."


sorryExpCmdInInvEq :: InInvEqRm -> Text
sorryExpCmdInInvEq loc = can'tTarget "an item in your " <> loc' <> " with an expressive command."
  where
    loc' = case loc of InEq  -> "readied equipment"
                       InInv -> "inventory"
                       _     -> patternMatchFail "sorryExpCmdInInvEq loc'" . showText $ loc


sorryExpCmdLen :: Text
sorryExpCmdLen = "An expressive command sequence may not be more than 2 words long."


sorryExpCmdName :: Text -> Text
sorryExpCmdName cn = prd $ "There is no expressive command by the name of " <> dblQuote cn


sorryExpCmdIllegalTarget :: ExpCmdName -> Text
sorryExpCmdIllegalTarget cn =  "The " <> dblQuote cn <> " expressive command cannot be used with a target."


sorryExpCmdRequiresTarget :: ExpCmdName -> Text
sorryExpCmdRequiresTarget cn = "The " <> dblQuote cn <> " expressive command requires a single target."


sorryExpCmdTargetType :: Text
sorryExpCmdTargetType = but "expressive commands can only target people."


-----


sorryFillAlreadyFull :: Sing -> Text
sorryFillAlreadyFull s = "The " <> s <> " is already full."


sorryFillCoins :: Text
sorryFillCoins = can't "fill a coin."


sorryFillEmptyRmNoHooks :: Text
sorryFillEmptyRmNoHooks = "You don't see a source of liquid here."


sorryFillEmptyRmWithHooks :: Text
sorryFillEmptyRmWithHooks = "You don't see any vessels on the ground here."


sorryFillEmptySource :: Sing -> Text
sorryFillEmptySource s = "The " <> s <> " is empty."


sorryFillExcessSources :: Text
sorryFillExcessSources = but "you can only specify a single liquid source at a time."


sorryFillInEq :: Text
sorryFillInEq = can't "fill an item in your readied equipment."


sorryFillInRm :: Text
sorryFillInRm = butCan't "fill a vessel in your current room. Please pick up the vessel(s) first."


sorryFillLiqTypes :: BothGramNos -> BothGramNos -> Text
sorryFillLiqTypes a@(as, _) b@(bs, _) = helper $ if
  | a == b    -> "The " <> mkPlurFromBoth a
  | otherwise -> T.concat [ "The ", as, " and the ", bs ]
  where
    helper = (<> " do not contain the same kind of liquid.")


sorryFillRmNoHooks :: Text
sorryFillRmNoHooks = butCan't "fill something with the contents of a vessel in your current room. Please pick up the \
                              \vessel first."


sorryFillRmWithHooks :: Text -> Text
sorryFillRmWithHooks t = T.concat [ "You don't see "
                                  , aOrAn t
                                  , " here. "
                                  , parensQuote "If you'd like to fill something with the contents of a vessel on the \
                                                \ground, please pick up the vessel first." ]


sorryFillSelf :: Text -> Text
sorryFillSelf s = can't "fill the " <> s <> " with itself."


sorryFillSourceCoins :: Text
sorryFillSourceCoins = butCan't "fill a vessel with coins."


sorryFillSourceEq :: Text
sorryFillSourceEq = can't "fill a vessel with an item in your readied equipment."


sorryFillSourceType :: Sing -> Text
sorryFillSourceType s = "The " <> s <> " is not a vessel or a source of liquid."


sorryFillType :: Sing -> Text
sorryFillType s = "The " <> s <> " is not a vessel that can be filled with liquid."


sorryFillWaterLiqTypes :: Sing -> Text
sorryFillWaterLiqTypes s = "The " <> s <> " already contains something other than water."


-----


sorryFull :: Text
sorryFull = "Ugh, you are nauseatingly satiated. You can't imagine consuming any more."


-----


sorryGetEmptyRmNoHooks :: Text
sorryGetEmptyRmNoHooks = "You don't see anything to pick up here."


sorryGetEmptyRmWithHooks :: Text
sorryGetEmptyRmWithHooks = "You don't see anything to pick up on the ground here."


sorryGetEnc :: Text
sorryGetEnc = "You are too encumbered to carry "


sorryGetInEq :: Text
sorryGetInEq = butCan't "get an item in your readied equipment. If you want to move a readied item to your inventory, \
                        \use the "         <>
                        dblQuote "unready" <>
                        " command."


sorryGetInInv :: Text
sorryGetInInv = can't "get an item that's already in your inventory. If you're intent on picking it up, try dropping \
                      \it first!"


sorryGetNothingHere :: Text
sorryGetNothingHere = "You don't see anything to pick up here."


sorryGetType :: Text -> Text
sorryGetType t = prd $ can't "pick up " <> t


sorryGetWeight :: Sing -> Text
sorryGetWeight s = "The " <> s <> " is too heavy for you to pick up."


-----


sorryGiveEnc :: Text -> Text
sorryGiveEnc = (<> " is too encumbered to hold ")


sorryGiveExcessTargets :: Text
sorryGiveExcessTargets = but "you can only give things to one person at a time."


sorryGiveInEq :: Text
sorryGiveInEq = butCan't "give an item in your readied equipment. Please unready the item(s) first."


sorryGiveInRm :: Text
sorryGiveInRm = butCan't "give an item in your current room. Please pick up the item(s) first."


sorryGiveToCoin :: Text
sorryGiveToCoin = can't "give something to a coin."


sorryGiveToEq :: Text
sorryGiveToEq = can't "give something to an item in your readied equipment."


sorryGiveToInv :: Text
sorryGiveToInv = can't "give something to an item in your inventory."


sorryGiveType :: Sing -> Text
sorryGiveType s = prd $ can't "give something to " <> aOrAn s


-----


sorryGoExit :: Text
sorryGoExit = can't "go that way."


sorryGoParseDir :: Text -> Text
sorryGoParseDir t = dblQuote t <> " is not a valid exit."


-----


sorryHelpName :: Text -> Text
sorryHelpName t = prd $ "No help is available on " <> dblQuote t


-----


sorryIncog :: Text -> Text
sorryIncog cn = can't "use the " <> dblQuote cn <> " command while incognito."


-----


sorryIndent :: Text
sorryIndent = "The indent amount must be less than the line length."


-----


sorryInterpNameDict :: Text
sorryInterpNameDict = "Your name cannot be an English word or proper name. Please choose an original fantasy name."


sorryInterpNameExcessArgs :: Text
sorryInterpNameExcessArgs = "Your name must be a single word."


sorryInterpNameIllegal :: Text
sorryInterpNameIllegal = "Your name cannot include any numbers or symbols."


sorryInterpNameLen :: Text
sorryInterpNameLen = T.concat [ "Your name must be between "
                              , minNameLenTxt
                              , " and "
                              , maxNameLenTxt
                              , " characters long." ]


sorryInterpNameProfanityLogged :: Text
sorryInterpNameProfanityLogged = "Nice try. Your IP address has been logged. Keep this up and you'll get banned."


sorryInterpNameProfanityBoot :: Text
sorryInterpNameProfanityBoot = "Come back when you're ready to act like an adult!"


sorryInterpNamePropName :: Text
sorryInterpNamePropName = "Your name cannot be a real-world proper name. Please choose an original fantasy name."


sorryInterpNameTaken :: Text
sorryInterpNameTaken = but "that name is already taken."


sorryInterpNewPwDigit :: Text
sorryInterpNewPwDigit = "Passwords must contain at least one digit."


sorryInterpNewPwExcessArgs :: Text
sorryInterpNewPwExcessArgs = "Passwords may not contain whitespace."


sorryInterpNewPwLen :: Text
sorryInterpNewPwLen = T.concat [ "Passwords must be "
                               , showText minPwLen
                               , "-"
                               , showText maxPwLen
                               , " characters in length." ]


sorryInterpNewPwLower :: Text
sorryInterpNewPwLower = "Passwords must contain at least one lowercase character."


sorryInterpNewPwMatch :: Text
sorryInterpNewPwMatch = "Passwords do not match."


sorryInterpNewPwUpper :: Text
sorryInterpNewPwUpper = "Passwords must contain at least one uppercase character."

-----


sorryInterpPager :: Text
sorryInterpPager = T.concat [ "Enter a blank line or "
                            , dblQuote "n"
                            , " for the next page, "
                            , dblQuote "b"
                            , " for the previous page, or "
                            , dblQuote "q"
                            , " to stop reading." ]


-----


sorryInterpPW :: Text
sorryInterpPW = "Incorrect password."


sorryInterpPwBoot :: Text
sorryInterpPwBoot = "You are being booted due to excessive incorrect passwords."


sorryInterpPwBanned :: Sing -> Text
sorryInterpPwBanned s = colorWith bootMsgColor $ s <> " has been banned from CurryMUD!"


sorryInterpPwLoggedIn :: Sing -> Text
sorryInterpPwLoggedIn s = s <> " is already logged in."


-----


sorryIntroAlready :: Text -> Text
sorryIntroAlready n = prd $ "You've already introduced yourself to " <> n


sorryIntroCoin :: Text
sorryIntroCoin = can't "introduce yourself to a coin."


sorryIntroInEq :: Text
sorryIntroInEq = can't "introduce yourself to an item in your readied equipment."


sorryIntroInInv :: Text
sorryIntroInInv = can't "introduce yourself to an item in your inventory."


sorryIntroNoOneHere :: Text
sorryIntroNoOneHere = "You don't see anyone here to introduce yourself to."


sorryIntroType :: Sing -> Text
sorryIntroType s = prd $ can't "introduce yourself to " <> theOnLower s


-----


sorryLinkAlready :: Text -> Text -> Text
sorryLinkAlready t n = T.concat [ "You've already established a ", t, " link with ", n, "." ]


sorryLinkCoin :: Text
sorryLinkCoin = can't "establish a telepathic link with a coin."


sorryLinkInEq :: Text
sorryLinkInEq = can't "establish a telepathic link with an item in your readied equipment."


sorryLinkInInv :: Text
sorryLinkInInv = can't "establish a telepathic link with an item in your inventory."


sorryLinkIntroSelf :: Sing -> Text
sorryLinkIntroSelf s = prd $ "You must first introduce yourself to " <> s


sorryLinkIntroTarget :: Text -> Text
sorryLinkIntroTarget n = "You don't know the " <> n <> "'s name."


sorryLinkNoOneHere :: Text
sorryLinkNoOneHere = "You don't see anyone here to link with."


sorryLinkType :: Sing -> Text
sorryLinkType s = prd $ can't "establish a telepathic link with " <> theOnLower s


-----


sorryLoggedOut :: Sing -> Text
sorryLoggedOut s = s <> " is not logged in."


-----


sorryLookEmptyRmNoHooks :: Text
sorryLookEmptyRmNoHooks = "You don't see anything to look at here."


sorryLookEmptyRmWithHooks :: Text
sorryLookEmptyRmWithHooks = "You don't see anything to look at on the ground here."


-----


sorryMsgIncog :: Text
sorryMsgIncog = can't "send a message to a player who is logged in while you are incognito."


-----


sorryNewChanExisting :: ChanName -> Text
sorryNewChanExisting cn = prd $ "You are already connected to a channel named " <> dblQuote cn


sorryNewChanName :: Text -> Text -> Text
sorryNewChanName a msg = T.concat [ dblQuote a, " is not a legal channel name ", parensQuote msg, "." ]


-----


sorryNoAdmins :: Text
sorryNoAdmins = "No administrators exist!"


-----


sorryNoConHere :: Text
sorryNoConHere = "You don't see any containers here."


-----


sorryNoOneHere :: Text
sorryNoOneHere = "You don't see anyone here."


-----


sorryNonexistentId :: Id -> [Text] -> Text
sorryNonexistentId i ts = T.concat [ "ID ", showText i, " does not exist in ", case ts of
  [t] -> "the " <> t <> " table."
  _   -> prd $ "any of the following tables: " <> commas ts ]


-----


sorryNotPossessed :: Sing -> CmdName -> Text
sorryNotPossessed s cn = T.concat [ "You must first possess "
                                  , theOnLower s
                                  , " before you can use the "
                                  , dblQuote cn
                                  , " command." ]


-----


sorryNoLinks :: Text
sorryNoLinks = "You haven't established a telepathic link with anyone."


-----


sorryPCName :: Text -> Text
sorryPCName n = prd $ "There is no PC by the name of " <> dblQuote n


sorryPCNameLoggedIn :: Text -> Text
sorryPCNameLoggedIn n = "No PC by the name of " <> dblQuote n <> " is currently logged in."


-----


sorryParseAmt :: Text -> Text
sorryParseAmt a = dblQuote a <> " is not a valid amount."


sorryParseArg :: Text -> Text
sorryParseArg a = dblQuote a <> " is not a valid argument."


sorryParseBase :: Text -> Text
sorryParseBase a = dblQuote a <> " is not a valid base."


sorryParseChanId :: Text -> Text
sorryParseChanId a = dblQuote a <> " is not a valid channel ID."


sorryParseId :: Text -> Text
sorryParseId a = dblQuote a <> " is not a valid ID."


sorryParseInOut :: Text -> Text -> Text
sorryParseInOut value n = T.concat [ dblQuote value
                                   , " is not a valid value for the "
                                   , dblQuote n
                                   , " setting. Please specify one of the following: "
                                   , inOutOrOnOff
                                   , "." ]
  where
    inOutOrOnOff = T.concat [ dblQuote "in"
                            , "/"
                            , dblQuote "out"
                            , " or "
                            , dblQuote "on"
                            , "/"
                            , dblQuote "off" ]


sorryParseIndent :: Text -> Text
sorryParseIndent a = dblQuote a <> " is not a valid width amount."


sorryParseLineLen :: Text -> Text
sorryParseLineLen a = dblQuote a <> " is not a valid line length."


sorryParseMouthfuls :: Text -> Text
sorryParseMouthfuls a = dblQuote a <> " is not a valid number of mouthfuls."


sorryParseNum :: Text -> Text -> Text
sorryParseNum numTxt base = T.concat [ dblQuote numTxt, " is not a valid number in base ", base, "." ]


sorryParseOnOff :: Text -> Text -> Text
sorryParseOnOff value n = T.concat [ dblQuote value
                                   , " is not a valid value for the "
                                   , dblQuote n
                                   , " setting. Please specify "
                                   , dblQuote "on"
                                   , " or "
                                   , dblQuote "off"
                                   , "." ]


sorryParseSetting :: Text -> Text -> Text
sorryParseSetting value name = T.concat [ dblQuote value, " is not a valid value for the ", dblQuote name, " setting." ]


-----


sorryPeepAdmin :: Text
sorryPeepAdmin = can't "peep an admin."


sorryPeepSelf :: Text
sorryPeepSelf = can't "peep yourself."


-----


sorryPickNotFlower :: Text -> Text
sorryPickNotFlower t = prd $ can't "pick " <> aOrAn t


sorryPickInEq :: Text
sorryPickInEq = can't "pick an item in your readied equipment."


sorryPickInInv :: Text
sorryPickInInv = can't "pick an item in your inventory."


-----



sorryPossessType :: Sing -> Text
sorryPossessType s = prd $ can't "possess " <> aOrAn s


-----


sorryPp :: Text -> Text
sorryPp t = prd $ "You don't have enough psionic energy to " <> t


-----


sorryPutEmptyRmWithHooks :: Text -> Text
sorryPutEmptyRmWithHooks t = "You don't see " <> aOrAn t <> " here."


sorryPutExcessCon :: Text
sorryPutExcessCon = "You can only put things into one container at a time."


sorryPutInCoin :: Text
sorryPutInCoin = can't "put something inside a coin."


sorryPutInEq :: Text
sorryPutInEq = butCan't "put an item in your readied equipment into a container. Please unready the item(s) first."


sorryPutInRm :: Text
sorryPutInRm = butCan't "put item in your current room into a container. Please pick up the item(s) first."


sorryPutInsideSelf :: Sing -> Text
sorryPutInsideSelf s = can't "put the " <> s <> " inside itself."


sorryPutVol :: Sing -> Text
sorryPutVol s = "The " <> s <> " is too full to contain "


-----


sorryQuitCan'tAbbrev :: Text
sorryQuitCan'tAbbrev = T.concat [ "The "
                                , dblQuote "quit"
                                , " command may not be abbreviated. Type "
                                , dblQuote "quit"
                                , " with no arguments to quit CurryMUD." ]


-----


sorryQuoteChars :: Text
sorryQuoteChars = "Unbalanced quote character."


-----


sorryReadCoins :: Text
sorryReadCoins = can't "read a coin."


sorryReadInEq :: Text
sorryReadInEq = can't "read an item in your readied equipment."


sorryReadLang :: Sing -> Lang -> Text
sorryReadLang s lang = T.concat [ "Although you recognize that the text on the "
                                , s
                                , " is written in "
                                , pp lang
                                , ", you can't make heads or tails of it." ]


sorryReadNoHooks :: Text
sorryReadNoHooks = "You don't see anything to read here. " <>
                   parensQuote "If you'd like to read an item on the ground, please pick up the item first."


sorryReadOrigLang :: Lang -> Text
sorryReadOrigLang lang = "Most unfortunately, as you don't know " <> pp lang <> ", you cannot read the text."


sorryReadType :: Sing -> Text
sorryReadType s = prd $ can't "read the " <> s


sorryReadUnknownLang :: Sing -> Text
sorryReadUnknownLang s = "The text written on the " <> s <> " is in a language you don't recognize."


sorryReadWithHooks :: Text -> Text
sorryReadWithHooks t = dblQuote t                                                      <>
                       " does not match the name of a readable fixture of this room. " <>
                       parensQuote "If you'd like to read an item on the ground, please pick up the item first."


-----


sorryReadyAlreadyWearing :: Text -> Text
sorryReadyAlreadyWearing t = prd $ "You're already wearing " <> aOrAn t


sorryReadyAlreadyWearingRing :: Slot -> Sing -> Text
sorryReadyAlreadyWearingRing sl s = T.concat [ "You're already wearing "
                                             , aOrAn s
                                             , " on your "
                                             , pp sl
                                             , "." ]


sorryReadyAlreadyWielding :: Sing -> Slot -> Text
sorryReadyAlreadyWielding s sl = T.concat [ "You're already wielding ", aOrAn s, " with your ", pp sl, "." ]


sorryReadyAlreadyWieldingTwoHanded :: Text
sorryReadyAlreadyWieldingTwoHanded = "You're already wielding a two-handed weapon."


sorryReadyAlreadyWieldingTwoWpns :: Text
sorryReadyAlreadyWieldingTwoWpns = "You're already wielding two weapons."


sorryReadyClothFull :: Text -> Text
sorryReadyClothFull t = can't "wear any more " <> t <> "s."


sorryReadyClothFullOneSide :: Cloth -> Slot -> Text
sorryReadyClothFullOneSide (pp -> c) (pp -> s) = can't . T.concat $ [ "wear any more ", c, "s on your ", s, "." ]


sorryReadyCoins :: Text
sorryReadyCoins = can't "ready coins."


sorryReadyInEq :: Text
sorryReadyInEq = can't "ready an item that's already in your readied equipment."


sorryReadyInRm :: Text
sorryReadyInRm = butCan't "ready an item in your current room. Please pick up the item(s) first."


sorryReadyRol :: Sing -> RightOrLeft -> Text
sorryReadyRol s rol = can't . T.concat $ [ "wear ", aOrAn s, " on your ", pp rol, "." ]


sorryReadyType :: Sing -> Text
sorryReadyType s = prd $ can't "ready " <> aOrAn s


sorryReadyWpnHands :: Sing -> Text
sorryReadyWpnHands s = prd $ "Both hands are required to wield the " <> s


sorryReadyWpnRol :: Sing -> Text
sorryReadyWpnRol s = can't "wield " <> aOrAn s <> " with your finger!"


-----


sorryRegPlaName :: Text -> Text
sorryRegPlaName n = prd $ "There is no regular player by the name of " <> dblQuote n


-----


sorryRemEmpty :: Sing -> Text
sorryRemEmpty s = "The " <> s <> " is empty."


sorryRemEnc :: Text
sorryRemEnc = sorryGetEnc


sorryRemCoin :: Text
sorryRemCoin = can't "remove something from a coin."


sorryRemExcessCon :: Text
sorryRemExcessCon = "You can only remove things from one container at a time."


sorryRemIgnore :: Text
sorryRemIgnore = sorryIgnoreLocPrefPlur "The names of the items to be removed from a container"


-----


sorrySayCoins :: Text
sorrySayCoins = "You're talking to coins now?"


sorrySayExcessTargets :: Text
sorrySayExcessTargets = but "you can only say something to one person at a time."


sorrySayInEq :: Text
sorrySayInEq = can't "talk to an item in your readied equipment. Try saying something to someone in your current room."


sorrySayInInv :: Text
sorrySayInInv = can't "talk to an item in your inventory. Try saying something to someone in your current room."


sorrySayNoOneHere :: Text
sorrySayNoOneHere = "You don't see anyone here to talk to."


sorrySayTargetType :: Sing -> Text
sorrySayTargetType s = prd $ can't "talk to " <> aOrAn s


-----


sorrySearch :: Text
sorrySearch = "No matches found."


-----


sorrySetName :: Text -> Text
sorrySetName n = dblQuote n <> " is not a valid setting name."


sorrySetRange :: Text -> Int -> Int -> Text
sorrySetRange settingName minVal maxVal = T.concat [ capitalize settingName
                                                   , " must be between "
                                                   , showText minVal
                                                   , " and "
                                                   , showText maxVal
                                                   , "." ]


-----


sorryShowExcessTargets :: Text
sorryShowExcessTargets = but "you can only show something to one person at a time."


sorryShowInRm :: Text
sorryShowInRm = can't "show an item in your current room."


sorryShowTarget :: Text -> Text
sorryShowTarget t = prd $ can't "show something to " <> aOrAn t


-----


sorrySmellEmptyRmNoHooks :: Text
sorrySmellEmptyRmNoHooks = "You don't see anything to smell here."


sorrySmellEmptyRmWithHooks :: Text
sorrySmellEmptyRmWithHooks = "You don't see anything to smell on the ground here."


sorrySmellExcessTargets :: Text
sorrySmellExcessTargets = but "you can only smell one thing at a time."


sorrySmellNothingToSmell :: Text
sorrySmellNothingToSmell = "There isn't anything to smell."


sorrySmellRmCoins :: (Text, Bool) -> Text
sorrySmellRmCoins (coinTxt, isPlur) = T.concat [ "You must pick up the "
                                               , coinTxt
                                               , " before you can smell "
                                               , isPlur ? "them" :? "it"
                                               , "." ]


sorrySmellRmNoHooks :: Sing -> Text
sorrySmellRmNoHooks s = "You must pick up the " <> s <> " before you can smell it."


-----


sorryStopActName :: Text -> Text
sorryStopActName t = T.concat [ dblQuote t
                              , " is not the name of an activity that can be stopped. Please type "
                              , colorWith quoteColor "stop"
                              , " followed by one of the following: "
                              , colorWith quoteColor "moving"
                              , ", "
                              , colorWith quoteColor "eating"
                              , ", "
                              , colorWith quoteColor "drinking"
                              , ", "
                              , colorWith quoteColor "attacking"
                              , ", or "
                              , colorWith quoteColor "all"
                              , "." ]


sorryStopNotDoing :: ActType -> Text
sorryStopNotDoing actType = let helper t = T.concat [ "You're not ", pp actType, " ", t, " at the moment." ]
                            in helper . onTrue (actType == Moving) (const "anywhere") $ "anything"


sorryStopNotDoingAnything :: Text
sorryStopNotDoingAnything = "You're not doing anything that can be stopped "     <>
                            parensQuote "moving, eating, drinking, or attacking" <>
                            "."


-----


sorrySudoerDemoteRoot :: Text
sorrySudoerDemoteRoot = can't "demote Root."


sorrySudoerDemoteSelf :: Text
sorrySudoerDemoteSelf = can't "demote yourself."


-----


sorrySummonAdmin :: Text
sorrySummonAdmin = butCan't "summon an admin."


sorrySummonAlready :: Sing -> Text
sorrySummonAlready s = s <> " is already here!"


sorrySummonSelf :: Text
sorrySummonSelf = can't "summon yourself."


-----


sorryTasteExcessTargets :: Text
sorryTasteExcessTargets = but "you can only taste one thing at a time."


sorryTasteInRm :: Text
sorryTasteInRm = butCan't "taste an item in your current room. Please pick up the item first."


sorryTasteNothingToTaste :: Text
sorryTasteNothingToTaste = "There isn't anything to taste."


sorryTasteType :: Sing -> Text
sorryTasteType s = prd $ can't "taste " <> aOrAn s


-----


sorryTeleAlready :: Text
sorryTeleAlready = "You're already there!"


sorryTeleLoggedOutRm :: Text
sorryTeleLoggedOutRm = can't "teleport to the logged out room."


sorryTeleRmName :: Text -> Text
sorryTeleRmName n = T.concat [ dblQuote n
                             , " is not a valid room name. Type "
                             , colorWith quoteColor . prefixAdminCmd $ "telerm"
                             , " with no arguments to get a list of valid room names." ]


sorryTeleSelf :: Text
sorryTeleSelf = can't "teleport to yourself."


-----


sorryTrashInEq :: Text
sorryTrashInEq = butCan't "dispose of an item in your readied equipment. Please unready the item(s) first."


sorryTrashInRm :: Text
sorryTrashInRm = butCan't "dispose of an item in your current room. Please pick up the item(s) first."


-----


sorryTuneName :: Text -> Text
sorryTuneName n = prd $ "You don't have a connection by the name of " <> dblQuote n


-----


sorryTunedOutICChan :: ChanName -> Text
sorryTunedOutICChan = sorryTunedOutChan "tune" DoQuote


sorryTunedOutChan :: CmdName -> ShouldQuote -> Text -> Text
sorryTunedOutChan x sq y = T.concat [ "You have tuned out the "
                                    , onTrue (sq == DoQuote) dblQuote y
                                    , " channel. Type "
                                    , colorWith quoteColor . T.concat $ [ x, " ", y, "=in" ]
                                    , " to tune it back in." ]


sorryTunedOutOOCChan :: Text -> Text
sorryTunedOutOOCChan = sorryTunedOutChan "set" Don'tQuote


sorryTunedOutPCSelf :: Sing -> Text
sorryTunedOutPCSelf s = prd $ "You have tuned out " <> s


sorryTunedOutPCTarget :: Sing -> Text
sorryTunedOutPCTarget s = s <> " has tuned you out."


-----


sorryTwoWayLink :: Text -> Text
sorryTwoWayLink t = prd $ "You haven't established a two-way telepathic link with anyone named " <> dblQuote t


sorryTwoWayTargetName :: ExpCmdName -> Sing -> Text
sorryTwoWayTargetName cn s = T.concat [ "In a telepathic message to "
                                      , s
                                      , ", the only possible target is "
                                      , s
                                      , ". Please try "
                                      , colorWith quoteColor . T.concat $ [ T.singleton expCmdChar
                                                                          , cn
                                                                          , " "
                                                                          , T.singleton . toLower . T.head $ s ]
                                      , " instead." ]


-----


sorryUnlinkIgnore :: Text
sorryUnlinkIgnore = sorryIgnoreLocPrefPlur "The names of the people with whom you would like to unlink"


sorryUnlinkName :: Text -> Text
sorryUnlinkName t = prd $ "You don't have a link with " <> dblQuote t


-----


sorryUnreadyCoins :: Text
sorryUnreadyCoins = can't "unready coins."


sorryUnreadyInInv :: Text
sorryUnreadyInInv = can't "unready an item in your inventory."


sorryUnreadyInRm :: Text
sorryUnreadyInRm = can't "unready an item in your current room."


-----


sorryWhisperCoins :: Text
sorryWhisperCoins = "You're whispering to coins now?"


sorryWhisperExcessTargets :: Text
sorryWhisperExcessTargets = but "you can only whisper something to one person at a time."


sorryWhisperInEq :: Text
sorryWhisperInEq = can't "whisper to an item in your readied equipment. Try whispering something to someone in your current room."


sorryWhisperInInv :: Text
sorryWhisperInInv = can't "whisper to an item in your inventory. Try whispering something to someone in your current room."


sorryWhisperNoOneHere :: Text
sorryWhisperNoOneHere = "You don't see anyone here to whisper to."


sorryWhisperTargetType :: Sing -> Text
sorryWhisperTargetType s = prd $ can't "whisper to " <> aOrAn s


-----


sorryWireAlready :: ChanName -> Text
sorryWireAlready cn = "As you are already connected to the " <> dblQuote cn <> " channel, there is no need to tap it."


-----


sorryWrapLineLen :: Text
sorryWrapLineLen = T.concat [ "The line length must be between "
                            , showText minCols
                            , " and "
                            , showText maxCols
                            , " characters." ]


-----


sorryWtf :: Text
sorryWtf = colorWith wtfColor "He don't."


-----


sorryWut :: Text -> Text
sorryWut t = prd $ "I don't understand " <> dblQuote t
