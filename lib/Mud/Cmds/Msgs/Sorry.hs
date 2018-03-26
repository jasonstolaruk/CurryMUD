{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.Cmds.Msgs.Sorry ( sorryActing
                           , sorryAdminChanSelf
                           , sorryAdminChanTargetName
                           , sorryAdminName
                           , sorryAdminPasswordAdmin
                           , sorryAdminPasswordSelf
                           , sorryAdminSetKey
                           , sorryAdminSetOp
                           , sorryAdminSetType
                           , sorryAdminSetValue
                           , sorryAlreadyPossessed
                           , sorryAlreadyPossessing
                           , sorryAsAdHoc
                           , sorryAsAdmin
                           , sorryAsSelf
                           , sorryAsType
                           , sorryAttackCan'tAbbrev
                           , sorryAttackCoins
                           , sorryAttackInEq
                           , sorryAttackInInv
                           , sorryAttackNothingHere
                           , sorryBanAdHoc
                           , sorryBanAdmin
                           , sorryBanSelf
                           , sorryBonusAdmin
                           , sorryBonusCount
                           , sorryBonusIgnore
                           , sorryBonusLvl
                           , sorryBonusName
                           , sorryBonusSelf
                           , sorryBonusTime
                           , sorryBootAdmin
                           , sorryBootSelf
                           , sorryBracketedMsg
                           , sorryChanIncog
                           , sorryChanMsg
                           , sorryChanName
                           , sorryChanNoOneListening
                           , sorryChanTargetName
                           , sorryChanTargetNameFromContext
                           , sorryCloneType
                           , sorryCmdNotFound
                           , sorryCon
                           , sorryConInEq
                           , sorryConnectAlready
                           , sorryConnectChanName
                           , sorryConnectIgnore
                           , sorryDestroyType
                           , sorryDisconnectIgnore
                           , sorryDrinkCoins
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
                           , sorryEatCoins
                           , sorryEatExcessTargets
                           , sorryEatInEq
                           , sorryEatInRm
                           , sorryEatMouthfuls
                           , sorryEatType
                           , sorryEating
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
                           , sorryExpCmdActing
                           , sorryExpCmdBlush
                           , sorryExpCmdCoins
                           , sorryExpCmdIllegalTarget
                           , sorryExpCmdInInvEq
                           , sorryExpCmdLen
                           , sorryExpCmdName
                           , sorryExpCmdRequiresTarget
                           , sorryExpCmdTargetActing
                           , sorryExpCmdTargetType
                           , sorryExtinguishCoins
                           , sorryExtinguishInRm
                           , sorryExtinguishLight
                           , sorryExtinguishNotLit
                           , sorryExtinguishType
                           , sorryFillAlready
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
                           , sorryInterpNameApostropheCount
                           , sorryInterpNameApostrophePosition
                           , sorryInterpNameDict
                           , sorryInterpNameExcessArgs
                           , sorryInterpNameIllegal
                           , sorryInterpNameLen
                           , sorryInterpNameLenApostrophe
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
                           , sorryInterpPW
                           , sorryInterpPager
                           , sorryInterpPickPtsMax
                           , sorryInterpPickPtsMin
                           , sorryInterpPickPtsPts
                           , sorryInterpPickPtsQuit
                           , sorryInterpPwBanned
                           , sorryInterpPwBoot
                           , sorryInterpPwDead
                           , sorryInterpPwLoggedIn
                           , sorryIntroAlready
                           , sorryIntroCoin
                           , sorryIntroInEq
                           , sorryIntroInInv
                           , sorryIntroNoOneHere
                           , sorryIntroType
                           , sorryKillAdHoc
                           , sorryKillAdmin
                           , sorryKillAsleep
                           , sorryKillDead
                           , sorryKillSpirit
                           , sorryKillType
                           , sorryLightCoins
                           , sorryLightExcessFires
                           , sorryLightExcessLights
                           , sorryLightFireCoins
                           , sorryLightFireInRm
                           , sorryLightFireType
                           , sorryLightFireUnlit
                           , sorryLightInRm
                           , sorryLightLampSecs
                           , sorryLightLightType
                           , sorryLightLit
                           , sorryLightSelf
                           , sorryLightTinderbox
                           , sorryLightTorchSecs
                           , sorryLinkAlready
                           , sorryLinkCoin
                           , sorryLinkInEq
                           , sorryLinkInInv
                           , sorryLinkIntroSelf
                           , sorryLinkIntroTarget
                           , sorryLinkNoOneHere
                           , sorryLinkSpirit
                           , sorryLinkType
                           , sorryLoggedOut
                           , sorryLookEmptyRmNoHooks
                           , sorryLookEmptyRmWithHooks
                           , sorryMkFoodAmt
                           , sorryMkFoodName
                           , sorryMkHolyAmt
                           , sorryMkHolyGodName
                           , sorryMsgIncog
                           , sorryNeat
                           , sorryNewChanExisting
                           , sorryNewChanName
                           , sorryNoAdmins
                           , sorryNoConHere
                           , sorryNoLinks
                           , sorryNoLinksSpirit
                           , sorryNoOneHere
                           , sorryNonexistentId
                           , sorryNotPossessed
                           , sorryPCName
                           , sorryPCNameLoggedIn
                           , sorryParseAmt
                           , sorryParseArg
                           , sorryParseBase
                           , sorryParseChanId
                           , sorryParseId
                           , sorryParseInOut
                           , sorryParseIndent
                           , sorryParseLineLen
                           , sorryParseMouthfuls
                           , sorryParseNum
                           , sorryParseOnOff
                           , sorryParseSeconds
                           , sorryParseSetting
                           , sorryParseZoom
                           , sorryPeepAdmin
                           , sorryPeepSelf
                           , sorryPickInEq
                           , sorryPickInInv
                           , sorryPickNotFlower
                           , sorryPossessRm
                           , sorryPossessType
                           , sorryPp
                           , sorryPutEmptyRmWithHooks
                           , sorryPutExcessCon
                           , sorryPutInCoin
                           , sorryPutInEq
                           , sorryPutInRm
                           , sorryPutInsideSelf
                           , sorryPutLitLight
                           , sorryPutVol
                           , sorryQuitCan'tAbbrev
                           , sorryQuoteChars
                           , sorryReadCoins
                           , sorryReadHolySymbol
                           , sorryReadInEq
                           , sorryReadLang
                           , sorryReadNoHooks
                           , sorryReadOrigLang
                           , sorryReadType
                           , sorryReadUnknownLang
                           , sorryReadWithHooks
                           , sorryReadyAlreadyWearing
                           , sorryReadyAlreadyWearingRing
                           , sorryReadyClothFull
                           , sorryReadyClothFullOneSide
                           , sorryReadyCoins
                           , sorryReadyHand
                           , sorryReadyHandsFull
                           , sorryReadyHolySymbolRhayk
                           , sorryReadyInEq
                           , sorryReadyInRm
                           , sorryReadyLight
                           , sorryReadyRol
                           , sorryReadyType
                           , sorryReadyWpnHands
                           , sorryReadyWpnRol
                           , sorryRefuelAlready
                           , sorryRefuelExcessLamps
                           , sorryRefuelExcessVessels
                           , sorryRefuelLampCoins
                           , sorryRefuelLampInRm
                           , sorryRefuelLampType
                           , sorryRefuelLiq
                           , sorryRefuelVesselCoins
                           , sorryRefuelVesselEmpty
                           , sorryRefuelVesselInEq
                           , sorryRefuelVesselInRm
                           , sorryRegPlaName
                           , sorryRemCoin
                           , sorryRemEmpty
                           , sorryRemEnc
                           , sorryRemExcessCon
                           , sorryRemIgnore
                           , sorrySacrificeCorpse
                           , sorrySacrificeCorpseCoins
                           , sorrySacrificeCorpseExcessTargets
                           , sorrySacrificeCorpseInEq
                           , sorrySacrificeCorpseInInv
                           , sorrySacrificeCorpseType
                           , sorrySacrificeHolySymbol
                           , sorrySacrificeHolySymbolCoins
                           , sorrySacrificeHolySymbolCorpse
                           , sorrySacrificeHolySymbolExcessTargets
                           , sorrySacrificeHolySymbolInEq
                           , sorrySacrificeHolySymbolInRm
                           , sorrySacrificeHolySymbolType
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
                           , sorrySmellLitLight
                           , sorrySmellNothingToSmell
                           , sorrySmellRmCoins
                           , sorrySmellRmNoHooks
                           , sorrySmellTasteSlot
                           , sorryStanceAlready
                           , sorryStanceName
                           , sorryStopActName
                           , sorryStopNotDoing
                           , sorryStopNotDoingAnything
                           , sorrySudoerAdHoc
                           , sorrySudoerDemoteRoot
                           , sorrySudoerDemoteSelf
                           , sorrySudoerSpirit
                           , sorrySuicide
                           , sorrySummonAdHoc
                           , sorrySummonAdmin
                           , sorrySummonAlready
                           , sorrySummonSelf
                           , sorryTasteExcessTargets
                           , sorryTasteInRm
                           , sorryTasteLitLight
                           , sorryTasteNothingToTaste
                           , sorryTasteType
                           , sorryTeleAlready
                           , sorryTeleLoggedOutRm
                           , sorryTeleNecropolis
                           , sorryTeleRmName
                           , sorryTeleSelf
                           , sorryTeleWelcomeRm
                           , sorryTimeNotOutside
                           , sorryTimeUnknown
                           , sorryTrashInEq
                           , sorryTrashInRm
                           , sorryTuneName
                           , sorryTunedOutChan
                           , sorryTunedOutICChan
                           , sorryTunedOutOOCChan
                           , sorryTunedOutPCSelf
                           , sorryTunedOutPCTarget
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

import           Mud.Cmds.Util.CmdPrefixes
import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Misc.ANSI
import           Mud.Misc.Misc
import           Mud.TopLvlDefs.Chars
import           Mud.TopLvlDefs.Misc
import qualified Mud.Util.Misc as U (pmf)
import           Mud.Util.Misc hiding (pmf)
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Data.Char (toLower)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

pmf :: PatternMatchFail
pmf = U.pmf "Mud.Cmds.Msgs.Sorry"

-- ==================================================

sorryActing :: Text -> ActType -> Text
sorryActing t act = prd . can't $ t <> " while " <> pp act

can't :: Text -> Text
can't = ("You can't " <>)

-----

sorryIgnoreLocPref :: Text -> Text
sorryIgnoreLocPref msg = parensQuote $ msg <> " need not be given a location prefix. The location prefix you provided \
                                              \will be ignored."

sorryIgnoreLocPrefPlur :: Text -> Text
sorryIgnoreLocPrefPlur msg = parensQuote $ msg <> " need not be given location prefixes. The location prefixes you \
                                                  \provided will be ignored."

-----

sorryInterpPickPtsMax :: Text -> Text
sorryInterpPickPtsMax t = can't "add any more to " <> t <> " (it's set to the maximum value of 100)."

sorryInterpPickPtsMin :: Text -> Text
sorryInterpPickPtsMin t = can't "subtract any more from " <> t <> " (it's set to the minimum value of 10)."

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

sorryAdminName :: Text -> Text
sorryAdminName n = prd $ "There is no administrator by the name of " <> dblQuote n

-----

sorryAdminPasswordAdmin :: Text
sorryAdminPasswordAdmin = can't "change an admin's password."

sorryAdminPasswordSelf :: Text
sorryAdminPasswordSelf = "Please use the \"password\" command to change your own password."

-----

sorryAdminSetKey :: Text -> Text
sorryAdminSetKey k = dblQuote k <> " is not a valid key."

sorryAdminSetOp :: Text -> Text -> Text
sorryAdminSetOp opTxt k = T.concat [ "The ", dblQuote opTxt, " operator cannot be used with the ", dblQuote k, " key." ]

sorryAdminSetType :: Id -> Text
sorryAdminSetType i = "ID " <> showTxt i <> " is not of the correct type."

sorryAdminSetValue :: Text -> Text -> Text
sorryAdminSetValue k v = T.concat [ v, " is not a valid value for the ", dblQuote k, " key." ]

-----

sorryAlreadyPossessed :: Sing -> Sing -> Text
sorryAlreadyPossessed a b = but . T.concat $ [ theOnLower a, " is already possessed by ", b, "." ]

-----

sorryAlreadyPossessing :: Sing -> Text
sorryAlreadyPossessing s = prd $ "You are already possessing " <> theOnLower s

-----

sorryAsAdHoc :: Text
sorryAsAdHoc = can'tTarget "an ad-hoc PC" <> withAs

sorryAsAdmin :: Text
sorryAsAdmin = can'tTarget "an admin" <> withAs

can'tTarget :: Text -> Text
can'tTarget = can't . ("target " <>)

withAs :: Text
withAs = " with the \"" <> prefixAdminCmd "as" <> "\" command."

sorryAsSelf :: Text
sorryAsSelf = can'tTarget "yourself" <> withAs

sorryAsType :: Sing -> Text
sorryAsType s = can'tTarget $ aOrAn s <> withAs

-----

sorryAttackCan'tAbbrev :: Text
sorryAttackCan'tAbbrev = "The \"attack\" command may not be abbreviated."

sorryAttackCoins :: Text
sorryAttackCoins = can't "attack a coin."

sorryAttackInEq :: Text
sorryAttackInEq = can't "attack an item in your readied equipment."

sorryAttackInInv :: Text
sorryAttackInInv = can't "attack an item in your inventory."

sorryAttackNothingHere :: Text
sorryAttackNothingHere = "You don't see anything to attack here."

-----

sorryBanAdHoc :: Text
sorryBanAdHoc = can't "ban an ad-hoc PC."

sorryBanAdmin :: Text
sorryBanAdmin = can't "ban an admin."

sorryBanSelf :: Text
sorryBanSelf = can't "ban yourself."

-----

sorryBonusAdmin :: Text
sorryBonusAdmin = can't "give a bonus to an administrator."

sorryBonusCount :: Sing -> Text
sorryBonusCount s = prd $ "You have already given the maximum number of 5 bonuses to " <> s

sorryBonusIgnore :: Text
sorryBonusIgnore = sorryIgnoreLocPref "The name of the character whose player you would like to give a bonus to"

sorryBonusLvl :: Text
sorryBonusLvl = "You must be at least level 3 to give a bonus."

sorryBonusName :: Text -> Text
sorryBonusName n = prd $ "You don't know anyone by the name of " <> dblQuote n

sorryBonusSelf :: Text
sorryBonusSelf = can't "give a bonus to yourself."

sorryBonusTime :: Text
sorryBonusTime = "It's too early since you last gave a bonus."

-----

sorryBootAdmin :: Text -> Text
sorryBootAdmin t = t <> " is an admin and cannot be booted."

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
sorryChanTargetName cn n = T.concat [ "There is no one by the name of ", dblQuote n, " currently tuned in to the ", cn
                                    , " channel." ]

sorryChanTargetNameFromContext :: Text -> ChanContext -> Text
sorryChanTargetNameFromContext n ChanContext { .. } = sorryChanTargetName effChanName n
  where
    effChanName = maybe someCmdName dblQuote someChanName

-----

sorryCloneType :: Type -> Text
sorryCloneType t = prd . can't $ "clone " <> aOrAn (pp t)

-----

sorryCmdNotFound :: Text
sorryCmdNotFound = "What?"

-----

sorryCon :: Sing -> Text
sorryCon s = theOnLowerCap s <> " isn't a container."

sorryConInEq :: PutOrRem -> Text
sorryConInEq por =
    let (a, b) = case por of Put -> ("put",    "into")
                             Rem -> ("remove", "from")
    in butCan't . T.concat $ [ a, " an item ", b, " a container in your readied equipment. Please unready the container first." ]

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

sorryDestroyType :: Type -> Text
sorryDestroyType t = prd . can't $ "destroy " <> aOrAn (pp t)

-----

sorryDisconnectIgnore :: Text
sorryDisconnectIgnore = sorryIgnoreLocPrefPlur "The names of the people you would like to disconnect"

-----

sorryDrinkCoins :: Text
sorryDrinkCoins = can't "drink from a coin. Honestly I'm not quite sure why you'd want to do that in the first place."

sorryDrinkEmpty :: Sing -> Text
sorryDrinkEmpty = the' . (<> " is empty.")

sorryDrinkEmptyRmNoHooks :: Text
sorryDrinkEmptyRmNoHooks = "You don't see anything to drink from here."

sorryDrinkEmptyRmWithHooks :: Text
sorryDrinkEmptyRmWithHooks = "You don't see any vessels on the ground here."

sorryDrinkExcessTargets :: Text
sorryDrinkExcessTargets = but "you can only drink from one vessel at a time."

sorryDrinkInEq :: Text
sorryDrinkInEq = can't "drink from an item in your readied equipment."

sorryDrinkMouthfuls :: Text
sorryDrinkMouthfuls = can't "drink less that one mouthful."

sorryDrinkRmNoHooks :: Text
sorryDrinkRmNoHooks = butCan't "drink from a vessel in your current room. Please pick up the vessel first."

sorryDrinkRmWithHooks :: Text -> Text
sorryDrinkRmWithHooks t | a <- "If you'd like to drink from a vessel on the ground, please pick up the vessel first."
                        = T.concat [ "You don't see ", aOrAn t, " here. ", parensQuote a ]

sorryDrinkType :: Sing -> Text
sorryDrinkType s = prd $ can't "drink from " <> aOrAn s

-----

sorryDropInEq :: Text
sorryDropInEq = butCan't "drop an item in your readied equipment. Please unready the item(s) first."

sorryDropInRm :: Text
sorryDropInRm = can't "drop an item that's already in your current room. If you're intent on dropping it, try picking \
                      \it up first!"

-----

sorryEatCoins :: Text
sorryEatCoins = can't "eat a coin. Honestly I'm not quite sure why you'd want to do that in the first place."

sorryEatExcessTargets :: Text
sorryEatExcessTargets = but "you can only eat one food at a time."

sorryEatInEq :: Text
sorryEatInEq = can't "eat an item in your readied equipment."

sorryEatInRm :: Text
sorryEatInRm = butCan't "eat food in your current room. Please pick up the food first."

sorryEatMouthfuls :: Text
sorryEatMouthfuls = can't "eat less than one mouthful."

sorryEatType :: Sing -> Text
sorryEatType s = prd $ can't "eat " <> aOrAn s

sorryEating :: Text -> Text -> Sing -> Text
sorryEating a b s =
    T.concat [ "You can't ", a, " the ", s, " ", b', "while you're eating it. Please stop eating the ", s, " first." ]
  where
    b' = onTrue (()!# b) spcR b

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
sorryEmptyAlready = the' . (<> " is already empty.")

sorryEmptyCoins :: Text
sorryEmptyCoins = can't "empty a coin."

sorryEmptyCon :: Text
sorryEmptyCon = sorryEmptyHelper "container"

sorryEmptyHelper :: Text -> Text
sorryEmptyHelper t = butCan't . T.concat $ [ "empty a ", t
                                           , " with the \"empty\" command. Please use the \"remove\" command to remove \
                                             \items from a "
                                           , t, "." ]

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
                                 , dblQuote . showTxt $ a
                                 , " command to examine items in your "
                                 , loc a
                                 , ". To examine items in your "
                                 , loc b
                                 , ", use the "
                                 , dblQuote . showTxt $ b
                                 , " command." ]
  where
    loc = \case EquipCmd -> "readied equipment"
                InvCmd   -> "inventory"
                LookCmd  -> "current room"

-----

sorryExpCmdActing :: ActType -> Text
sorryExpCmdActing act = prd . can't $ "do that while " <> pp act

sorryExpCmdBlush :: Text -> Text
sorryExpCmdBlush r = but $ r <> "s can't visibly blush (the fur gets in the way)."

sorryExpCmdCoins :: Text
sorryExpCmdCoins = but "expressive commands cannot be used with coins."

sorryExpCmdInInvEq :: InInvEqRm -> Text
sorryExpCmdInInvEq loc = can'tTarget "an item in your " <> loc' <> " with an expressive command."
  where
    loc' = case loc of InEq  -> "readied equipment"
                       InInv -> "inventory"
                       _     -> pmf "sorryExpCmdInInvEq loc'" loc

sorryExpCmdLen :: Text
sorryExpCmdLen = "An expressive command sequence may not be more than 2 words long."

sorryExpCmdName :: Text -> Text
sorryExpCmdName cn = prd $ "There is no expressive command by the name of " <> dblQuote cn

sorryExpCmdIllegalTarget :: ExpCmdName -> Text
sorryExpCmdIllegalTarget = the' . (<> " expressive command cannot be used with a target.") . dblQuote

sorryExpCmdRequiresTarget :: ExpCmdName -> Text
sorryExpCmdRequiresTarget = the' . (<> " expressive command requires a single target.") . dblQuote

sorryExpCmdTargetActing :: ActType -> Text
sorryExpCmdTargetActing act = prd . can't $ "do that to a person who is " <> pp act

sorryExpCmdTargetType :: Text
sorryExpCmdTargetType = but "expressive commands can only target people."

-----

sorryExtinguishCoins :: Text
sorryExtinguishCoins = can't "extinguish a coin."

sorryExtinguishInRm :: Text
sorryExtinguishInRm = can't "extinguish a light source in your current room. Please pick up the light source first."

sorryExtinguishLight :: Text
sorryExtinguishLight = "You don't have a lit source of light in your readied equipment."

sorryExtinguishNotLit :: Sing -> Text
sorryExtinguishNotLit s = the' s <> " is not lit."

sorryExtinguishType :: Sing -> Text
sorryExtinguishType s = prd $ can't "extinguish " <> aOrAn s

-----

sorryFillAlready :: Sing -> Text
sorryFillAlready = the' . (<> " is already full.")

sorryFillCoins :: Text
sorryFillCoins = can't "fill a coin."

sorryFillEmptyRmNoHooks :: Text
sorryFillEmptyRmNoHooks = "You don't see a source of liquid here."

sorryFillEmptyRmWithHooks :: Text
sorryFillEmptyRmWithHooks = "You don't see any vessels on the ground here."

sorryFillEmptySource :: Sing -> Text
sorryFillEmptySource = the' . (<> " is empty.")

sorryFillExcessSources :: Text
sorryFillExcessSources = but "you can only specify a single liquid source at a time."

sorryFillInEq :: Text
sorryFillInEq = can't "fill an item in your readied equipment."

sorryFillInRm :: Text
sorryFillInRm = butCan't "fill a vessel in your current room. Please pick up the vessel(s) first."

sorryFillLiqTypes :: BothGramNos -> BothGramNos -> Text
sorryFillLiqTypes a@(as, _) b@(bs, _) = helper $ if
  | a == b    -> the' . mkPlurFromBoth $ a
  | otherwise -> T.concat [ "The ", as, " and the ", bs ]
  where
    helper = (<> " do not contain the same kind of liquid.")

sorryFillRmNoHooks :: Text
sorryFillRmNoHooks = butCan't "fill something with the contents of a vessel in your current room. Please pick up the \
                              \vessel first."

sorryFillRmWithHooks :: Text -> Text
sorryFillRmWithHooks t
  | a <- "If you'd like to fill something with the contents of a vessel on the ground, please pick up the vessel first."
  = T.concat [ "You don't see ", aOrAn t, " here. ", parensQuote a ]

sorryFillSelf :: Text -> Text
sorryFillSelf s = can't "fill the " <> s <> " with itself."

sorryFillSourceCoins :: Text
sorryFillSourceCoins = butCan't "fill a vessel with coins."

sorryFillSourceEq :: Text
sorryFillSourceEq = can't "fill a vessel with an item in your readied equipment."

sorryFillSourceType :: Sing -> Text
sorryFillSourceType = the' . (<> " is not a vessel or a source of liquid.")

sorryFillType :: Sing -> Text
sorryFillType = the' . (<> " is not a vessel that can be filled with liquid.")

sorryFillWaterLiqTypes :: Sing -> Text
sorryFillWaterLiqTypes = the' . (<> " already contains something other than water.")

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
                        \use the \"unready\" command."

sorryGetInInv :: Text
sorryGetInInv = can't "get an item that's already in your inventory. If you're intent on picking it up, try dropping \
                      \it first!"

sorryGetType :: Text -> Text
sorryGetType t = prd $ can't "pick up " <> t

sorryGetWeight :: Sing -> Text
sorryGetWeight = the' . (<> " is too heavy for you to pick up.")

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

sorryInterpNameApostropheCount :: Text
sorryInterpNameApostropheCount = "Your name may contain at most one apostrophe."

sorryInterpNameApostrophePosition :: Text
sorryInterpNameApostrophePosition = "Your name may not begin or end with an apostrophe."

sorryInterpNameDict :: Text
sorryInterpNameDict = "Your name cannot be an English word or proper name. Please choose an original fantasy name."

sorryInterpNameExcessArgs :: Text
sorryInterpNameExcessArgs = "Your name must be a single word."

sorryInterpNameIllegal :: Text
sorryInterpNameIllegal = "Your name cannot include any numbers or symbols (though it may contain an apostrophe)."

sorryInterpNameLen :: Text
sorryInterpNameLen = T.concat [ "Your name must be between ", minNameLenTxt, " and ", maxNameLenTxt, " characters long." ]

sorryInterpNameLenApostrophe :: Text
sorryInterpNameLenApostrophe = "A name containing an apostrophe must be at least " <> minNameLenPlus1Txt <> " characters long."

sorryInterpNameProfanityLogged :: Text
sorryInterpNameProfanityLogged = "Nice try. Your IP address has been logged. Keep this up and you'll get banned."

sorryInterpNameProfanityBoot :: Text
sorryInterpNameProfanityBoot = "Come back when you're ready to act like an adult!"

sorryInterpNamePropName :: Text
sorryInterpNamePropName = "Your name cannot be a real-world proper name. Please choose an original fantasy name."

sorryInterpNameTaken :: Text
sorryInterpNameTaken = but "that name is reserved and may not be used by players."

-----

sorryInterpNewPwDigit :: Text
sorryInterpNewPwDigit = "Passwords must contain at least one digit."

sorryInterpNewPwExcessArgs :: Text
sorryInterpNewPwExcessArgs = "Passwords may not contain whitespace."

sorryInterpNewPwLen :: Text
sorryInterpNewPwLen = T.concat [ "Passwords must be ", showTxt minPwLen, "-", showTxt maxPwLen, " characters in length." ]

sorryInterpNewPwLower :: Text
sorryInterpNewPwLower = "Passwords must contain at least one lowercase character."

sorryInterpNewPwMatch :: Text
sorryInterpNewPwMatch = "Passwords do not match."

sorryInterpNewPwUpper :: Text
sorryInterpNewPwUpper = "Passwords must contain at least one uppercase character."

-----

sorryInterpPager :: Text
sorryInterpPager = "Enter a blank line or \"n\" for the next page, \"b\" for the previous page, or \"q\" to stop reading."

-----

sorryInterpPW :: Text
sorryInterpPW = "Incorrect password."

sorryInterpPwBoot :: Text
sorryInterpPwBoot = "You are being booted due to excessive incorrect passwords."

sorryInterpPwBanned :: Sing -> Text
sorryInterpPwBanned s = colorWith bootMsgColor $ s <> " has been banned from CurryMUD!"

sorryInterpPwDead :: Sing -> Text
sorryInterpPwDead s = but $ s <> " has deceased. Please create a new character."

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

sorryKillAdHoc :: Text -> Text
sorryKillAdHoc t = t <> " is an ad-hoc PC " <> can'tKill

can'tKill :: Text
can'tKill = "and cannot be killed."

sorryKillAdmin :: Text -> Text
sorryKillAdmin t = t <> " is an admin " <> can'tKill

sorryKillAsleep :: Text -> Text
sorryKillAsleep t = t <> " is presently asleep " <> can'tKill

sorryKillDead :: Text -> Text
sorryKillDead t = t <> " is already dead."

sorryKillSpirit :: Text -> Text
sorryKillSpirit t = t <> " has already died (and is presently a spirit)."

sorryKillType :: Id -> Text
sorryKillType i = "ID " <> showTxt i <> " is not a mobile."

-----

sorryLightCoins :: Text
sorryLightCoins = can't "light a coin."

sorryLightExcessFires :: Text
sorryLightExcessFires = butCan't "ignite a light source using more than one fire source."

sorryLightExcessLights :: Text
sorryLightExcessLights = but "you can only ignite a single light source at a time."

sorryLightFireCoins :: Text
sorryLightFireCoins = can't "ignite a light source using a coin."

sorryLightFireInRm :: Text
sorryLightFireInRm = butCan't "ignite a light source using a fire source in your current room. Please pick up the fire \
                              \source first."

sorryLightFireType :: Sing -> Text
sorryLightFireType s = the' s <> " is not a fire source."

sorryLightFireUnlit :: Sing -> Text
sorryLightFireUnlit s = the' s <> " is not lit."

sorryLightInRm :: Text
sorryLightInRm = can't "ignite a light source in your current room. Please pick up the light source first."

sorryLightLampSecs :: Text
sorryLightLampSecs = "The lamp is out of oil."

sorryLightLightType :: Sing -> Text
sorryLightLightType s = prd $ can't "light " <> aOrAn s

sorryLightLit :: Sing -> Text
sorryLightLit s = the' s <> " is already lit."

sorryLightSelf :: Sing -> Text
sorryLightSelf s = the' s <> " can't light itself."

sorryLightTinderbox :: Text
sorryLightTinderbox = "You don't have a tinderbox in your inventory (a fire source is required)."

sorryLightTorchSecs :: Text
sorryLightTorchSecs = "The torch is spent and cannot be lit again."

-----

sorryLinkAlready :: Text -> Sing -> Text
sorryLinkAlready t s = T.concat [ "You've already established a ", parensQuote t, " link with ", s, "." ]

sorryLinkCoin :: Text
sorryLinkCoin = can't "establish a telepathic link with a coin."

sorryLinkInEq :: Text
sorryLinkInEq = can't "establish a telepathic link with an item in your readied equipment."

sorryLinkInInv :: Text
sorryLinkInInv = can't "establish a telepathic link with an item in your inventory."

sorryLinkIntroSelf :: Sing -> Text
sorryLinkIntroSelf s = prd $ "You must first introduce yourself to " <> s

sorryLinkIntroTarget :: Text -> Text
sorryLinkIntroTarget t = "You don't know " <> t <> "'s name."

sorryLinkNoOneHere :: Text
sorryLinkNoOneHere = "You don't see anyone here to link with."

sorryLinkSpirit :: Text
sorryLinkSpirit = "It seems that as a spirit you don't have the capacity to create a new link."

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

sorryMkFoodAmt :: Text -> Text
sorryMkFoodAmt a = dblQuote a <> " is not a valid quantity of food. (You can create between 1 and 100 food objects.)"

sorryMkFoodName :: Text -> Text
sorryMkFoodName a = dblQuote a <> " is not a valid distinct food name."

-----

sorryMkHolyAmt :: Text -> Text
sorryMkHolyAmt a = dblQuote a <> " is not a valid quantity of holy symbols. (You can create between 1 and 100 holy \
                                 \symbols.)"

sorryMkHolyGodName :: Text -> Text
sorryMkHolyGodName a = dblQuote a <> " is not a valid god name."

-----

sorryMsgIncog :: Text
sorryMsgIncog = can't "send a message to a player who is logged in while you are incognito."

-----

sorryNeat :: Text
sorryNeat = "That would be neat, wouldn't it?"

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
sorryNonexistentId i ts = T.concat [ "ID ", showTxt i, " does not exist in ", case ts of
  [t] -> the t <> " table."
  _   -> prd $ "any of the following tables: " <> commas ts ]

-----

sorryNotPossessed :: Sing -> CmdName -> Text
sorryNotPossessed s cn =
    T.concat [ "You must first possess ", theOnLower s, " before you can use the ", dblQuote cn, " command." ]

-----

sorryNoLinks :: Text
sorryNoLinks = "You haven't established a telepathic link with anyone."

sorryNoLinksSpirit :: Text
sorryNoLinksSpirit = "You've lost all your telepathic links."

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
sorryParseInOut value n = T.concat [ dblQuote value, " is not a valid value for the ", dblQuote n
                                   , " setting. Please specify one of the following: \"in\"/\"out\" or \"on\"/\"off\"." ]


sorryParseIndent :: Text -> Text
sorryParseIndent a = dblQuote a <> " is not a valid width amount."

sorryParseLineLen :: Text -> Text
sorryParseLineLen a = dblQuote a <> " is not a valid line length."

sorryParseMouthfuls :: Text -> Text
sorryParseMouthfuls a = dblQuote a <> " is not a valid number of mouthfuls."

sorryParseNum :: Text -> Text -> Text
sorryParseNum numTxt base = T.concat [ dblQuote numTxt, " is not a valid number in base ", base, "." ]

sorryParseOnOff :: Text -> Text -> Text
sorryParseOnOff value n = T.concat [ dblQuote value, " is not a valid value for the ", dblQuote n
                                   , " setting. Please specify \"on\" or \"off\"." ]

sorryParseSeconds :: Text -> Text
sorryParseSeconds a = dblQuote a <> " is not a valid number of seconds."

sorryParseSetting :: Text -> Text -> Text
sorryParseSetting value name = T.concat [ dblQuote value, " is not a valid value for the ", dblQuote name, " setting." ]

sorryParseZoom :: Text -> Text
sorryParseZoom a = dblQuote a <> " is not a valid zoom level."

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

sorryPossessRm :: Text
sorryPossessRm = can't "possess a room."

sorryPossessType :: Sing -> Type -> Text
sorryPossessType s t = T.concat [ theOnLower s, " is a ", pp t, " and cannot be possessed." ]

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

sorryPutLitLight :: Sing -> Sing -> Text
sorryPutLitLight lightSing conSing = T.concat [ "You think twice about putting the lit ", lightSing, " in the ", conSing, "." ]

sorryPutVol :: Sing -> Text
sorryPutVol = the' . (<> " is too full to contain ")

-----

sorryQuitCan'tAbbrev :: Text
sorryQuitCan'tAbbrev = "The \"quit\" command may not be abbreviated. (Type \"quit\" with no arguments to quit CurryMUD.)"

-----

sorryQuoteChars :: Text
sorryQuoteChars = "Unbalanced quote character."

-----

sorryReadCoins :: Text
sorryReadCoins = can't "read a coin."

sorryReadHolySymbol :: Text
sorryReadHolySymbol = "Try though you may, there is nothing to read."

sorryReadInEq :: Text
sorryReadInEq = can't "read an item in your readied equipment."

sorryReadLang :: Sing -> Lang -> Text
sorryReadLang s lang = T.concat [ "Although you recognize that the text on the ", s, " is written in ", pp lang
                                , ", you can't make heads or tails of it." ]

sorryReadNoHooks :: Text
sorryReadNoHooks = "You don't see anything to read here. (If you'd like to read an item on the ground, please pick up \
                   \the item first.)"

sorryReadOrigLang :: Lang -> Text
sorryReadOrigLang lang = "Most unfortunately, as you don't know " <> pp lang <> ", you cannot read the text."

sorryReadType :: Sing -> Text
sorryReadType s = prd $ can't "read the " <> s

sorryReadUnknownLang :: Sing -> Text
sorryReadUnknownLang s = "The text written on the " <> s <> " is in a language you don't recognize."

sorryReadWithHooks :: Text -> Text
sorryReadWithHooks t = dblQuote t <> " does not match the name of a readable fixture of this room. (If you'd like to \
                                     \read an item on the ground, please pick up the item first.)"

-----

sorryReadyAlreadyWearing :: Text -> Text
sorryReadyAlreadyWearing t = prd $ "You're already wearing " <> aOrAn t

sorryReadyAlreadyWearingRing :: Slot -> Sing -> Text
sorryReadyAlreadyWearingRing sl s = T.concat [ "You're already wearing ", aOrAn s, " on your ", pp sl, "." ]

sorryReadyClothFull :: Text -> Text
sorryReadyClothFull t = can't "wear any more " <> t <> "s."

sorryReadyClothFullOneSide :: Cloth -> Slot -> Text
sorryReadyClothFullOneSide (pp -> c) (pp -> s) = can't . T.concat $ [ "wear any more ", c, "s on your ", s, "." ]

sorryReadyCoins :: Text
sorryReadyCoins = can't "ready coins."

sorryReadyHand :: Sing -> Slot -> Text
sorryReadyHand s sl = T.concat [ "You already have ", aOrAn s, " in your ", pp sl, "." ]

sorryReadyHandsFull :: Sing -> Text
sorryReadyHandsFull s = prd $ "You don't have a free hand in which to hold the " <> s

sorryReadyHolySymbolRhayk :: Text
sorryReadyHolySymbolRhayk = "The holy symbol of Rhayk is strictly a ceremonial object; it may not be used as a weapon."

sorryReadyInEq :: Text
sorryReadyInEq = can't "ready an item that's already in your readied equipment."

sorryReadyInRm :: Text
sorryReadyInRm = butCan't "ready an item in your current room. Please pick up the item(s) first."

sorryReadyLight :: Sing -> Text
sorryReadyLight = sorryReadyHandsFull

sorryReadyRol :: Sing -> RightOrLeft -> Text
sorryReadyRol s rol = can't . T.concat $ [ "wear ", aOrAn s, " on your ", pp rol, "." ]

sorryReadyType :: Sing -> Text
sorryReadyType s = prd $ can't "ready " <> aOrAn s

sorryReadyWpnHands :: Sing -> Text
sorryReadyWpnHands s = prd $ "Both hands are required to wield the " <> s

sorryReadyWpnRol :: Sing -> Text
sorryReadyWpnRol s = can't "wield " <> aOrAn s <> " with your finger!"

-----

sorryRefuelAlready :: Sing -> Text
sorryRefuelAlready = the' . (<> " is already full.")

sorryRefuelExcessLamps :: Text
sorryRefuelExcessLamps = but "you can only refuel one lamp at a time."

sorryRefuelExcessVessels :: Text
sorryRefuelExcessVessels = but "you can only specify a single source of oil at a time."

sorryRefuelLampCoins :: Text
sorryRefuelLampCoins = can't "refuel a coin."

sorryRefuelLampInRm :: Text
sorryRefuelLampInRm = can't "refuel a lamp in your current room. Please pick up the lamp first."

sorryRefuelLampType :: Sing -> Text
sorryRefuelLampType s = prd $ can't "refuel " <> aOrAn s

sorryRefuelLiq :: Sing -> Text
sorryRefuelLiq = the' . (<> " doesn't contain oil.")

sorryRefuelVesselCoins :: Text
sorryRefuelVesselCoins = can't "refuel a lamp with coins."

sorryRefuelVesselEmpty :: Sing -> Text
sorryRefuelVesselEmpty = the' . (<> " is empty.")

sorryRefuelVesselInEq :: Text
sorryRefuelVesselInEq = can't "refuel a lamp with an item in your readied equipment."

sorryRefuelVesselInRm :: Text
sorryRefuelVesselInRm = butCan't "refuel a lamp with the contents of a vessel in your current room. Please pick up the \
                                 \vessel first."

-----

sorryRegPlaName :: Text -> Text
sorryRegPlaName n = prd $ "There is no regular player by the name of " <> dblQuote n

-----

sorryRemEmpty :: Sing -> Text
sorryRemEmpty = the' . (<> " is empty.")

sorryRemEnc :: Text
sorryRemEnc = sorryGetEnc

sorryRemCoin :: Text
sorryRemCoin = can't "remove something from a coin."

sorryRemExcessCon :: Text
sorryRemExcessCon = "You can only remove things from one container at a time."

sorryRemIgnore :: Text
sorryRemIgnore = sorryIgnoreLocPrefPlur "The names of the items to be removed from a container"

-----

sorrySacrificeCorpse :: Text
sorrySacrificeCorpse = "There isn't a corpse in your current room for your to sacrifice."

sorrySacrificeCorpseCoins :: Text
sorrySacrificeCorpseCoins = can't "sacrifice a coin."

sorrySacrificeCorpseExcessTargets :: Text
sorrySacrificeCorpseExcessTargets = "You can only sacrifice one corpse at a time."

sorrySacrificeCorpseInEq :: Text
sorrySacrificeCorpseInEq = sorrySacrificeCorpseHelper "readied equipment"

sorrySacrificeCorpseHelper :: Text -> Text
sorrySacrificeCorpseHelper t = can't $ "sacrifice a corpse in your " <> t <> ". The corpse must be on the ground in \
                                       \your current room."

sorrySacrificeCorpseInInv :: Text
sorrySacrificeCorpseInInv = sorrySacrificeCorpseHelper "inventory"

sorrySacrificeCorpseType :: Sing -> Text
sorrySacrificeCorpseType s = prd $ can't "sacrifice " <> aOrAnOnLower s

sorrySacrificeHolySymbol :: Text
sorrySacrificeHolySymbol = "You must have a holy symbol in your inventory to sacrifice a corpse."

sorrySacrificeHolySymbolCoins :: Text
sorrySacrificeHolySymbolCoins = can't "sacrifice a corpse using a coin."

sorrySacrificeHolySymbolCorpse :: Text
sorrySacrificeHolySymbolCorpse = "To sacrifice a corpse, you must have a holy symbol in your inventory and there must \
                                 \be a corpse in your current room."

sorrySacrificeHolySymbolExcessTargets :: Text
sorrySacrificeHolySymbolExcessTargets = "Please specify a single holy symbol with which to sacrifice the corpse."

sorrySacrificeHolySymbolInEq :: Text
sorrySacrificeHolySymbolInEq = sorrySacrificeHolySymbolHelper "readied equipment"

sorrySacrificeHolySymbolHelper :: Text -> Text
sorrySacrificeHolySymbolHelper t = can't $ "sacrifice a corpse with a holy symbol in your " <> t <> ". The holy symbol \
                                           \must be in your inventory."

sorrySacrificeHolySymbolInRm :: Text
sorrySacrificeHolySymbolInRm = sorrySacrificeHolySymbolHelper "current room"

sorrySacrificeHolySymbolType :: Sing -> Text
sorrySacrificeHolySymbolType s = prd $ can't "sacrifice a corpse using " <> aOrAn s

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
sorrySetRange settingName minVal maxVal =
    T.concat [ capitalize settingName, " must be between ", showTxt minVal, " and ", showTxt maxVal, "." ]

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

sorrySmellLitLight :: Sing -> Text
sorrySmellLitLight s = prd $ "You think twice about putting your nose near the lit " <> s

sorrySmellNothingToSmell :: Text
sorrySmellNothingToSmell = "There isn't anything to smell."

sorrySmellRmCoins :: (Text, Bool) -> Text
sorrySmellRmCoins (coinTxt, isPlur) =
    T.concat [ "You must pick up the ", coinTxt, " before you can smell ", isPlur ? "them" :? "it", "." ]

sorrySmellRmNoHooks :: Sing -> Text
sorrySmellRmNoHooks s = "You must pick up the " <> s <> " before you can smell it."

sorrySmellTasteSlot :: Sing -> Text
sorrySmellTasteSlot s = "You'll have to unready the " <> s <> " first."

-----

sorryStanceAlready :: Text -> Text
sorryStanceAlready t = "You're already taking " <> aOrAn t <> " combat stance."

sorryStanceName :: Text -> Text
sorryStanceName t = T.concat [ dblQuote t
                             , " is not a valid combat stance. Please type "
                             , colorWith quoteColor "stance"
                             , " followed by one of the following: "
                             , colorWith quoteColor "offensive"
                             , ", "
                             , colorWith quoteColor "neutral"
                             , ", or "
                             , colorWith quoteColor "defensive"
                             , "." ]

-----

sorryStopActName :: Text -> Text
sorryStopActName t = T.concat [ dblQuote t
                              , " is not the name of an activity that can be stopped. Please type "
                              , colorWith quoteColor "stop"
                              , " followed by one of the following: "
                              , colorWith quoteColor "sacrificing"
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
sorryStopNotDoing actType = "You're not " <> pp actType <> " anything at the moment."

sorryStopNotDoingAnything :: Text
sorryStopNotDoingAnything = "You're not doing anything that can be stopped (sacrificing, eating, drinking, or attacking)."

-----

sorrySudoerAdHoc :: Text
sorrySudoerAdHoc = can't "promote to admin an ad-hoc PC."

sorrySudoerDemoteRoot :: Text
sorrySudoerDemoteRoot = can't "demote Root."

sorrySudoerDemoteSelf :: Text
sorrySudoerDemoteSelf = can't "demote yourself."

sorrySudoerSpirit :: Text -> Text
sorrySudoerSpirit t = t <> " is a spirit and cannot be promoted."

-----

sorrySuicide :: Text
sorrySuicide = "Feeling suicidal?"

-----

sorrySummonAdHoc :: Text
sorrySummonAdHoc = can't "summon an ad-hoc PC."

sorrySummonAdmin :: Text
sorrySummonAdmin = can't "summon an admin."

sorrySummonAlready :: Sing -> Text
sorrySummonAlready s = s <> " is already here!"

sorrySummonSelf :: Text
sorrySummonSelf = can't "summon yourself."

-----

sorryTasteExcessTargets :: Text
sorryTasteExcessTargets = but "you can only taste one thing at a time."

sorryTasteInRm :: Text
sorryTasteInRm = butCan't "taste an item in your current room. Please pick up the item first."

sorryTasteLitLight :: Sing -> Text
sorryTasteLitLight s = prd $ "You think twice about putting your mouth near the lit " <> s

sorryTasteNothingToTaste :: Text
sorryTasteNothingToTaste = "There isn't anything to taste."

sorryTasteType :: Sing -> Text
sorryTasteType s = prd $ can't "taste " <> aOrAn s

-----

sorryTeleAlready :: Text
sorryTeleAlready = "You're already there!"

sorryTeleLoggedOutRm :: Text
sorryTeleLoggedOutRm = can't "teleport to the logged out room."

sorryTeleNecropolis :: Text
sorryTeleNecropolis = can't "teleport to the necropolis."

sorryTeleRmName :: Text -> Text
sorryTeleRmName n =
    T.concat [ dblQuote n, " is not a valid room name. Type ", colorWith quoteColor . prefixAdminCmd $ "telerm"
             , " with no arguments to get a list of valid room names." ]

sorryTeleSelf :: Text
sorryTeleSelf = can't "teleport to yourself."

sorryTeleWelcomeRm :: Text
sorryTeleWelcomeRm = can't "teleport to the welcome room."

-----

sorryTimeNotOutside :: Text
sorryTimeNotOutside = "You must be outside, where you can see the sky."

-----

sorryTimeUnknown :: Text
sorryTimeUnknown = can't "tell what time it is."

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

sorryTunedOutChan :: CmdName -> DoOrDon'tQuote -> Text -> Text
sorryTunedOutChan x quote y =
    T.concat [ "You have tuned out the ", onTrue (quote == DoQuote) dblQuote y, " channel. Type "
             , colorWith quoteColor . T.concat $ [ x, " ", y, "=in" ], " to tune it back in." ]

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
                                      , let ts = [ T.singleton expCmdChar, cn, " ", T.singleton . toLower . T.head $ s ]
                                        in colorWith quoteColor . T.concat $ ts
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
sorryWrapLineLen = T.concat [ "The line length must be between ", showTxt minCols, " and ", showTxt maxCols, " characters." ]

-----

sorryWtf :: Text
sorryWtf = colorWith wtfColor "He don't."

-----

sorryWut :: Text -> Text
sorryWut t = prd $ "I don't understand " <> dblQuote t
