{-# LANGUAGE OverloadedStrings #-}

module Mud.Cmds.Msgs.Misc where

import Mud.Util.Quoting

import Data.Monoid ((<>))
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


bannedMsg :: T.Text
bannedMsg = "You have been banned from CurryMUD!"


dudeYou'reNaked :: T.Text
dudeYou'reNaked = "You don't have anything readied. You're naked!"


dudeYou'reScrewed :: T.Text
dudeYou'reScrewed = "You aren't carrying anything, and you don't have anything readied. You're naked!"


dudeYourHandsAreEmpty :: T.Text
dudeYourHandsAreEmpty = "You aren't carrying anything."


dbErrorMsg :: T.Text
dbErrorMsg = "There was an error when reading the database."


dfltBootMsg :: T.Text
dfltBootMsg = "You have been booted from CurryMUD. Goodbye!"


dfltShutdownMsg :: T.Text
dfltShutdownMsg = "CurryMUD is shutting down. We apologize for the inconvenience. See you soon!"


effortsBlockedMsg :: T.Text
effortsBlockedMsg = "Your efforts are blocked; "


focusingInnateMsg :: T.Text
focusingInnateMsg = "Focusing your innate psionic energy for a brief moment, "


genericErrorMsg :: T.Text
genericErrorMsg = "Unfortunately, an error occured while executing your command."


inacBootMsg :: T.Text
inacBootMsg = "You are being disconnected from CurryMUD due to inactivity."


loadWorldErrorMsg :: T.Text
loadWorldErrorMsg = "Oops! There was an error loading the world. Check the error log for details."


motdErrorMsg :: T.Text
motdErrorMsg = "Unfortunately, the message of the day could not be retrieved."


notifyArrivalMsg :: T.Text -> T.Text
notifyArrivalMsg n = n <> " slowly materializes out of thin air."


plusRelatedMsg :: T.Text
plusRelatedMsg = parensQuote "plus related functionality" <> "."


rethrowExMsg :: T.Text -> T.Text
rethrowExMsg t = "exception caught " <> t <> "; rethrowing to listen thread"


sudoMsg :: T.Text
sudoMsg = "HELLO, ROOT! We trust you have received the usual lecture from the local System Administrator..."
