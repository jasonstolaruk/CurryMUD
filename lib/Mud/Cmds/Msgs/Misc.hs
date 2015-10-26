{-# LANGUAGE OverloadedStrings #-}

module Mud.Cmds.Msgs.Misc where

import Mud.Util.Quoting

import Data.Monoid ((<>))
import qualified Data.Text as T


bannedMsg :: T.Text
bannedMsg = "You have been banned from CurryMUD!"


dbErrorMsg :: T.Text
dbErrorMsg = "There was an error when reading the database."


dfltBootMsg :: T.Text
dfltBootMsg = "You have been booted from CurryMUD. Goodbye!"


dfltShutdownMsg :: T.Text
dfltShutdownMsg = "CurryMUD is shutting down. We apologize for the inconvenience. See you soon!"


effortsBlockedMsg :: T.Text
effortsBlockedMsg = "Your efforts are blocked; "


egressMsg :: T.Text -> T.Text
egressMsg n = n <> " slowly dissolves into nothingness."


focusingInnateMsg :: T.Text
focusingInnateMsg = "Focusing your innate psionic energy for a brief moment, "


genericErrorMsg :: T.Text
genericErrorMsg = "Unfortunately, an error occured while executing your command."


helpRootErrorMsg :: T.Text
helpRootErrorMsg = helpFileErrorMsg "root"


helpFileErrorMsg :: T.Text -> T.Text
helpFileErrorMsg n = "Unfortunately, the " <> n <> " help file could not be read."


inacBootMsg :: T.Text
inacBootMsg = "You are being disconnected from CurryMUD due to inactivity."


loadWorldErrorMsg :: T.Text
loadWorldErrorMsg = "There was an error loading the world. Check the error log for details."


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
