{-# LANGUAGE OverloadedStrings #-}

module Mud.Cmds.Msgs.Misc where

import Mud.Data.State.MudData
import Mud.Util.Quoting

import Data.Monoid ((<>))
import qualified Data.Text as T


asMsg :: T.Text
asMsg = "You suddenly feel as though someone else is in control..."


bannedMsg :: T.Text
bannedMsg = "You have been banned from CurryMUD!"


dbEmptyMsg :: T.Text
dbEmptyMsg = "The database is empty."


dbErrorMsg :: T.Text
dbErrorMsg = "There was an error while reading the database."


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


loadTblErrorMsg :: FilePath -> T.Text -> T.Text
loadTblErrorMsg fp msg = T.concat [ "error parsing ", dblQuote . T.pack $ fp, ": ", msg, "." ]


loadWorldErrorMsg :: T.Text
loadWorldErrorMsg = "There was an error loading the world. Check the error log for details."


lvlMsg :: T.Text
lvlMsg = "Congratulations! You gained a level."


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


teleDescMsg :: T.Text
teleDescMsg = "You are instantly transported in a blinding flash of white light. For a brief moment you are \
              \overwhelmed with vertigo accompanied by a confusing sensation of nostalgia."


teleDestMsg :: T.Text -> T.Text
teleDestMsg t = "There is a soft audible pop as " <> t <> " appears in a jarring flash of white light."


teleOriginMsg :: T.Text -> T.Text
teleOriginMsg t = "There is a soft audible pop as " <> t <> " vanishes in a jarring flash of white light."


unlinkMsg :: T.Text -> Sing -> T.Text
unlinkMsg t s = T.concat [ "You suddenly feel a slight tingle "
                         , t
                         , "; you sense that your telepathic link with "
                         , s
                         , " has been severed." ]
