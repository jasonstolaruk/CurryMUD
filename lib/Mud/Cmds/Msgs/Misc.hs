{-# LANGUAGE OverloadedStrings #-}

module Mud.Cmds.Msgs.Misc where

import Mud.Data.State.MudData
import Mud.TopLvlDefs.Misc
import Mud.Util.Quoting
import Mud.Util.Text

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


asMsg :: Text
asMsg = "You suddenly feel as though someone else is in control..."


bannedMsg :: Text
bannedMsg = "You have been banned from CurryMUD!"


blankWritableMsg :: Sing -> Text
blankWritableMsg s = prd $ "There isn't anything written on the " <> s


dbEmptyMsg :: Text
dbEmptyMsg = "The database is empty."


dbErrorMsg :: Text
dbErrorMsg = "There was an error while reading the database."


descRulesMsg :: Text
descRulesMsg =
    "In order to preserve the integrity of the virtual world along with the enjoyment of all, the following rules must \
    \be observed. Violation of these rules is grounds for discipline including banishment from CurryMUD.\n\
    \1) Descriptions must be realistic and reasonable. A felinoid with an unusual fur color is acceptable, while a \
    \six-foot dwarf is not.3`\n\
    \2) Descriptions must be passive and written from an objective viewpoint. \"He is exceptionally thin\" is \
    \acceptable, while \"You can't believe how thin he is\" is not.3`\n\
    \3) Descriptions may only contain observable information. \"People tend to ask her about her adventures\" and \
    \\"He is a true visionary among elves\" are both illegal. Likewise, you may not include your character's name in \
    \your description.3`\n\
    \4) Keep your description short. The longer your description, the less likely people are to actually read it!3`\n\
    \5) You may not make radical changes to your description without a plausible in-game explanation. This means that \
    \it is normally illegal to make sudden, striking changes to enduring physical characteristics (height, eye color, \
    \etc.). If you would like to make such a change and feel there could be a plausible in-game explanation, get \
    \permission from an administrator first.3`"


dfltBootMsg :: Text
dfltBootMsg = "You have been booted from CurryMUD. Goodbye!"


dfltShutdownMsg :: Text
dfltShutdownMsg = "CurryMUD is shutting down. We apologize for the inconvenience. See you soon!"


effortsBlockedMsg :: Text
effortsBlockedMsg = "Your efforts are blocked; "


egressMsg :: Text -> Text
egressMsg n = n <> " slowly dissolves into nothingness."


focusingInnateMsg :: Text
focusingInnateMsg = "Focusing your innate psionic energy for a brief moment, "


genericErrorMsg :: Text
genericErrorMsg = "Unfortunately, an error occured while executing your command."


helpRootErrorMsg :: Text
helpRootErrorMsg = helpFileErrorMsg "root"


helpFileErrorMsg :: Text -> Text
helpFileErrorMsg n = "Unfortunately, the " <> n <> " help file could not be read."


inacBootMsg :: Text
inacBootMsg = "You are being disconnected from CurryMUD due to inactivity."


loadTblErrorMsg :: FilePath -> Text -> Text
loadTblErrorMsg fp msg = T.concat [ "error parsing ", dblQuote . T.pack $ fp, ": ", msg, "." ]


loadWorldErrorMsg :: Text
loadWorldErrorMsg = "There was an error loading the world. Check the error log for details."


lvlUpMsg :: Text
lvlUpMsg = "Congratulations! You gained a level."


motdErrorMsg :: Text
motdErrorMsg = "Unfortunately, the message of the day could not be retrieved."


noSmellMsg :: Text
noSmellMsg = "You don't smell anything in particular."


noSoundMsg :: Text
noSoundMsg = "You don't hear anything in particular."


noTasteMsg :: Text
noTasteMsg = "You don't taste anything in particular."


notifyArrivalMsg :: Text -> Text
notifyArrivalMsg n = n <> " slowly materializes out of thin air."


plusRelatedMsg :: Text
plusRelatedMsg = prd . parensQuote $ "plus related functionality"


pwMsg :: Text -> [Text]
pwMsg t = [ T.concat [ t
                     , " Passwords must be "
                     , showText minPwLen
                     , "-"
                     , showText maxPwLen
                     , " characters in length and contain:" ]
          , "* 1 or more lowercase characters"
          , "* 1 or more uppercase characters"
          , "* 1 or more digits"
          , "* 0 whitespace characters" ]


pwWarningMsg :: Text
pwWarningMsg = pwWarningTxt <> ".)"


pwWarningLoginMsg :: Text
pwWarningLoginMsg = pwWarningTxt <> " once inside the game.)"


pwWarningTxt :: Text
pwWarningTxt = "Please make a note of your new password. If you lose your password, you may lose your character! \
               \(To safeguard against this unfortunate situation, use the " <> dblQuote "security" <> " command to \
               \provide a security Q&A"


rethrowExMsg :: Text -> Text
rethrowExMsg t = "exception caught " <> t <> "; rethrowing to listen thread"


sudoMsg :: Text
sudoMsg = "HELLO, ROOT! We trust you have received the usual lecture from the local System Administrator..."


teleDescMsg :: Text
teleDescMsg = "You are instantly transported in a blinding flash of white light. For a brief moment you are \
              \overwhelmed with vertigo accompanied by a confusing sensation of nostalgia."


teleDestMsg :: Text -> Text
teleDestMsg t = "There is a soft audible pop as " <> t <> " appears in a jarring flash of white light."


teleOriginMsg :: Text -> Text
teleOriginMsg t = "There is a soft audible pop as " <> t <> " vanishes in a jarring flash of white light."


unlinkMsg :: Text -> Sing -> Text
unlinkMsg t s = T.concat [ "You suddenly feel a slight tingle "
                         , t
                         , "; you sense that your telepathic link with "
                         , s
                         , " has been severed." ]
