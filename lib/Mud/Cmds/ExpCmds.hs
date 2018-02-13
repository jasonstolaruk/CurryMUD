{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.Cmds.ExpCmds ( expCmdNames
                        , expCmdSet
                        , expCmds
                        , getExpCmdByName
                        , mkExpAction ) where

import           Mud.Cmds.Msgs.Advice
import           Mud.Cmds.Msgs.Sorry
import           Mud.Cmds.Util.Misc
import           Mud.Cmds.Util.Pla
import           Mud.Data.Misc
import           Mud.Data.State.ActionParams.ActionParams
import           Mud.Data.State.MsgQueue
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import           Mud.Misc.LocPref
import qualified Mud.Misc.Logging as L (logPlaOut)
import           Mud.TopLvlDefs.Chars
import           Mud.Util.List
import qualified Mud.Util.Misc as U (pmf)
import           Mud.Util.Misc hiding (pmf)
import           Mud.Util.Operators
import           Mud.Util.Text

import           Control.Arrow (first)
import           Control.Lens (both)
import           Control.Lens.Operators ((?~), (.~), (&), (%~))
import           Control.Monad (when)
import           Data.Bool
import           Data.List ((\\), delete, intersect)
import qualified Data.Set as S (Set, filter, foldr, fromList, map, toList)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Stack (HasCallStack)

pmf :: PatternMatchFail
pmf = U.pmf "Mud.Cmds.ExpCmds"

-----

logPlaOut :: Text -> Id -> [Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Cmds.ExpCmds"

-- ==================================================

expCmdSet :: HasCallStack => S.Set ExpCmd
expCmdSet = S.fromList
    [ ExpCmd "admire"
             (HasTarget "You admire @."
                        "% admires you."
                        "% admires @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (dup False)
             False
             Nothing
    , ExpCmd "anger"
             (Versatile "You cry out in anger."
                        "% cries out in anger."
                        "You cry out in anger at @."
                        "% cries out in anger at you."
                        "% cries out in anger at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (True, False)
             True
             Nothing
    , ExpCmd "applaud"
             (Versatile "You applaud enthusiastically."
                        "% applauds enthusiastically."
                        "You applaud enthusiastically for @."
                        "% applauds enthusiastically for you."
                        "% applauds enthusiastically for @.")
             ([ Drinking, Eating ], allValues)
             (True, False)
             True
             Nothing
    , ExpCmd "astonished"
             (Versatile "You are absolutely astonished."
                        "% is absolutely astonished."
                        "You stare at @ with an astonished expression on your face."
                        "% stares at you with an astonished expression on & face."
                        "% stares at @ with an astonished expression on & face.")
             ([ Attacking, Drinking, Eating ], allValues)
             (dup False)
             False
             Nothing
    , ExpCmd "astounded"
             (Versatile "You are altogether astounded."
                        "% is altogether astounded."
                        "You are altogether astounded at @."
                        "% is altogether astounded at you."
                        "% is altogether astounded at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (dup False)
             False
             Nothing
    , ExpCmd "avert"
             (Versatile "You avert your eyes."
                        "% averts & eyes."
                        "You avert your eyes from @."
                        "% averts & eyes from you."
                        "% averts & eyes from @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (dup False)
             False
             Nothing
    , ExpCmd "bawl"
             (NoTarget  "You bawl like a baby."
                        "% bawls like a baby.")
             ([ Attacking, Drinking, Eating ], [])
             (dup True)
             True
             Nothing
    , ExpCmd "beam"
             (Versatile "You beam cheerfully."
                        "% beams cheerfully."
                        "You beam cheerfully at @."
                        "% beams cheerfully at you."
                        "% beams cheerfully at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (True, False)
             False
             Nothing
    , ExpCmd "belch"
             (Versatile "You let out a deep belch."
                        "% lets out a deep belch."
                        "You belch purposefully at @."
                        "% belches purposefully at you."
                        "% belches purposefully at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (True, False)
             True
             Nothing
    , ExpCmd "bewildered"
             (Versatile "You are hopelessly bewildered."
                        "% is hopelessly bewildered."
                        "You are hopelessly bewildered by @'s behavior."
                        "% is hopelessly bewildered by your behavior."
                        "% is hopelessly bewildered by @'s behavior.")
             ([ Attacking, Drinking, Eating ], allValues)
             (dup False)
             False
             Nothing
    , ExpCmd "blank"
             (Versatile "You have a blank expression on your face."
                        "% has a blank expression on & face."
                        "You look blankly at @."
                        "% looks blankly at you."
                        "% looks blankly at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (dup False)
             False
             Nothing
    , ExpCmd "blink"
             (Versatile "You blink."
                        "% blinks."
                        "You blink at @."
                        "% blinks at you."
                        "% blinks at @.")
             (dup allValues) -- TODO: Can sacrifice.
             (dup False)
             False
             Nothing
    , ExpCmd "blush"
             (NoTarget  "You blush."
                        "% blushes.")
             ([ Attacking, Drinking, Eating ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "boggle"
             (NoTarget  "You boggle at the concept."
                        "% boggles at the concept.")
             ([ Attacking, Drinking, Eating ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "bow"
             (Versatile "You bow."
                        "% bows."
                        "You bow before @."
                        "% bows before you."
                        "% bows before @.")
             ([ Drinking, Eating ], allValues)
             (True, False)
             True
             Nothing
    , ExpCmd "burp"
             (Versatile "You burp."
                        "% burps."
                        "You burp rudely at @."
                        "% burps rudely at you."
                        "% burps rudely at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (True, False)
             True
             Nothing
    , ExpCmd "burstlaugh"
             (Versatile "You abruptly burst into laughter."
                        "% abruptly bursts into laughter."
                        "You abruptly burst into laughter in reaction to @."
                        "% abruptly bursts into laughter in reaction to you."
                        "% abruptly bursts into laughter in reaction to @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (dup True)
             True
             Nothing
    , ExpCmd "calm"
             (NoTarget  "You appear calm and collected."
                        "% appears calm and collected.")
             (allValues, []) -- TODO: Can sacrifice.
             (dup False)
             False
             Nothing
    , ExpCmd "caresscheek"
             (HasTarget "You caress @'s cheek."
                        "% caresses your cheek."
                        "% caresses @'s cheek.")
             ([ Drinking, Eating ], [ Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "cheer"
             (Versatile "You cheer eagerly."
                        "% cheers eagerly."
                        "You cheer eagerly for @."
                        "% cheers eagerly for you."
                        "% cheers eagerly for @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (True, False)
             True
             Nothing
    , ExpCmd "chortle"
             (Versatile "You chortle gleefully."
                        "% chortles gleefully."
                        "You chortle gleefully at @."
                        "% chortles gleefully at you."
                        "% chortles gleefully at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (True, False)
             True
             Nothing
    , ExpCmd "chuckle"
             (Versatile "You chuckle."
                        "% chuckles."
                        "You chuckle at @."
                        "% chuckles at you."
                        "% chuckles at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (True, False)
             True
             Nothing
    , ExpCmd "clap"
             (Versatile "You clap."
                        "% claps."
                        "You clap for @."
                        "% claps for you."
                        "% claps for @.")
             ([ Drinking, Eating ], allValues)
             (True, False)
             True
             Nothing
    , ExpCmd "closeeyes"
             (NoTarget  "You close your eyes."
                        "% closes & eyes.")
             ([ Drinking, Eating, Sacrificing ], []) -- TODO: Can sacrifice.
             (dup False)
             False
             Nothing
    , ExpCmd "coldsweat"
             (NoTarget  "You break out in a cold sweat."
                        "% breaks out in a cold sweat.")
             (allValues, []) -- TODO: Can sacrifice.
             (dup False)
             False
             Nothing
    , ExpCmd "comfort"
             (HasTarget "You comfort @."
                        "% comforts you."
                        "% comforts @.")
             ([ Drinking, Eating ], [ Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "confused"
             (Versatile "You look utterly confused."
                        "% looks utterly confused."
                        "Utterly confused, you look fixedly at @."
                        "Utterly confused, % looks fixedly at you."
                        "Utterly confused, % looks fixedly at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (dup False)
             False
             Nothing
    , ExpCmd "cough"
             (Versatile "You cough."
                        "% coughs."
                        "You cough loudly at @."
                        "% coughs loudly at you."
                        "% coughs loudly at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (True, False)
             True
             Nothing
    , ExpCmd "coverears"
             (NoTarget  "You cover your ears."
                        "% covers & ears.")
             ([ Drinking, Eating ], [])
             (True, False)
             False
             Nothing
    , ExpCmd "covereyes"
             (NoTarget  "You cover your eyes."
                        "% covers & eyes.")
             ([ Drinking, Eating ], [])
             (True, False)
             False
             Nothing
    , ExpCmd "covermouth"
             (NoTarget  "You cover your mouth."
                        "% covers & mouth.")
             ([ Drinking, Eating ], [])
             (True, False)
             False
             Nothing
    , ExpCmd "cower"
             (Versatile "You cower in fear."
                        "% cowers in fear."
                        "You cower in fear before @."
                        "% cowers in fear before you."
                        "% cowers in fear before @.")
             ([ Drinking, Eating ], allValues)
             (dup True)
             False
             Nothing
    , ExpCmd "cringe"
             (Versatile "You cringe."
                        "% cringes."
                        "You cringe at @."
                        "% cringes at you."
                        "% cringes at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (dup False)
             False
             Nothing
    , ExpCmd "crossarms"
             (NoTarget  "You cross your arms."
                        "% crosses & arms.")
             ([ Drinking, Eating ], [])
             (True, False)
             False
             Nothing
    , ExpCmd "crossfingers"
             (NoTarget  "You cross your fingers."
                        "% crosses & fingers.")
             ([ Attacking, Drinking, Eating ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "cry"
             (NoTarget  "You cry."
                        "% cries.")
             ([ Attacking, Drinking, Eating, Sacrificing ], []) -- TODO: Can sacrifice.
             (dup False)
             True
             Nothing
    , ExpCmd "cryanger"
             (Versatile "You cry out in anger."
                        "% cries out in anger."
                        "You cry out in anger at @."
                        "% cries out in anger at you."
                        "% cries out in anger at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (True, False)
             True
             Nothing
    , ExpCmd "cuddle"
             (HasTarget "You cuddle with @."
                        "% cuddles with you."
                        "% cuddles with @.")
             ([ Drinking, Eating ], [ Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "curious"
             (Versatile "You have a curious expression on your face."
                        "% has a curious expression on & face."
                        "You flash a curious expression at @."
                        "% flashes a curious expression at you."
                        "% flashes a curious expression at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (dup False)
             False
             Nothing
    , ExpCmd "curse"
             (Versatile "You curse."
                        "% curses."
                        "You curse at @."
                        "% curses at you."
                        "% curses at @.")
             ([ Attacking, Drinking, Eating ], allValues)
             (True, False)
             True
             Nothing
    , ExpCmd "curtsey"
             (Versatile "You curtsey."
                        "% curtseys."
                        "You curtsey to @."
                        "% curtseys to you."
                        "% curtseys to @.")
             ([ Drinking, Eating ], allValues)
             (True, False)
             False
             Nothing
    , ExpCmd "curtsy"
             (Versatile "You curtsy."
                        "% curtsies."
                        "You curtsy to @."
                        "% curtsies to you."
                        "% curtsies to @.")
             ([ Drinking, Eating ], allValues)
             (True, False)
             False
             Nothing
    , ExpCmd "dance"
             (Versatile "You dance around."
                        "% dances around."
                        "You dance with @."
                        "% dances with you."
                        "% dances with @.")
             ([ Drinking, Eating ], [ Drinking, Eating ])
             (True, False)
             True
             (Just "")
    , ExpCmd "daydream"
             (NoTarget  "Staring off into the distance, you indulge in a daydream."
                        "Staring off into the distance, % indulges in a daydream.")
             ([ Drinking, Eating ], [])
             (True, False)
             False
             Nothing
    , ExpCmd "deepbreath"
             (NoTarget  "You take a deep breath."
                        "% takes a deep breath.")
             ([ Attacking, Drinking, Eating ], [])
             (True, False)
             True
             Nothing
    , ExpCmd "disappoint" -- TODO: Here.
             (Versatile "You are clearly disappointed."
                        "% is clearly disappointed."
                        "You are clearly disappointed in @."
                        "% is clearly disappointed in you."
                        "% is clearly disappointed in @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "dizzy"
             (NoTarget  "Dizzy and reeling, you look as though you might pass out."
                        "Dizzy and reeling, % looks as though ^ might pass out.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "doublelaugh"
             (Versatile "You double over with laughter."
                        "% doubles over with laughter."
                        "You double over with laughter in reaction to @."
                        "% doubles over with laughter in reaction to you."
                        "% doubles over with laughter in reaction to @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "drool"
             (NoTarget  "You drool."
                        "% drools.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "droop"
             (NoTarget  "Your eyes droop."
                        "%'s eyes droop.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "duck"
             (Versatile "You duck."
                        "% ducks."
                        "You duck away from @."
                        "% ducks away from you."
                        "% ducks away from @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "embrace"
             (HasTarget "You warmly embrace @."
                        "% embraces you warmly."
                        "% embraces @ warmly.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "exhausted"
             (NoTarget  "Exhausted, your face displays a weary expression."
                        "Exhausted, %'s face displays a weary expression.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "facepalm"
             (NoTarget  "You facepalm."
                        "% facepalms.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "faint"
             (NoTarget  "You faint."
                        "% faints.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             (Just "fainted")
    , ExpCmd "fistpump"
             (NoTarget  "You pump your fist in the air triumphantly."
                        "% pumps & fist in the air triumphantly.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "flex"
             (Versatile "You flex your muscles."
                        "%'s flexes & muscles."
                        "You flex your muscles at @."
                        "% flexes & muscles at you."
                        "% flexes & muscles at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "flinch"
             (NoTarget  "You flinch."
                        "% flinches.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "flop"
             (NoTarget  "You flop down on the ground."
                        "% flops down on the ground.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             (Just "on the ground")
    , ExpCmd "flustered"
             (NoTarget  "You look entirely flustered."
                        "% looks entirely flustered.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "frown"
             (Versatile "You frown."
                        "% frowns."
                        "You frown at @."
                        "% frowns at you."
                        "% frowns at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "funnyface"
             (Versatile "You make a funny face."
                        "% makes a funny face."
                        "You make a funny face at @."
                        "% makes a funny face at you."
                        "% makes a funny face at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "gape"
             (Versatile "You gape."
                        "% gapes."
                        "You gape at @."
                        "% gapes at you."
                        "% gapes at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "gasp"
             (Versatile "You gasp."
                        "% gasps."
                        "You gasp at @."
                        "% gasps at you."
                        "% gasps at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "gawk"
             (HasTarget "You gawk at @."
                        "% gawks at you."
                        "% gawks at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "gaze"
             (HasTarget "You gaze longingly at @."
                        "% gazes longingly at you."
                        "% gazes longingly at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "giggle"
             (Versatile "You giggle."
                        "% giggles."
                        "You giggle at @."
                        "% giggles at you."
                        "% giggles at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "glance"
             (Versatile "You glance around."
                        "% glances around."
                        "You glance at @."
                        "% glances at you."
                        "% glances at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "glare"
             (HasTarget "You glare at @."
                        "% glares at you."
                        "% glares at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "greet"
             (HasTarget "You greet @."
                        "% greets you."
                        "% greets @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "grimace"
             (Versatile "You grimace disapprovingly."
                        "% grimaces disapprovingly."
                        "You grimace disapprovingly at @."
                        "% grimaces disapprovingly at you."
                        "% grimaces disapprovingly at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "grin"
             (Versatile "You grin."
                        "% grins."
                        "You grin at @."
                        "% grins at you."
                        "% grins at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "groan"
             (Versatile "You groan."
                        "% groans."
                        "You groan at @."
                        "% groans at you."
                        "% groans at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "grovel"
             (HasTarget "You grovel before @."
                        "% grovels before you."
                        "% grovels before @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "growl"
             (Versatile "You growl menacingly."
                        "% growls menacingly."
                        "You growl menacingly at @."
                        "% growls menacingly at you."
                        "% growls menacingly at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "grumble"
             (NoTarget  "You grumble to yourself."
                        "% grumbles to *.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "guffaw"
             (Versatile "You guffaw boisterously."
                        "% guffaws boisterously."
                        "You guffaw boisterously at @."
                        "% guffaws boisterously at you."
                        "% guffaws boisterously at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "gulp"
             (NoTarget  "You gulp."
                        "% gulps.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "handhips"
             (NoTarget  "You put your hands on your hips."
                        "% puts & hands on & hips.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "hesitate"
             (NoTarget  "You hesitate."
                        "% hesitates.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "hiccup"
             (NoTarget  "You hiccup involuntarily."
                        "% hiccups involuntarily.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "highfive"
             (HasTarget "You give @ a high five."
                        "% gives you a high five."
                        "% gives @ a high five.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "hmm"
             (NoTarget  "You say, \"Hmm...\" and think about it."
                        "% says, \"Hmm...\" and thinks about it.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "holdhand"
             (HasTarget "You hold @'s hand."
                        "% holds your hand."
                        "% holds @'s hand.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "hop"
             (NoTarget  "You hop up and down excitedly."
                        "% hops up and down excitedly.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "howllaugh"
             (Versatile "You howl with laughter."
                        "% howls with laughter."
                        "You howl with laughter at @."
                        "% howls with laughter at you."
                        "% howls with laughter at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "hug"
             (HasTarget "You hug @."
                        "% hugs you."
                        "% hugs @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "hum"
             (NoTarget  "You hum a merry tune."
                        "% hums a merry tune.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "innocent"
             (NoTarget  "You try to look innocent."
                        "% tries to look innocent.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "inquisitive"
             (Versatile "You have an inquisitive expression on your face."
                        "% has an inquisitive expression on & face."
                        "You flash an inquisitive expression at @."
                        "% flashes an inquisitive expression at you."
                        "% flashes an inquisitive expression at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "jig"
             (Versatile "You dance a lively jig."
                        "% dances a lively jig."
                        "You dance a lively jig with @."
                        "% dances a lively jig with you."
                        "% dances a lively jig with @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             (Just "")
    , ExpCmd "joytears"
             (NoTarget  "You are overcome with tears of joy."
                        "% is overcome with tears of joy.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "jump"
             (NoTarget  "You jump up and down excitedly."
                        "% jumps up and down excitedly.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "kiss"
             (HasTarget "You kiss @."
                        "% kisses you."
                        "% kisses @.")
             (dup . pure $ Eating)
             (False, False)
             True
             Nothing
    , ExpCmd "kisscheek"
             (HasTarget "You kiss @ on the cheek."
                        "% kisses you on the cheek."
                        "% kisses @ on the cheek.")
             (pure Eating, [ Drinking, Eating, Sacrificing ])
             (False, False)
             True
             Nothing
    , ExpCmd "kisspassion"
             (HasTarget "You kiss @ passionately."
                        "% kisses you passionately."
                        "% kisses @ passionately.")
             mempties
             (False, False)
             True
             Nothing
    , ExpCmd "kneel"
             (Versatile "You kneel down."
                        "% kneels down."
                        "You kneel down before @."
                        "% kneels down before you."
                        "% kneels down before @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             (Just "kneeling down")
    , ExpCmd "laugh"
             (Versatile "You laugh."
                        "% laughs."
                        "You laugh at @."
                        "% laughs at you."
                        "% laughs at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "laughheart"
             (Versatile "You laugh heartily."
                        "% laughs heartily."
                        "You laugh heartily at @."
                        "% laughs heartily at you."
                        "% laughs heartily at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "laydown"
             (Versatile "You lay down."
                        "% lays down."
                        "You lay down next to @."
                        "% lays down next to you."
                        "% lays down next to @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             (Just "laying down")
    , ExpCmd "leap"
             (NoTarget  "You leap into the air."
                        "% leaps into the air.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "leer"
             (HasTarget "You leer at @."
                        "% leers at you."
                        "% leers at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "licklips"
             (NoTarget  "You lick your lips."
                        "% licks & lips.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "livid"
             (NoTarget  "You are positively livid."
                        "% is positively livid.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "longingly"
             (HasTarget "You gaze longingly at @."
                        "% gazes longingly at you."
                        "% gazes longingly at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "losswords"
             (NoTarget  "You appear to be at a loss for words."
                        "% appears to be at a loss for words.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "massage"
             (HasTarget "You massage @."
                        "% massages you."
                        "% massages @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "moan"
             (NoTarget  "You moan."
                        "% moans.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "mumble"
             (Versatile "You mumble to yourself."
                        "% mumbles to *."
                        "You mumble something to @."
                        "% mumbles something to you."
                        "% mumbles something to @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "muscles"
             (Versatile "You flex your muscles."
                        "%'s flexes & muscles."
                        "You flex your muscles at @."
                        "% flexes & muscles at you."
                        "% flexes & muscles at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "mutter"
             (Versatile "You mutter to yourself."
                        "% mutters to *."
                        "You mutter something to @."
                        "% mutters something to you."
                        "% mutters something to @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "nod"
             (Versatile "You nod."
                        "% nods."
                        "You nod to @."
                        "% nods to you."
                        "% nods to @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "nodagree"
             (Versatile "You nod in agreement."
                        "% nods in agreement."
                        "You nod to @ in agreement."
                        "% nods to you in agreement."
                        "% nods to @ in agreement.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "noexpress"
             (NoTarget  "Your face is entirely expressionless."
                        "%'s face is entirely expressionless.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "nudge"
             (HasTarget "You nudge @."
                        "% nudges you."
                        "% nudges @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "nuzzle"
             (HasTarget "You nuzzle @ lovingly."
                        "% nuzzles you lovingly."
                        "% nuzzles @ lovingly.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "openeyes"
             (NoTarget  "You open your eyes."
                        "% opens & eyes.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "openmouth"
             (Versatile "Your mouth hangs open."
                        "%'s mouth hangs open."
                        "Your mouth hangs open in response to @."
                        "%'s mouth hangs open in response to you."
                        "%'s mouth hangs open in response to @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "pace"
             (NoTarget  "You pace around nervously."
                        "% paces around nervously.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             (Just "")
    , ExpCmd "pant"
             (NoTarget  "You pant."
                        "% pants.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "patback"
             (HasTarget "You pat @ on the back."
                        "% pats you on the back."
                        "% pats @ on the back.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "pathead"
             (HasTarget "You pat @ on the head."
                        "% pats you on the head."
                        "% pats @ on the head.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "peer"
             (HasTarget "You peer at @."
                        "% peers at you."
                        "% peers at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "pensive"
             (Versatile "Your wistful expression suggests a pensive mood."
                        "%'s wistful expression suggests a pensive mood."
                        "You gaze pensively at @."
                        "% gazes pensively at you."
                        "% gazes pensively at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "perplexed"
             (NoTarget  "You are truly perplexed by the situation."
                        "% is truly perplexed by the situation.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "pet"
             (HasTarget "You pet @."
                        "% pets you."
                        "% pets @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "picknose"
             (NoTarget  "You pick your nose."
                        "% picks & nose.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "pinch"
             (HasTarget "You pinch @."
                        "% pinches you."
                        "% pinches @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "point"
             (HasTarget "You point to @."
                        "% points to you."
                        "% points to @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "poke"
             (HasTarget "You poke @."
                        "% pokes you."
                        "% pokes @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "ponder"
             (NoTarget  "You ponder the situation."
                        "% ponders the situation.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "pose"
             (Versatile "You strike a pose."
                        "% strikes a pose."
                        "You strike a pose before @."
                        "% strikes a pose before you."
                        "% strikes a pose before @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "pounce"
             (HasTarget "You pounce on @."
                        "% pounces on you."
                        "% pounces on @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             (Just "")
    , ExpCmd "pout"
             (Versatile "You pout."
                        "% pouts."
                        "You pout at @."
                        "% pouts at you."
                        "% pout at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "prance"
             (Versatile "You prance around."
                        "% prances around."
                        "You prance around @."
                        "% prances around you."
                        "% prances around @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "purr"
             (NoTarget  "You purr."
                        "% purrs.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "questioning"
             (Versatile "You have a questioning expression on your face."
                        "% has a questioning expression on & face."
                        "You flash a questioning expression at @."
                        "% flashes a questioning expression at you."
                        "% flashes a questioning expression at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "raisebrow"
             (Versatile "You raise an eyebrow."
                        "% raises an eyebrow."
                        "You raise an eyebrow at @."
                        "% raises an eyebrow at you."
                        "% raises an eyebrow at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "raisehand"
             (NoTarget  "You raise your hand."
                        "% raises & hand.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "reeling"
             (NoTarget  "Dizzy and reeling, you look as though you might pass out."
                        "Dizzy and reeling, % looks as though ^ might pass out.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "relieved"
             (NoTarget  "You look relieved."
                        "% looks relieved.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "rock"
             (NoTarget  "You rock back and forth."
                        "% rocks back and forth.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "rolleyes"
             (Versatile "You roll your eyes."
                        "% rolls & eyes."
                        "You roll your eyes at @."
                        "% rolls & eyes at you."
                        "% rolls & eyes at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "rubeyes"
             (NoTarget  "You rub your eyes."
                        "% rubs & eyes.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "ruffle"
             (HasTarget "You ruffle @'s hair."
                        "% ruffles your hair."
                        "% ruffles @'s hair.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "salute"
             (HasTarget "You salute @."
                        "% salutes you."
                        "% salutes @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "satisfied"
             (NoTarget  "You look satisfied."
                        "% looks satisfied.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "scowl"
             (Versatile "You scowl with contempt."
                        "% scowls with contempt."
                        "You scowl with contempt at @."
                        "% scowls with contempt at you."
                        "% scowls with contempt at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "scratchchin"
             (NoTarget  "You scratch your chin."
                        "% scratches & chin.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "scratchhead"
             (NoTarget  "You scratch your head."
                        "% scratches & head.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "scream"
             (Versatile "You unleash a high-pitched scream."
                        "% unleashes high-pitched scream."
                        "You scream at @."
                        "% screams at you."
                        "% screams at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "shake"
             (Versatile "You shake your head."
                        "% shakes & head."
                        "You shake your head at @."
                        "% shakes & head at you."
                        "% shakes & head at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "shiver"
             (NoTarget  "You shiver."
                        "% shivers.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "shriek"
             (NoTarget  "You let out a shriek."
                        "% lets out a shriek.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "shrieklaugh"
             (Versatile "You shriek with laughter."
                        "% shrieks with laughter."
                        "You shriek with laughter at @."
                        "% shrieks with laughter at you."
                        "% shrieks with laughter at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "shrug"
             (NoTarget  "You shrug your shoulders."
                        "% shrugs & shoulders.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "shudder"
             (NoTarget  "You shudder."
                        "% shudders.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "shush"
             (HasTarget "You shush @."
                        "% shushes you."
                        "% shushed @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "sigh"
             (NoTarget  "You sigh."
                        "% sighs.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "sighrelief"
             (NoTarget  "You sigh in relief."
                        "% sighs in relief.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "sighsadly"
             (NoTarget  "You sigh sadly."
                        "% sighs sadly.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "sighwearily"
             (NoTarget  "You sigh wearily."
                        "% sighs wearily.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "sit"
             (Versatile "You sit down."
                        "% sits down."
                        "You sit down next to @."
                        "% sits down next to you."
                        "% sit down next to @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             (Just "sitting down")
    , ExpCmd "sleepy"
             (NoTarget  "You look sleepy."
                        "% looks sleepy.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "slowclap"
             (Versatile "You clap slowly with a mocking lack of enthusiasm."
                        "% claps slowly with a mocking lack of enthusiasm."
                        "With a mocking lack of enthusiasm, you clap slowly for @."
                        "With a mocking lack of enthusiasm, % claps slowly for you."
                        "With a mocking lack of enthusiasm, % claps slowly for @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "smacklips"
             (NoTarget  "You smack your lips."
                        "% smacks & lips.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "smile"
             (Versatile "You smile."
                        "% smiles."
                        "You smile at @."
                        "% smiles at you."
                        "% smiles at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "smirk"
             (Versatile "You smirk."
                        "% smirks."
                        "You smirk at @."
                        "% smirks at you."
                        "% smirks at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "snap"
             (Versatile "You snap your fingers."
                        "% snaps & fingers."
                        "You snap your fingers at @."
                        "% snaps & fingers at you."
                        "% snaps & fingers at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "snarl"
             (Versatile "You snarl."
                        "% snarls."
                        "You snarl at @."
                        "% snarls at you."
                        "% snarls at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "sneeze"
             (NoTarget  "You sneeze."
                        "% sneezes.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "snicker"
             (Versatile "You snicker derisively."
                        "% snickers derisively."
                        "You snicker derisively at @."
                        "% snickers derisively at you."
                        "% snickers derisively at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "sniff"
             (NoTarget  "You sniff the air."
                        "% sniffs the air.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "sniffle"
             (NoTarget  "You sniffle."
                        "% sniffles.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "snore"
             (NoTarget  "You snore loudly."
                        "% snores loudly.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "snort"
             (Versatile "You snort."
                        "% snorts."
                        "You snort at @."
                        "% snorts at you."
                        "% snorts at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "sob"
             (NoTarget  "You sob."
                        "% sobs.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "spit"
             (Versatile "You spit."
                        "% spits."
                        "You spit on @."
                        "% spits on you."
                        "% spits on @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "stagger"
             (NoTarget  "You stagger around."
                        "% staggers around.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "stamp"
             (NoTarget  "Your stamp your feet."
                        "% stamps & feet.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "stand"
             (NoTarget  "You stand up."
                        "% stands up.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             (Just "")
    , ExpCmd "stare"
             (HasTarget "You stare at @."
                        "% stares at you."
                        "% stares at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "stiflelaugh"
             (NoTarget  "You try hard to stifle a laugh."
                        "% tries hard to stifle a laugh.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "stifletears"
             (NoTarget  "You try hard to stifle your tears."
                        "% tries hard to stifle & tears.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "stomach"
             (NoTarget  "Your stomach growls."
                        "%'s stomach growls.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "stomp"
             (NoTarget  "Your stomp your feet."
                        "% stomps & feet.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "stretch"
             (NoTarget  "You stretch your muscles."
                        "% stretches & muscles.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "strokehair"
             (HasTarget "You stroke @'s hair."
                        "% strokes your hair."
                        "% strokes @'s hair.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "strut"
             (NoTarget  "Your strut your stuff."
                        "% struts & stuff.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "stumble"
             (NoTarget  "You stumble and almost fall over."
                        "% stumbles and almost falls over.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "suckthumb"
             (NoTarget  "You suck your thumb."
                        "% sucks & thumb.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "sulk"
             (NoTarget  "You sulk."
                        "% sulks.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "sweat"
             (NoTarget  "You break out in a sweat."
                        "% breaks out in a sweat.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "tap"
             (HasTarget "You tap @ on the shoulder."
                        "% taps you on the shoulder."
                        "% taps @ on the shoulder.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "taunt"
             (HasTarget "You taunt @."
                        "% taunts you."
                        "% taunts @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "think"
             (NoTarget  "You say, \"Hmm...\" and think about it."
                        "% says, \"Hmm...\" and thinks about it.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "throat"
             (NoTarget  "You clear your throat."
                        "% clears & throat.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "thumbsdown"
             (Versatile "You give a thumbs down."
                        "% gives a thumbs down."
                        "You give a thumbs down to @."
                        "% gives a thumbs down to you."
                        "% gives a thumbs down to @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "thumbsup"
             (Versatile "You give a thumbs up."
                        "% gives a thumbs up."
                        "You give a thumbs up to @."
                        "% gives a thumbs up to you."
                        "% gives a thumbs up to @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "tickle"
             (HasTarget "You tickle @."
                        "% tickles you."
                        "% tickles @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "tongue"
             (Versatile "You stick your tongue out."
                        "% sticks & tongue out."
                        "You stick your tongue out at @."
                        "% sticks & tongue out at you."
                        "% sticks & tongue out at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "tremble"
             (Versatile "You tremble in fear."
                        "% trembles in fear."
                        "You tremble in fear of @."
                        "% trembles in fear of you."
                        "% trembles in fear of @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "turnhead"
             (HasTarget "You turn your head to look at @."
                        "% turns & head to look at you."
                        "% turns & head to look at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "twiddle"
             (NoTarget  "You twiddle your thumbs."
                        "% twiddles & thumbs.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "twirl"
             (NoTarget  "You twirl around."
                        "% twirls around.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "twitch"
             (NoTarget  "You twitch nervously."
                        "% twitches nervously.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "unamused"
             (Versatile "You are plainly unamused."
                        "% is plainly unamused."
                        "You are plainly unamused by @'s antics."
                        "% is plainly unamused by your antics."
                        "% is plainly unamused by @'s antics.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "watch"
             (HasTarget "You watch @ with interest."
                        "% watches you with interest."
                        "% watches @ with interest.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "wave"
             (Versatile "You wave."
                        "% waves."
                        "You wave at @."
                        "% waves at you."
                        "% waves at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "weary"
             (Versatile "Exhausted, your face displays a weary expression."
                        "Exhausted, %'s face displays a weary expression."
                        "You cast @ a weary glance."
                        "% casts you a weary glance."
                        "% casts @ a weary glance.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "whimper"
             (Versatile "You whimper."
                        "% whimpers."
                        "You whimper at @."
                        "% whimpers at you."
                        "% whimpers at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             True
             Nothing
    , ExpCmd "whistle"
             (NoTarget  "You whistle."
                        "% whistles.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing
    , ExpCmd "wiggle"
             (NoTarget  "You wiggle around."
                        "% wiggles around.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "wince"
             (NoTarget  "You wince in pain."
                        "% winces in pain.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "wink"
             (Versatile "You wink."
                        "% winks."
                        "You wink at @."
                        "% winks at you."
                        "% winks at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "wipeface"
             (NoTarget  "You wipe your face."
                        "% wipes & face.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             False
             Nothing
    , ExpCmd "wistful"
             (Versatile "Your wistful expression suggests a pensive mood."
                        "%'s wistful expression suggests a pensive mood."
                        "You gaze wistfully at @."
                        "% gazes wistfully at you."
                        "% gazes wistfully at @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "worried"
             (Versatile "You look genuinely worried."
                        "% looks genuinely worried."
                        "You look genuinely worried for @."
                        "% looks genuinely worried for you."
                        "% looks genuinely worried for @.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [ Attacking, Drinking, Eating, Sacrificing ])
             (dup False)
             False
             Nothing
    , ExpCmd "yawn"
             (NoTarget  "You yawn."
                        "% yawns.")
             ([ Attacking, Drinking, Eating, Sacrificing ], [])
             (dup False)
             True
             Nothing ]

expCmds :: HasCallStack => [Cmd]
expCmds = S.foldr helper [] expCmdSet
  where
    helper ec@(ExpCmd expCmdName _ _ _ _ _) = (Cmd { cmdName           = expCmdName
                                                   , cmdPriorityAbbrev = Nothing
                                                   , cmdFullName       = expCmdName
                                                   , cmdAction         = Action (expCmd ec) True
                                                   , cmdDesc           = "" } :)

expCmdNames :: HasCallStack => [Text]
expCmdNames = S.toList . S.map (\ExpCmd { expCmdName } -> expCmdName) $ expCmdSet

getExpCmdByName :: HasCallStack => ExpCmdName -> ExpCmd
getExpCmdByName cn = head . S.toList . S.filter (\ExpCmd { expCmdName } -> expCmdName == cn) $ expCmdSet

-----

expCmd :: HasCallStack => ExpCmd -> ActionFun
expCmd    (ExpCmd ecn HasTarget {} _ _ _     _) p@NoArgs {}          = advise p [] . sorryExpCmdRequiresTarget $ ecn
expCmd ec@(ExpCmd ecn ect          _ _ isVis _)   (NoArgs i mq cols) = getStateTime >>= \pair@(ms, _) -> case ect of
  (NoTarget  toSelf toOthers      ) | isPla i ms
                                    , r <- getRace i ms
                                    , r `elem` furRaces
                                    , ecn == "blush" -> wrapSend mq cols . sorryExpCmdBlush . pp $ r
                                    | otherwise      -> helper pair toSelf toOthers
  (Versatile toSelf toOthers _ _ _)                  -> helper pair toSelf toOthers
  _                                                  -> pmf "expCmd" ect
  where
    furRaces                        = [ Felinoid, Lagomorph, Vulpenoid ]
    helper (ms, ct) toSelf toOthers =
        let d                           = mkStdDesig i ms DoCap
            serialized                  = serializeDesigHelper d toOthers
            (heShe, hisHer, himHerself) = mkPros . getSex i $ ms
            substitutions               = [ ("%", serialized), ("^", heShe), ("&", hisHer), ("*", himHerself) ]
            toOthersBcast               = ( markForSpiritOnly isLit isVis . nlnl . replace substitutions $ toOthers
                                          , desigOtherIds d )
            isLit                       = isMobRmLit ct i ms
            tuple                       = (toSelf, pure toOthersBcast, toSelf)
        in expCmdHelper i ms mq cols ec Nothing tuple
expCmd    (ExpCmd ecn NoTarget {} _ _ _     _) p@(WithArgs     _ _  _    (_:_) ) = advise p [] . sorryExpCmdIllegalTarget $ ecn
expCmd ec@(ExpCmd _   ect         _ _ isVis _)   (OneArgNubbed i mq cols target) = case ect of
  (HasTarget     toSelf toTarget toOthers) -> helper toSelf toTarget toOthers
  (Versatile _ _ toSelf toTarget toOthers) -> helper toSelf toTarget toOthers
  _                                        -> pmf "expCmd" ect
  where
    helper toSelf toTarget toOthers = getStateTime >>= \(ms, ct) -> case singleArgInvEqRm InRm target of
      (InRm, target') ->
          let d                                = mkStdDesig i ms DoCap
              (first (i `delete`) -> invCoins) = getMobRmInvCoins i ms
          in if ()!# invCoins
            then case uncurry (resolveRmInvCoins i ms . pure $ target') invCoins of
              (_,                  [Left [sorryMsg]]) -> wrapSend mq cols sorryMsg
              (_,                  Right _:_        ) -> wrapSend mq cols sorryExpCmdCoins
              ([Left sorryMsg   ], _                ) -> wrapSend mq cols sorryMsg
              ([Right (_:_:_)   ], _                ) -> wrapSend mq cols adviceExpCmdExcessArgs
              ([Right [targetId]], _                ) ->
                let ioHelper targetDesigTxt =
                        let (toSelf', toOthers', logMsg, substitutions) = mkBindings targetDesigTxt
                            toTarget'     = replace substitutions toTarget
                            toTargetBcast = (nlnl toTarget', pure targetId)
                            toOthersBcast = (nlnl toOthers', targetId `delete` desigOtherIds d)
                            bs            = [ (markForSpiritOnly isLit isVis txt, is)
                                            | (txt, is) <- [ toTargetBcast, toOthersBcast ] ]
                            isLit         = isMobRmLit ct i ms
                            tuple         = (toSelf', bs, logMsg)
                        in expCmdHelper i ms mq cols ec (Just targetId) tuple
                    mkBindings targetTxt = let msg                         = replace (pure ("@", targetTxt)) toSelf
                                               toSelf'                     = parseDesig Nothing i ms msg
                                               logMsg                      = parseDesigSuffix   i ms msg
                                               serialized                  = serializeDesigHelper d toOthers
                                               (heShe, hisHer, himHerself) = mkPros . getSex i $ ms
                                               toOthers'                   = replace substitutions toOthers
                                               substitutions               = [ ("@", targetTxt)
                                                                             , ("%", serialized)
                                                                             , ("^", heShe)
                                                                             , ("&", hisHer)
                                                                             , ("*", himHerself) ]
                                           in (toSelf', toOthers', logMsg, substitutions)
                in if getType targetId ms `elem` [ PlaType, NpcType ]
                  then ioHelper . serialize . mkStdDesig targetId ms $ Don'tCap
                  else wrapSend mq cols sorryExpCmdTargetType
              x -> pmf "expCmd helper" x
            else wrapSend mq cols sorryNoOneHere
      (x, _) -> wrapSend mq cols . sorryExpCmdInInvEq $ x
expCmd _ p = advise p [] adviceExpCmdExcessArgs

markForSpiritOnly :: Bool -> Bool -> Text -> Text
markForSpiritOnly isLit isVis | isLit || isVis = id
                              | otherwise      = (forSpiritOnlyMarker `T.cons`) -- Spirits can see in the dark.

expCmdHelper :: HasCallStack => Id
                             -> MudState
                             -> MsgQueue
                             -> Cols
                             -> ExpCmd
                             -> Maybe Id
                             -> (Text, [Broadcast], Text)
                             -> MudStack ()
expCmdHelper i ms mq cols ExpCmd { .. } target (toSelf, bs, logMsg) = case myActs `intersect` ngActsSelf of
  []      -> case target of Nothing       -> next
                            Just targetId -> case getActs targetId ms `intersect` ngActsTarget of
                                               []      -> next
                                               (act:_) -> wrapSend mq cols . sorryExpCmdTargetActing $ act
  (act:_) -> wrapSend mq cols . sorryExpCmdActing $ act
  where
    myActs                     = getActs i ms
    (ngActsSelf, ngActsTarget) = expCmdActs & both %~ (allValues \\)
    next     = let f (b, act, stopper)          = when (b && act `elem` myActs) . stopper p $ ms
                   p                            = ActionParams i mq cols []
                   (stopsDrinking, stopsEating) = expCmdStopsActing
               in (>> ioHelper) .  mapM_ f $ [ (stopsDrinking, Drinking, stopDrinking)
                                             , (stopsEating,   Eating,   stopEating  ) ]
    ioHelper = do logPlaOut expCmdName i . pure $ logMsg
                  wrapSend mq cols toSelf
                  bcastIfNotIncog i bs
                  mobRmDescHelper i expDesc

mobRmDescHelper :: HasCallStack => Id -> MobRmDesc -> MudStack ()
mobRmDescHelper _ Nothing    = unit
mobRmDescHelper i (Just "" ) = tweak $ mobTbl.ind i.mobRmDesc .~ Nothing
mobRmDescHelper i (Just txt) = tweak $ mobTbl.ind i.mobRmDesc ?~ txt

serializeDesigHelper :: HasCallStack => Desig -> Text -> Text
serializeDesigHelper d toOthers = serialize . bool d { desigCap = Don'tCap } d $ T.head toOthers == '%'

-----

mkExpAction :: HasCallStack => Text -> ActionFun
mkExpAction name = expCmd . head . S.toList . S.filter helper $ expCmdSet
  where
    helper ExpCmd { expCmdName } = expCmdName == name
