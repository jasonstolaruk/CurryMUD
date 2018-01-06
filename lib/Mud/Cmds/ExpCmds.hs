{-# LANGUAGE NamedFieldPuns, OverloadedStrings, ViewPatterns #-}

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
import           Control.Lens.Operators ((?~), (.~))
import           Data.Bool
import           Data.List (delete)
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

-- TODO: Consider "ActType". For example, you shouldn't be able to kiss someone who is eating, or if you yourself are eating.
expCmdSet :: HasCallStack => S.Set ExpCmd
expCmdSet = S.fromList
    [ ExpCmd "admire"
             (HasTarget "You admire @."
                        "% admires you."
                        "% admires @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "applaud"
             (Versatile "You applaud enthusiastically."
                        "% applauds enthusiastically."
                        "You applaud enthusiastically for @."
                        "% applauds enthusiastically for you."
                        "% applauds enthusiastically for @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "astonished"
             (Versatile "You are absolutely astonished."
                        "% is absolutely astonished."
                        "You stare at @ with an astonished expression on your face."
                        "% stares at you with an astonished expression on & face."
                        "% stares at @ with an astonished expression on & face.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "astounded"
             (Versatile "You are altogether astounded."
                        "% is altogether astounded."
                        "You are altogether astounded at @."
                        "% is altogether astounded at you."
                        "% is altogether astounded at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "avert"
             (Versatile "You avert your eyes."
                        "% averts & eyes."
                        "You avert your eyes from @."
                        "% averts & eyes from you."
                        "% averts & eyes from @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "bawl"
             (NoTarget  "You bawl like a baby."
                        "% bawls like a baby.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "beam"
             (Versatile "You beam cheerfully."
                        "% beams cheerfully."
                        "You beam cheerfully at @."
                        "% beams cheerfully at you."
                        "% beams cheerfully at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "belch"
             (Versatile "You let out a deep belch."
                        "% lets out a deep belch."
                        "You belch purposefully at @."
                        "% belches purposefully at you."
                        "% belches purposefully at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "bewildered"
             (Versatile "You are hopelessly bewildered."
                        "% is hopelessly bewildered."
                        "You are hopelessly bewildered by @'s behavior."
                        "% is hopelessly bewildered by your behavior."
                        "% is hopelessly bewildered by @'s behavior.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "blank"
             (Versatile "You have a blank expression on your face."
                        "% has a blank expression on & face."
                        "You look blankly at @."
                        "% looks blankly at you."
                        "% looks blankly at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "blink"
             (Versatile "You blink."
                        "% blinks."
                        "You blink at @."
                        "% blinks at you."
                        "% blinks at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "blush"
             (NoTarget  "You blush."
                        "% blushes.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "boggle"
             (NoTarget  "You boggle at the concept."
                        "% boggles at the concept.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "bow"
             (Versatile "You bow."
                        "% bows."
                        "You bow before @."
                        "% bows before you."
                        "% bows before @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "burp"
             (Versatile "You burp."
                        "% burps."
                        "You burp rudely at @."
                        "% burps rudely at you."
                        "% burps rudely at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "burstlaugh"
             (Versatile "You abruptly burst into laughter."
                        "% abruptly bursts into laughter."
                        "You abruptly burst into laughter in reaction to @."
                        "% abruptly bursts into laughter in reaction to you."
                        "% abruptly bursts into laughter in reaction to @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "calm"
             (NoTarget  "You appear calm and collected."
                        "% appears calm and collected.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "caresscheek"
             (HasTarget "You caress @'s cheek."
                        "% caresses your cheek."
                        "% caresses @'s cheek.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "cheer"
             (Versatile "You cheer eagerly."
                        "% cheers eagerly."
                        "You cheer eagerly for @."
                        "% cheers eagerly for you."
                        "% cheers eagerly for @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "chortle"
             (Versatile "You chortle gleefully."
                        "% chortles gleefully."
                        "You chortle gleefully at @."
                        "% chortles gleefully at you."
                        "% chortles gleefully at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "chuckle"
             (Versatile "You chuckle."
                        "% chuckles."
                        "You chuckle at @."
                        "% chuckles at you."
                        "% chuckles at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "clap"
             (Versatile "You clap."
                        "% claps."
                        "You clap for @."
                        "% claps for you."
                        "% claps for @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "closeeyes"
             (NoTarget  "You close your eyes."
                        "% closes & eyes.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "coldsweat"
             (NoTarget  "You break out in a cold sweat."
                        "% breaks out in a cold sweat.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "comfort"
             (HasTarget "You comfort @."
                        "% comforts you."
                        "% comforts @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "confused"
             (Versatile "You look utterly confused."
                        "% looks utterly confused."
                        "Utterly confused, you look fixedly at @."
                        "Utterly confused, % looks fixedly at you."
                        "Utterly confused, % looks fixedly at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "cough"
             (Versatile "You cough."
                        "% coughs."
                        "You cough loudly at @."
                        "% coughs loudly at you."
                        "% coughs loudly at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "coverears"
             (NoTarget  "You cover your ears."
                        "% covers & ears.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "covereyes"
             (NoTarget  "You cover your eyes."
                        "% covers & eyes.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "covermouth"
             (NoTarget  "You cover your mouth."
                        "% covers & mouth.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "cower"
             (Versatile "You cower in fear."
                        "% cowers in fear."
                        "You cower in fear before @."
                        "% cowers in fear before you."
                        "% cowers in fear before @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "cringe"
             (Versatile "You cringe."
                        "% cringes."
                        "You cringe at @."
                        "% cringes at you."
                        "% cringes at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "crossarms"
             (NoTarget  "You cross your arms."
                        "% crosses & arms.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "crossfingers"
             (NoTarget  "You cross your fingers."
                        "% crosses & fingers.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "cry"
             (NoTarget  "You cry."
                        "% cries.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "cryanger"
             (Versatile "You cry out in anger."
                        "% cries out in anger."
                        "You cry out in anger at @."
                        "% cries out in anger at you."
                        "% cries out in anger at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "cuddle"
             (HasTarget "You cuddle with @."
                        "% cuddles with you."
                        "% cuddles with @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "curious"
             (Versatile "You have a curious expression on your face."
                        "% has a curious expression on & face."
                        "You flash a curious expression at @."
                        "% flashes a curious expression at you."
                        "% flashes a curious expression at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "curse"
             (Versatile "You curse."
                        "% curses."
                        "You curse at @."
                        "% curses at you."
                        "% curses at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "curtsey"
             (Versatile "You curtsey."
                        "% curtseys."
                        "You curtsey to @."
                        "% curtseys to you."
                        "% curtseys to @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "curtsy"
             (Versatile "You curtsy."
                        "% curtsies."
                        "You curtsy to @."
                        "% curtsies to you."
                        "% curtsies to @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "dance"
             (Versatile "You dance around."
                        "% dances around."
                        "You dance with @."
                        "% dances with you."
                        "% dances with @.")
             (dup allValues)
             True
             (Just "")
    , ExpCmd "daydream"
             (NoTarget  "Staring off into the distance, you indulge in a daydream."
                        "Staring off into the distance, % indulges in a daydream.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "deepbreath"
             (NoTarget  "You take a deep breath."
                        "% takes a deep breath.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "disappoint"
             (Versatile "You are clearly disappointed."
                        "% is clearly disappointed."
                        "You are clearly disappointed in @."
                        "% is clearly disappointed in you."
                        "% is clearly disappointed in @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "dizzy"
             (NoTarget  "Dizzy and reeling, you look as though you might pass out."
                        "Dizzy and reeling, % looks as though ^ might pass out.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "doublelaugh"
             (Versatile "You double over with laughter."
                        "% doubles over with laughter."
                        "You double over with laughter in reaction to @."
                        "% doubles over with laughter in reaction to you."
                        "% doubles over with laughter in reaction to @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "drool"
             (NoTarget  "You drool."
                        "% drools.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "droop"
             (NoTarget  "Your eyes droop."
                        "%'s eyes droop.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "duck"
             (Versatile "You duck."
                        "% ducks."
                        "You duck away from @."
                        "% ducks away from you."
                        "% ducks away from @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "embrace"
             (HasTarget "You warmly embrace @."
                        "% embraces you warmly."
                        "% embraces @ warmly.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "exhausted"
             (NoTarget  "Exhausted, your face displays a weary expression."
                        "Exhausted, %'s face displays a weary expression.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "facepalm"
             (NoTarget  "You facepalm."
                        "% facepalms.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "faint"
             (NoTarget  "You faint."
                        "% faints.")
             (dup allValues)
             True
             (Just "fainted")
    , ExpCmd "fistpump"
             (NoTarget  "You pump your fist in the air triumphantly."
                        "% pumps & fist in the air triumphantly.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "flex"
             (Versatile "You flex your muscles."
                        "%'s flexes & muscles."
                        "You flex your muscles at @."
                        "% flexes & muscles at you."
                        "% flexes & muscles at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "flinch"
             (NoTarget  "You flinch."
                        "% flinches.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "flop"
             (NoTarget  "You flop down on the ground."
                        "% flops down on the ground.")
             (dup allValues)
             True
             (Just "on the ground")
    , ExpCmd "flustered"
             (NoTarget  "You look entirely flustered."
                        "% looks entirely flustered.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "frown"
             (Versatile "You frown."
                        "% frowns."
                        "You frown at @."
                        "% frowns at you."
                        "% frowns at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "funnyface"
             (Versatile "You make a funny face."
                        "% makes a funny face."
                        "You make a funny face at @."
                        "% makes a funny face at you."
                        "% makes a funny face at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "gape"
             (Versatile "You gape."
                        "% gapes."
                        "You gape at @."
                        "% gapes at you."
                        "% gapes at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "gasp"
             (Versatile "You gasp."
                        "% gasps."
                        "You gasp at @."
                        "% gasps at you."
                        "% gasps at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "gawk"
             (HasTarget "You gawk at @."
                        "% gawks at you."
                        "% gawks at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "gaze"
             (HasTarget "You gaze longingly at @."
                        "% gazes longingly at you."
                        "% gazes longingly at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "giggle"
             (Versatile "You giggle."
                        "% giggles."
                        "You giggle at @."
                        "% giggles at you."
                        "% giggles at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "glance"
             (Versatile "You glance around."
                        "% glances around."
                        "You glance at @."
                        "% glances at you."
                        "% glances at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "glare"
             (HasTarget "You glare at @."
                        "% glares at you."
                        "% glares at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "greet"
             (HasTarget "You greet @."
                        "% greets you."
                        "% greets @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "grimace"
             (Versatile "You grimace disapprovingly."
                        "% grimaces disapprovingly."
                        "You grimace disapprovingly at @."
                        "% grimaces disapprovingly at you."
                        "% grimaces disapprovingly at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "grin"
             (Versatile "You grin."
                        "% grins."
                        "You grin at @."
                        "% grins at you."
                        "% grins at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "groan"
             (Versatile "You groan."
                        "% groans."
                        "You groan at @."
                        "% groans at you."
                        "% groans at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "grovel"
             (HasTarget "You grovel before @."
                        "% grovels before you."
                        "% grovels before @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "growl"
             (Versatile "You growl menacingly."
                        "% growls menacingly."
                        "You growl menacingly at @."
                        "% growls menacingly at you."
                        "% growls menacingly at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "grumble"
             (NoTarget  "You grumble to yourself."
                        "% grumbles to *.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "guffaw"
             (Versatile "You guffaw boisterously."
                        "% guffaws boisterously."
                        "You guffaw boisterously at @."
                        "% guffaws boisterously at you."
                        "% guffaws boisterously at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "gulp"
             (NoTarget  "You gulp."
                        "% gulps.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "handhips"
             (NoTarget  "You put your hands on your hips."
                        "% puts & hands on & hips.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "hesitate"
             (NoTarget  "You hesitate."
                        "% hesitates.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "hiccup"
             (NoTarget  "You hiccup involuntarily."
                        "% hiccups involuntarily.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "highfive"
             (HasTarget "You give @ a high five."
                        "% gives you a high five."
                        "% gives @ a high five.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "hmm"
             (NoTarget  "You say, \"Hmm...\" and think about it."
                        "% says, \"Hmm...\" and thinks about it.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "holdhand"
             (HasTarget "You hold @'s hand."
                        "% holds your hand."
                        "% holds @'s hand.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "hop"
             (NoTarget  "You hop up and down excitedly."
                        "% hops up and down excitedly.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "howllaugh"
             (Versatile "You howl with laughter."
                        "% howls with laughter."
                        "You howl with laughter at @."
                        "% howls with laughter at you."
                        "% howls with laughter at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "hug"
             (HasTarget "You hug @."
                        "% hugs you."
                        "% hugs @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "hum"
             (NoTarget  "You hum a merry tune."
                        "% hums a merry tune.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "innocent"
             (NoTarget  "You try to look innocent."
                        "% tries to look innocent.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "inquisitive"
             (Versatile "You have an inquisitive expression on your face."
                        "% has an inquisitive expression on & face."
                        "You flash an inquisitive expression at @."
                        "% flashes an inquisitive expression at you."
                        "% flashes an inquisitive expression at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "jig"
             (Versatile "You dance a lively jig."
                        "% dances a lively jig."
                        "You dance a lively jig with @."
                        "% dances a lively jig with you."
                        "% dances a lively jig with @.")
             (dup allValues)
             True
             (Just "")
    , ExpCmd "joytears"
             (NoTarget  "You are overcome with tears of joy."
                        "% is overcome with tears of joy.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "jump"
             (NoTarget  "You jump up and down excitedly."
                        "% jumps up and down excitedly.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "kiss"
             (HasTarget "You kiss @."
                        "% kisses you."
                        "% kisses @.")
             (dup . pure $ Eating)
             True
             Nothing
    , ExpCmd "kisscheek"
             (HasTarget "You kiss @ on the cheek."
                        "% kisses you on the cheek."
                        "% kisses @ on the cheek.")
             (pure Eating, [ Drinking, Eating, Sacrificing ])
             True
             Nothing
    , ExpCmd "kisspassion"
             (HasTarget "You kiss @ passionately."
                        "% kisses you passionately."
                        "% kisses @ passionately.")
             mempties
             True
             Nothing
    , ExpCmd "kneel"
             (Versatile "You kneel down."
                        "% kneels down."
                        "You kneel down before @."
                        "% kneels down before you."
                        "% kneels down before @.")
             (dup allValues)
             True
             (Just "kneeling down")
    , ExpCmd "laugh"
             (Versatile "You laugh."
                        "% laughs."
                        "You laugh at @."
                        "% laughs at you."
                        "% laughs at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "laughheart"
             (Versatile "You laugh heartily."
                        "% laughs heartily."
                        "You laugh heartily at @."
                        "% laughs heartily at you."
                        "% laughs heartily at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "laydown"
             (Versatile "You lay down."
                        "% lays down."
                        "You lay down next to @."
                        "% lays down next to you."
                        "% lays down next to @.")
             (dup allValues)
             True
             (Just "laying down")
    , ExpCmd "leap"
             (NoTarget  "You leap into the air."
                        "% leaps into the air.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "leer"
             (HasTarget "You leer at @."
                        "% leers at you."
                        "% leers at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "licklips"
             (NoTarget  "You lick your lips."
                        "% licks & lips.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "livid"
             (NoTarget  "You are positively livid."
                        "% is positively livid.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "longingly"
             (HasTarget "You gaze longingly at @."
                        "% gazes longingly at you."
                        "% gazes longingly at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "losswords"
             (NoTarget  "You appear to be at a loss for words."
                        "% appears to be at a loss for words.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "massage"
             (HasTarget "You massage @."
                        "% massages you."
                        "% massages @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "moan"
             (NoTarget  "You moan."
                        "% moans.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "mumble"
             (Versatile "You mumble to yourself."
                        "% mumbles to *."
                        "You mumble something to @."
                        "% mumbles something to you."
                        "% mumbles something to @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "muscles"
             (Versatile "You flex your muscles."
                        "%'s flexes & muscles."
                        "You flex your muscles at @."
                        "% flexes & muscles at you."
                        "% flexes & muscles at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "mutter"
             (Versatile "You mutter to yourself."
                        "% mutters to *."
                        "You mutter something to @."
                        "% mutters something to you."
                        "% mutters something to @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "nod"
             (Versatile "You nod."
                        "% nods."
                        "You nod to @."
                        "% nods to you."
                        "% nods to @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "nodagree"
             (Versatile "You nod in agreement."
                        "% nods in agreement."
                        "You nod to @ in agreement."
                        "% nods to you in agreement."
                        "% nods to @ in agreement.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "noexpress"
             (NoTarget  "Your face is entirely expressionless."
                        "%'s face is entirely expressionless.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "nudge"
             (HasTarget "You nudge @."
                        "% nudges you."
                        "% nudges @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "nuzzle"
             (HasTarget "You nuzzle @ lovingly."
                        "% nuzzles you lovingly."
                        "% nuzzles @ lovingly.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "openeyes"
             (NoTarget  "You open your eyes."
                        "% opens & eyes.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "openmouth"
             (Versatile "Your mouth hangs open."
                        "%'s mouth hangs open."
                        "Your mouth hangs open in response to @."
                        "%'s mouth hangs open in response to you."
                        "%'s mouth hangs open in response to @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "pace"
             (NoTarget  "You pace around nervously."
                        "% paces around nervously.")
             (dup allValues)
             True
             (Just "")
    , ExpCmd "pant"
             (NoTarget  "You pant."
                        "% pants.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "patback"
             (HasTarget "You pat @ on the back."
                        "% pats you on the back."
                        "% pats @ on the back.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "pathead"
             (HasTarget "You pat @ on the head."
                        "% pats you on the head."
                        "% pats @ on the head.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "peer"
             (HasTarget "You peer at @."
                        "% peers at you."
                        "% peers at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "pensive"
             (Versatile "Your wistful expression suggests a pensive mood."
                        "%'s wistful expression suggests a pensive mood."
                        "You gaze pensively at @."
                        "% gazes pensively at you."
                        "% gazes pensively at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "perplexed"
             (NoTarget  "You are truly perplexed by the situation."
                        "% is truly perplexed by the situation.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "pet"
             (HasTarget "You pet @."
                        "% pets you."
                        "% pets @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "picknose"
             (NoTarget  "You pick your nose."
                        "% picks & nose.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "pinch"
             (HasTarget "You pinch @."
                        "% pinches you."
                        "% pinches @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "point"
             (HasTarget "You point to @."
                        "% points to you."
                        "% points to @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "poke"
             (HasTarget "You poke @."
                        "% pokes you."
                        "% pokes @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "ponder"
             (NoTarget  "You ponder the situation."
                        "% ponders the situation.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "pose"
             (Versatile "You strike a pose."
                        "% strikes a pose."
                        "You strike a pose before @."
                        "% strikes a pose before you."
                        "% strikes a pose before @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "pounce"
             (HasTarget "You pounce on @."
                        "% pounces on you."
                        "% pounces on @.")
             (dup allValues)
             True
             (Just "")
    , ExpCmd "pout"
             (Versatile "You pout."
                        "% pouts."
                        "You pout at @."
                        "% pouts at you."
                        "% pout at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "prance"
             (Versatile "You prance around."
                        "% prances around."
                        "You prance around @."
                        "% prances around you."
                        "% prances around @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "purr"
             (NoTarget  "You purr."
                        "% purrs.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "questioning"
             (Versatile "You have a questioning expression on your face."
                        "% has a questioning expression on & face."
                        "You flash a questioning expression at @."
                        "% flashes a questioning expression at you."
                        "% flashes a questioning expression at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "raisebrow"
             (Versatile "You raise an eyebrow."
                        "% raises an eyebrow."
                        "You raise an eyebrow at @."
                        "% raises an eyebrow at you."
                        "% raises an eyebrow at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "raisehand"
             (NoTarget  "You raise your hand."
                        "% raises & hand.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "reeling"
             (NoTarget  "Dizzy and reeling, you look as though you might pass out."
                        "Dizzy and reeling, % looks as though ^ might pass out.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "relieved"
             (NoTarget  "You look relieved."
                        "% looks relieved.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "rock"
             (NoTarget  "You rock back and forth."
                        "% rocks back and forth.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "rolleyes"
             (Versatile "You roll your eyes."
                        "% rolls & eyes."
                        "You roll your eyes at @."
                        "% rolls & eyes at you."
                        "% rolls & eyes at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "rubeyes"
             (NoTarget  "You rub your eyes."
                        "% rubs & eyes.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "ruffle"
             (HasTarget "You ruffle @'s hair."
                        "% ruffles your hair."
                        "% ruffles @'s hair.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "salute"
             (HasTarget "You salute @."
                        "% salutes you."
                        "% salutes @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "satisfied"
             (NoTarget  "You look satisfied."
                        "% looks satisfied.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "scowl"
             (Versatile "You scowl with contempt."
                        "% scowls with contempt."
                        "You scowl with contempt at @."
                        "% scowls with contempt at you."
                        "% scowls with contempt at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "scratchchin"
             (NoTarget  "You scratch your chin."
                        "% scratches & chin.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "scratchhead"
             (NoTarget  "You scratch your head."
                        "% scratches & head.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "scream"
             (Versatile "You unleash a high-pitched scream."
                        "% unleashes high-pitched scream."
                        "You scream at @."
                        "% screams at you."
                        "% screams at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "shake"
             (Versatile "You shake your head."
                        "% shakes & head."
                        "You shake your head at @."
                        "% shakes & head at you."
                        "% shakes & head at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "shiver"
             (NoTarget  "You shiver."
                        "% shivers.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "shriek"
             (NoTarget  "You let out a shriek."
                        "% lets out a shriek.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "shrieklaugh"
             (Versatile "You shriek with laughter."
                        "% shrieks with laughter."
                        "You shriek with laughter at @."
                        "% shrieks with laughter at you."
                        "% shrieks with laughter at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "shrug"
             (NoTarget  "You shrug your shoulders."
                        "% shrugs & shoulders.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "shudder"
             (NoTarget  "You shudder."
                        "% shudders.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "shush"
             (HasTarget "You shush @."
                        "% shushes you."
                        "% shushed @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "sigh"
             (NoTarget  "You sigh."
                        "% sighs.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "sighrelief"
             (NoTarget  "You sigh in relief."
                        "% sighs in relief.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "sighsadly"
             (NoTarget  "You sigh sadly."
                        "% sighs sadly.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "sighwearily"
             (NoTarget  "You sigh wearily."
                        "% sighs wearily.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "sit"
             (Versatile "You sit down."
                        "% sits down."
                        "You sit down next to @."
                        "% sits down next to you."
                        "% sit down next to @.")
             (dup allValues)
             True
             (Just "sitting down")
    , ExpCmd "sleepy"
             (NoTarget  "You look sleepy."
                        "% looks sleepy.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "slowclap"
             (Versatile "You clap slowly with a mocking lack of enthusiasm."
                        "% claps slowly with a mocking lack of enthusiasm."
                        "With a mocking lack of enthusiasm, you clap slowly for @."
                        "With a mocking lack of enthusiasm, % claps slowly for you."
                        "With a mocking lack of enthusiasm, % claps slowly for @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "smacklips"
             (NoTarget  "You smack your lips."
                        "% smacks & lips.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "smile"
             (Versatile "You smile."
                        "% smiles."
                        "You smile at @."
                        "% smiles at you."
                        "% smiles at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "smirk"
             (Versatile "You smirk."
                        "% smirks."
                        "You smirk at @."
                        "% smirks at you."
                        "% smirks at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "snap"
             (Versatile "You snap your fingers."
                        "% snaps & fingers."
                        "You snap your fingers at @."
                        "% snaps & fingers at you."
                        "% snaps & fingers at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "snarl"
             (Versatile "You snarl."
                        "% snarls."
                        "You snarl at @."
                        "% snarls at you."
                        "% snarls at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "sneeze"
             (NoTarget  "You sneeze."
                        "% sneezes.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "snicker"
             (Versatile "You snicker derisively."
                        "% snickers derisively."
                        "You snicker derisively at @."
                        "% snickers derisively at you."
                        "% snickers derisively at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "sniff"
             (NoTarget  "You sniff the air."
                        "% sniffs the air.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "sniffle"
             (NoTarget  "You sniffle."
                        "% sniffles.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "snore"
             (NoTarget  "You snore loudly."
                        "% snores loudly.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "snort"
             (Versatile "You snort."
                        "% snorts."
                        "You snort at @."
                        "% snorts at you."
                        "% snorts at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "sob"
             (NoTarget  "You sob."
                        "% sobs.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "spit"
             (Versatile "You spit."
                        "% spits."
                        "You spit on @."
                        "% spits on you."
                        "% spits on @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "stagger"
             (NoTarget  "You stagger around."
                        "% staggers around.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "stamp"
             (NoTarget  "Your stamp your feet."
                        "% stamps & feet.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "stand"
             (NoTarget  "You stand up."
                        "% stands up.")
             (dup allValues)
             True
             (Just "")
    , ExpCmd "stare"
             (HasTarget "You stare at @."
                        "% stares at you."
                        "% stares at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "stiflelaugh"
             (NoTarget  "You try hard to stifle a laugh."
                        "% tries hard to stifle a laugh.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "stifletears"
             (NoTarget  "You try hard to stifle your tears."
                        "% tries hard to stifle & tears.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "stomach"
             (NoTarget  "Your stomach growls."
                        "%'s stomach growls.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "stomp"
             (NoTarget  "Your stomp your feet."
                        "% stomps & feet.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "stretch"
             (NoTarget  "You stretch your muscles."
                        "% stretches & muscles.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "strokehair"
             (HasTarget "You stroke @'s hair."
                        "% strokes your hair."
                        "% strokes @'s hair.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "strut"
             (NoTarget  "Your strut your stuff."
                        "% struts & stuff.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "stumble"
             (NoTarget  "You stumble and almost fall over."
                        "% stumbles and almost falls over.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "suckthumb"
             (NoTarget  "You suck your thumb."
                        "% sucks & thumb.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "sulk"
             (NoTarget  "You sulk."
                        "% sulks.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "sweat"
             (NoTarget  "You break out in a sweat."
                        "% breaks out in a sweat.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "tap"
             (HasTarget "You tap @ on the shoulder."
                        "% taps you on the shoulder."
                        "% taps @ on the shoulder.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "taunt"
             (HasTarget "You taunt @."
                        "% taunts you."
                        "% taunts @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "think"
             (NoTarget  "You say, \"Hmm...\" and think about it."
                        "% says, \"Hmm...\" and thinks about it.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "throat"
             (NoTarget  "You clear your throat."
                        "% clears & throat.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "thumbsdown"
             (Versatile "You give a thumbs down."
                        "% gives a thumbs down."
                        "You give a thumbs down to @."
                        "% gives a thumbs down to you."
                        "% gives a thumbs down to @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "thumbsup"
             (Versatile "You give a thumbs up."
                        "% gives a thumbs up."
                        "You give a thumbs up to @."
                        "% gives a thumbs up to you."
                        "% gives a thumbs up to @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "tickle"
             (HasTarget "You tickle @."
                        "% tickles you."
                        "% tickles @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "tongue"
             (Versatile "You stick your tongue out."
                        "% sticks & tongue out."
                        "You stick your tongue out at @."
                        "% sticks & tongue out at you."
                        "% sticks & tongue out at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "tremble"
             (Versatile "You tremble in fear."
                        "% trembles in fear."
                        "You tremble in fear of @."
                        "% trembles in fear of you."
                        "% trembles in fear of @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "turnhead"
             (HasTarget "You turn your head to look at @."
                        "% turns & head to look at you."
                        "% turns & head to look at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "twiddle"
             (NoTarget  "You twiddle your thumbs."
                        "% twiddles & thumbs.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "twirl"
             (NoTarget  "You twirl around."
                        "% twirls around.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "twitch"
             (NoTarget  "You twitch nervously."
                        "% twitches nervously.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "unamused"
             (Versatile "You are plainly unamused."
                        "% is plainly unamused."
                        "You are plainly unamused by @'s antics."
                        "% is plainly unamused by your antics."
                        "% is plainly unamused by @'s antics.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "watch"
             (HasTarget "You watch @ with interest."
                        "% watches you with interest."
                        "% watches @ with interest.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "wave"
             (Versatile "You wave."
                        "% waves."
                        "You wave at @."
                        "% waves at you."
                        "% waves at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "weary"
             (Versatile "Exhausted, your face displays a weary expression."
                        "Exhausted, %'s face displays a weary expression."
                        "You cast @ a weary glance."
                        "% casts you a weary glance."
                        "% casts @ a weary glance.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "whimper"
             (Versatile "You whimper."
                        "% whimpers."
                        "You whimper at @."
                        "% whimpers at you."
                        "% whimpers at @.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "whistle"
             (NoTarget  "You whistle."
                        "% whistles.")
             (dup allValues)
             True
             Nothing
    , ExpCmd "wiggle"
             (NoTarget  "You wiggle around."
                        "% wiggles around.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "wince"
             (NoTarget  "You wince in pain."
                        "% winces in pain.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "wink"
             (Versatile "You wink."
                        "% winks."
                        "You wink at @."
                        "% winks at you."
                        "% winks at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "wipeface"
             (NoTarget  "You wipe your face."
                        "% wipes & face.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "wistful"
             (Versatile "Your wistful expression suggests a pensive mood."
                        "%'s wistful expression suggests a pensive mood."
                        "You gaze wistfully at @."
                        "% gazes wistfully at you."
                        "% gazes wistfully at @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "worried"
             (Versatile "You look genuinely worried."
                        "% looks genuinely worried."
                        "You look genuinely worried for @."
                        "% looks genuinely worried for you."
                        "% looks genuinely worried for @.")
             (dup allValues)
             False
             Nothing
    , ExpCmd "yawn"
             (NoTarget  "You yawn."
                        "% yawns.")
             (dup allValues)
             True
             Nothing ]

expCmds :: HasCallStack => [Cmd]
expCmds = S.foldr helper [] expCmdSet
  where
    helper ec@(ExpCmd expCmdName _ _ _ _) = (Cmd { cmdName           = expCmdName
                                                 , cmdPriorityAbbrev = Nothing
                                                 , cmdFullName       = expCmdName
                                                 , cmdAction         = Action (expCmd ec) True
                                                 , cmdDesc           = "" } :)

expCmdNames :: HasCallStack => [Text]
expCmdNames = S.toList . S.map (\(ExpCmd n _ _ _ _) -> n) $ expCmdSet

getExpCmdByName :: HasCallStack => ExpCmdName -> ExpCmd
getExpCmdByName cn = head . S.toList . S.filter (\(ExpCmd cn' _ _ _ _) -> cn' == cn) $ expCmdSet

-----

expCmd :: HasCallStack => ExpCmd -> ActionFun
expCmd (ExpCmd ecn HasTarget {} _ _     _   ) p@NoArgs {}        = advise p [] . sorryExpCmdRequiresTarget $ ecn
expCmd (ExpCmd ecn ect          _ isVis desc) (NoArgs i mq cols) = getStateTime >>= \pair@(ms, _) -> case ect of
  (NoTarget  toSelf toOthers      ) | isPla i ms
                                    , r <- getRace i ms
                                    , r `elem` furRaces
                                    , ecn == "blush" -> wrapSend mq cols . sorryExpCmdBlush . pp $ r
                                    | otherwise      -> helper pair toSelf toOthers
  (Versatile toSelf toOthers _ _ _)                  -> helper pair toSelf toOthers
  _                                                  -> pmf "expCmd" ect
  where
    furRaces                  = [ Felinoid, Lagomorph, Vulpenoid ]
    helper (ms, ct) toSelf toOthers =
        let d                           = mkStdDesig i ms DoCap
            serialized                  = serializeDesigHelper d toOthers
            (heShe, hisHer, himHerself) = mkPros . getSex i $ ms
            substitutions               = [ ("%", serialized), ("^", heShe), ("&", hisHer), ("*", himHerself) ]
            toOthersBcast               = ( markForSpiritOnly isLit isVis . nlnl . replace substitutions $ toOthers
                                          , desigOtherIds d )
            isLit                       = isMobRmLit ct i ms
            tuple                       = (toSelf, pure toOthersBcast, desc, toSelf)
        in expCmdHelper i mq cols ecn tuple
expCmd (ExpCmd ecn NoTarget {} _ _     _   ) p@(WithArgs     _ _  _    (_:_) ) = advise p [] . sorryExpCmdIllegalTarget $ ecn
expCmd (ExpCmd ecn ect         _ isVis desc)   (OneArgNubbed i mq cols target) = case ect of
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
              (_,                    [Left [sorryMsg]]) -> wrapSend mq cols sorryMsg
              (_,                    Right _:_        ) -> wrapSend mq cols sorryExpCmdCoins
              ([Left sorryMsg   ], _                  ) -> wrapSend mq cols sorryMsg
              ([Right (_:_:_)   ], _                  ) -> wrapSend mq cols adviceExpCmdExcessArgs
              ([Right [targetId]], _                  ) ->
                let ioHelper targetDesigTxt =
                        let (toSelf', toOthers', logMsg, substitutions) = mkBindings targetDesigTxt
                            toTarget'     = replace substitutions toTarget
                            toTargetBcast = (nlnl toTarget', pure targetId)
                            toOthersBcast = (nlnl toOthers', targetId `delete` desigOtherIds d)
                            bs    = [ (markForSpiritOnly isLit isVis txt, is)
                                    | (txt, is) <- [ toTargetBcast, toOthersBcast ] ]
                            isLit = isMobRmLit ct i ms
                            tuple = (toSelf', bs, desc, logMsg)
                        in expCmdHelper i mq cols ecn tuple
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


expCmdHelper :: HasCallStack => ExpCmdFun
expCmdHelper i mq cols ecn (toSelf, bs, desc, logMsg) = do logPlaOut ecn i . pure $ logMsg
                                                           wrapSend mq cols toSelf
                                                           bcastIfNotIncog i bs
                                                           mobRmDescHelper i desc

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
    helper (ExpCmd ecn _ _ _ _) = ecn == name
