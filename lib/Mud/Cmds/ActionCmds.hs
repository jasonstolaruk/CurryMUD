{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE NamedFieldPuns, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Cmds.ActionCmds ( actionCmdSet
                           , actionCmds ) where

import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.State
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.STM
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Quoting
import qualified Mud.Logging as L (logPlaOut)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow ((***))
import Control.Lens.Getter (view)
import Control.Lens.Operators ((^.))
import Data.IntMap.Lazy ((!))
import Data.List ((\\), delete)
import Data.Monoid (mempty)
import qualified Data.Set as S (Set, fromList, foldr)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.ActionCmds"


-----


logPlaOut :: T.Text -> Id -> [T.Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Cmds.ActionCmds"


-- ==================================================


actionCmdSet :: S.Set ActionCmd
actionCmdSet = S.fromList
    [ ActionCmd "admire"      (HasTarget "You admire @."
                                         "% admires you."
                                         "% admires @.")
    , ActionCmd "applaud"     (Versatile "You applaud."
                                         "% applauds."
                                         "You applaud @."
                                         "% applauds you."
                                         "% applauds @.")
    , ActionCmd "astonished"  (NoTarget  "You are astonished."
                                         "% is astonished.")
    , ActionCmd "avert"       (Versatile "You avert your eyes."
                                         "% averts # eyes."
                                         "You avert your eyes from @."
                                         "% averts # eyes from you."
                                         "% averts # eyes from @.")
    , ActionCmd "beam"        (Versatile "You beam."
                                         "% beams."
                                         "You beam at @."
                                         "% beams at you."
                                         "% beams at @.")
    , ActionCmd "belch"       (Versatile "You belch."
                                         "% belches."
                                         "You belch at @."
                                         "% belches at you."
                                         "% belches at @.")
    , ActionCmd "blank"       (Versatile "You have a blank expression on your face."
                                         "% has a blank expression on # face."
                                         "You look blankly at @."
                                         "% looks blankly at you."
                                         "% looks blankly at @.")
    , ActionCmd "bleed"       (NoTarget  "You bleed."
                                         "% bleeds.")
    , ActionCmd "blink"       (Versatile "You blink."
                                         "% blinks."
                                         "You blink at @."
                                         "% blinks at you."
                                         "% blinks at @.")
    , ActionCmd "blush"       (NoTarget  "You blush."
                                         "% blushes.")
    , ActionCmd "boggle"      (NoTarget  "You boggle at the concept."
                                         "% boggles at the concept.")
    , ActionCmd "bow"         (Versatile "You bow."
                                         "% bows."
                                         "You bow before @."
                                         "% bows before you."
                                         "% bows before @.")
    , ActionCmd "burp"        (Versatile "You burp."
                                         "% burps."
                                         "You burp at @."
                                         "% burps at you."
                                         "% burps at @.")
    , ActionCmd "cheer"       (Versatile "You cheer."
                                         "% cheers."
                                         "You cheer for @."
                                         "% cheers for you."
                                         "% cheers for @.")
    , ActionCmd "chuckle"     (Versatile "You chuckle."
                                         "% chuckles."
                                         "You chuckle at @."
                                         "% chuckles at you."
                                         "% chuckles at @.")
    , ActionCmd "clap"        (Versatile "You clap."
                                         "% claps."
                                         "You clap for @."
                                         "% claps for you."
                                         "% claps for @.")
    , ActionCmd "closeeyes"   (NoTarget  "You close your eyes."
                                         "% closes # eyes.")
    , ActionCmd "comfort"     (HasTarget "You comfort @."
                                         "% comforts you."
                                         "% comforts @.")
    , ActionCmd "confused"    (Versatile "You have a confused expression on your face."
                                         "% has a confused expression on # face."
                                         "You look at @ with a confused expression on your face."
                                         "% looks at you with a confused expression on # face."
                                         "% looks at @ with a confused expression on # face.")
    , ActionCmd "cough"       (Versatile "You cough."
                                         "% coughs."
                                         "You cough at @."
                                         "% coughs at you."
                                         "% coughs at @.")
    , ActionCmd "cower"       (Versatile "You cower in fear."
                                         "% cowers in fear."
                                         "You cower in fear before @."
                                         "% cowers in fear before you."
                                         "% cowers in fear before @.")
    , ActionCmd "cringe"      (Versatile "You cringe."
                                         "% cringes."
                                         "You cringe at @."
                                         "% cringes at you."
                                         "% cringes at @.")
    , ActionCmd "cry"         (NoTarget  "You cry."
                                         "% cries.")
    , ActionCmd "cryanger"    (Versatile "You cry out in anger."
                                         "% cries out in anger."
                                         "You cry out in anger at @."
                                         "% cries out in anger at you."
                                         "% cries out in anger at @.")
    , ActionCmd "cuddle"      (HasTarget "You cuddle @."
                                         "% cuddles you."
                                         "% cuddles @.")
    , ActionCmd "curtsey"     (Versatile "You curtsey."
                                         "% curtseys."
                                         "You curtsey to @."
                                         "% curtseys to you."
                                         "% curtseys to @.")
    , ActionCmd "curtsy"      (Versatile "You curtsy."
                                         "% curtsies."
                                         "You curtsy to @."
                                         "% curtsies to you."
                                         "% curtsies to @.")
    , ActionCmd "dance"       (Versatile "You dance around."
                                         "% dances around."
                                         "You dance with @."
                                         "% dances with you."
                                         "% dances with @.")
    , ActionCmd "daydream"    (NoTarget  "You daydream."
                                         "% daydreams.")
    , ActionCmd "deepbreath"  (NoTarget  "You take a deep breath."
                                         "% takes a deep breath.")
    , ActionCmd "drool"       (NoTarget  "You drool."
                                         "% drools.")
    , ActionCmd "facepalm"    (NoTarget  "You facepalm."
                                         "% facepalms.")
    , ActionCmd "faint"       (NoTarget  "You faint."
                                         "% faints.")
    , ActionCmd "flop"        (NoTarget  "You flop down on the ground."
                                         "% flops down on the ground.")
    , ActionCmd "frown"       (Versatile "You frown."
                                         "% frowns."
                                         "You frown at @."
                                         "% frowns at you."
                                         "% frowns at @.")
    , ActionCmd "funnyface"   (Versatile "You make a funny face."
                                         "% makes a funny face."
                                         "You make a funny face at @."
                                         "% makes a funny face at you."
                                         "% makes a funny face at @.")
    , ActionCmd "gag"         (NoTarget  "You gag."
                                         "% gags.")
    , ActionCmd "gasp"        (Versatile "You gasp."
                                         "% gasps."
                                         "You gasp at @."
                                         "% gasps at you."
                                         "% gasps at @.")
    , ActionCmd "giggle"      (Versatile "You giggle."
                                         "% giggles."
                                         "You giggle at @."
                                         "% giggles at you."
                                         "% giggles at @.")
    , ActionCmd "giggle"      (Versatile "You glance around."
                                         "% glances around."
                                         "You glance at @."
                                         "% glances at you."
                                         "% glances at @.")
    , ActionCmd "glare"       (HasTarget "You glare at @."
                                         "% glares at you."
                                         "% glares at @.")
    , ActionCmd "greet"       (HasTarget "You greet @."
                                         "% greets you."
                                         "% greets @.")
    , ActionCmd "grin"        (Versatile "You grin."
                                         "% grins."
                                         "You grin at @."
                                         "% grins at you."
                                         "% grins at @.")
    , ActionCmd "groan"       (Versatile "You groan."
                                         "% groans."
                                         "You groan at @."
                                         "% groans at you."
                                         "% groans at @.")
    , ActionCmd "grovel"      (HasTarget "You grovel before @."
                                         "% grovels before you."
                                         "% grovels before @.")
    , ActionCmd "growl"       (Versatile "You growl."
                                         "% growls."
                                         "You growl at @."
                                         "% growls at you."
                                         "% growls at @.")
    , ActionCmd "grumble"     (NoTarget  "You grumble to yourself."
                                         "% grumbles to $.")
    , ActionCmd "handhips"    (NoTarget  "You put your hands on your hips."
                                         "% puts # hands on # hips.")
    , ActionCmd "hesitate"    (NoTarget  "You hesitate."
                                         "% hesitates.")
    , ActionCmd "hiccup"      (NoTarget  "You hiccup."
                                         "% hiccups.")
    , ActionCmd "horf"        (Versatile "You horf all over the place."
                                         "% horfs all over the place."
                                         "You horf all over @."
                                         "% horfs all over you."
                                         "% horfs all over @.")
    , ActionCmd "jump"        (NoTarget  "You jump up and down."
                                         "% jumps up and down.")
    , ActionCmd "hop"         (NoTarget  "You hop up and down."
                                         "% hops up and down.")
    , ActionCmd "hug"         (HasTarget "You hug @."
                                         "% hugs you."
                                         "% hugs @.")
    , ActionCmd "hum"         (NoTarget  "You hum a merry tune."
                                         "% hums a merry tune.")
    , ActionCmd "kiss"        (HasTarget "You kiss @."
                                         "% kisses you."
                                         "% kisses @.")
    , ActionCmd "kneel"       (Versatile "You kneel down."
                                         "% kneels down."
                                         "You kneel down before @."
                                         "% kneels down before you."
                                         "% kneels down before @.")
    , ActionCmd "laugh"       (Versatile "You laugh."
                                         "% laughs."
                                         "You laugh at @."
                                         "% laughs at you."
                                         "% laughs at @.")
    , ActionCmd "leap"        (NoTarget  "You leap into the air."
                                         "% leaps into the air.")
    , ActionCmd "leer"        (HasTarget "You leer at @."
                                         "% leers at you."
                                         "% leers at @.")
    , ActionCmd "licklips"    (NoTarget  "You lick your lips."
                                         "% licks # lips.")
    , ActionCmd "massage"     (HasTarget "You massage @."
                                         "% massages you."
                                         "% massages @.")
    , ActionCmd "moan"        (NoTarget  "You moan."
                                         "% moans.")
    , ActionCmd "mutter"      (NoTarget  "You mutter to yourself."
                                         "% mutters to $.")
    , ActionCmd "nod"         (Versatile "You nod."
                                         "% nods."
                                         "You nod to @."
                                         "% nods to you."
                                         "% nods to @.")
    , ActionCmd "nudge"       (HasTarget "You nudge @."
                                         "% nudges you."
                                         "% nudges @.")
    , ActionCmd "nuzzle"      (HasTarget "You nuzzle @."
                                         "% nuzzles you."
                                         "% nuzzles @.")
    , ActionCmd "openeyes"    (NoTarget  "You open your eyes."
                                         "% opens # eyes.")
    , ActionCmd "pace"        (NoTarget  "You pace around."
                                         "% paces around.")
    , ActionCmd "pant"        (NoTarget  "You pant."
                                         "% pants.")
    , ActionCmd "pat"         (HasTarget "You pat @ on the back."
                                         "% pats you on the back."
                                         "% pats @ on the back.")
    , ActionCmd "peer"        (HasTarget "You peer at @."
                                         "% peers at you."
                                         "% peers at @.")
    , ActionCmd "picknose"    (NoTarget  "You pick your nose."
                                         "% picks # nose.")
    , ActionCmd "pinch"       (HasTarget "You pinch @."
                                         "% pinches you."
                                         "% pinches @.")
    , ActionCmd "point"       (HasTarget "You point to @."
                                         "% points to you."
                                         "% points to @.")
    , ActionCmd "poke"        (HasTarget "You poke @."
                                         "% pokes you."
                                         "% pokes @.")
    , ActionCmd "ponder"      (NoTarget  "You ponder the situation."
                                         "% ponders the situation.")
    , ActionCmd "pose"        (NoTarget  "You strike a pose."
                                         "% strike a pose.")
    , ActionCmd "pounce"      (HasTarget "You pounce on @."
                                         "% pounces on you."
                                         "% pounces on @.")
    , ActionCmd "pout"        (Versatile "You pout."
                                         "% pouts."
                                         "You pout at @."
                                         "% pouts at you."
                                         "% pout at @.")
    , ActionCmd "puke"        (Versatile "You puke all over."
                                         "% pukes all over."
                                         "You puke on @."
                                         "% pukes on you."
                                         "% pukes on @.")
    , ActionCmd "raisebrow"   (Versatile "You raise an eyebrow."
                                         "% raises an eyebrow."
                                         "You raise an eyebrow at @."
                                         "% raises an eyebrow at you."
                                         "% raises an eyebrow at @.")
    , ActionCmd "raisehand"   (NoTarget  "You raise your hand."
                                         "% raises # hand.")
    , ActionCmd "rock"        (NoTarget  "You rock back and forth."
                                         "% rocks back and forth.")
    , ActionCmd "rolleyes"    (Versatile "You roll your eyes."
                                         "% rolls # eyes."
                                         "You roll your eyes at @."
                                         "% rolls # eyes at you."
                                         "% rolls # eyes at @.")
    , ActionCmd "rubeyes"     (NoTarget  "You rub your eyes."
                                         "% rubs # eyes.")
    , ActionCmd "satisfied"   (NoTarget  "You look satisfied."
                                         "% looks satisfied.")
    , ActionCmd "sleepy"      (NoTarget  "You look sleepy."
                                         "% looks sleepy.")
    , ActionCmd "smirk"       (Versatile "You smirk."
                                         "% smirks."
                                         "You smirk at @."
                                         "% smirks at you."
                                         "% smirks at @.")
    , ActionCmd "tears"       (NoTarget  "Tears roll down your face."
                                         "Tears roll down %'s face.")
    , ActionCmd "thumbsdown"  (Versatile "You give a thumbs down."
                                         "% gives a thumbs down."
                                         "You give a thumbs down to @."
                                         "% gives a thumbs down to you."
                                         "% gives a thumbs down to @.")
    , ActionCmd "thumbsup"    (Versatile "You give a thumbs up."
                                         "% gives a thumbs up."
                                         "You give a thumbs up to @."
                                         "% gives a thumbs up to you."
                                         "% gives a thumbs up to @.")
    , ActionCmd "tongue"      (Versatile "You stick out your tongue."
                                         "% sticks out # tongue."
                                         "You stick out your tongue at @."
                                         "% sticks out # tongue at you."
                                         "% sticks out # tongue at @.")
    , ActionCmd "unamused"    (Versatile "You are clearly unamused."
                                         "% is clearly unamused."
                                         "You are clearly unamused by @'s antics."
                                         "% is clearly unamused by your antics."
                                         "% is clearly unamused by @'s antics.")
    , ActionCmd "vomit"       (Versatile "You vomit."
                                         "% vomits."
                                         "You vomit on @."
                                         "% vomits on you."
                                         "% vomits on @.")
    , ActionCmd "watch"       (HasTarget "You watch @ with interest."
                                         "% watches you with interest."
                                         "% watches @ with interest.")
    , ActionCmd "wink"        (Versatile "You wink."
                                         "% winks."
                                         "You wink at @."
                                         "% winks at you."
                                         "% winks at @.") ]


actionCmds :: [Cmd]
actionCmds = S.foldr helper [] actionCmdSet
  where
    helper (ActionCmd actionCmdName actionCmdType) = (Cmd { cmdName = actionCmdName
                                                          , action  = actionCmd actionCmdType
                                                          , cmdDesc = "" } :)


-----


-- TODO: More refactoring for code reuse?
actionCmd :: ActionCmdType -> Action
actionCmd (HasTarget {}) (NoArgs   _ mq cols) = wrapSend mq cols "This action command requires a single target."
actionCmd act            (NoArgs'' i        ) = case act of
  (NoTarget  toSelf toOthers      ) -> helper toSelf toOthers
  (Versatile toSelf toOthers _ _ _) -> helper toSelf toOthers
  x                                 -> patternMatchFail "actionCmd" [ showText x ]
  where
    helper toSelf toOthers = readWSTMVar >>= \ws ->
        let (d, _, _, _, _)                     = mkCapStdDesig i ws
            toSelfBrdcst                        = (nlnl toSelf, [i])
            serialized | T.head toOthers == '%' = serialize d
                       | otherwise              = serialize d { isCap = False }
            (hisHer, hisHerself)                = mkPronouns i ws
            toOthers'                           = T.replace "%" serialized . T.replace "#" hisHer . T.replace "$" hisHerself $ toOthers
            toOthersBrdcst                      = (nlnl toOthers', i `delete` pcIds d)
        in logPlaOut (bracketQuote "action command") i [toSelf] >> bcast (toSelfBrdcst : [toOthersBrdcst])
actionCmd (NoTarget {}) (WithArgs _ mq cols (_:_) ) = wrapSend mq cols "This action command may not be used with a \
                                                                       \target."
actionCmd act           (OneArg   i mq cols target) = case act of
  (HasTarget     toSelf toTarget toOthers) -> helper toSelf toTarget toOthers
  (Versatile _ _ toSelf toTarget toOthers) -> helper toSelf toTarget toOthers
  x                                        -> patternMatchFail "actionCmd" [ showText x ]
  where
    helper toSelf toTarget toOthers = readWSTMVar >>= \ws ->
        let (d, _, _, ri, ris@((i `delete`) -> ris')) = mkCapStdDesig i ws
            c                                         = (ws^.coinsTbl) ! ri
        in if (not . null $ ris') || (c /= mempty)
          then case resolveRmInvCoins i ws [target] ris' c of
            (_,                    [ Left  [sorryMsg] ]) -> wrapSend mq cols sorryMsg
            (_,                    Right _:_           ) -> wrapSend mq cols "Sorry, but action commands cannot be \
                                                                             \used with coins."
            ([ Left sorryMsg    ], _                   ) -> wrapSend mq cols sorryMsg
            ([ Right (_:_:_)    ], _                   ) -> wrapSend mq cols "Sorry, but you can only target one \
                                                                             \person at a time with action commands."
            ([ Right [targetId] ], _                   ) ->
              let (view sing -> targetSing) = (ws^.entTbl) ! targetId
                  actionOnPC targetDesig    =
                      let toSelf'        = T.replace "@" targetDesig toSelf
                          toSelfBrdcst   = (nlnl toSelf', [i])
                          serialized     = mkSerializedDesig d
                          (hisHer, _)    = mkPronouns i ws
                          toTarget'      = T.replace "%" serialized . T.replace "#" hisHer $ toTarget
                          toTargetBrdcst = (nlnl toTarget', [targetId])
                          toOthers'      = T.replace "@" targetDesig . T.replace "%" serialized . T.replace "#" hisHer $ toOthers
                          toOthersBrdcst = (nlnl toOthers', pcIds d \\ [ i, targetId ])
                      in do
                          logPlaOut (bracketQuote "action command") i [ parsePCDesig i ws toSelf' ]
                          bcast $ toSelfBrdcst : toTargetBrdcst : [toOthersBrdcst]
                  actionOnMob targetNoun    =
                      let toSelf'        = T.replace "@" targetNoun toSelf
                          toSelfBrdcst   = (nlnl toSelf', [i])
                          serialized     = mkSerializedDesig d
                          (hisHer, _)    = mkPronouns i ws
                          toOthers'      = T.replace "@" targetNoun . T.replace "%" serialized . T.replace "#" hisHer $ toOthers
                          toOthersBrdcst = (nlnl toOthers', i `delete` pcIds d)
                      in do
                          logPlaOut (bracketQuote "action command") i [toSelf']
                          bcast $ toSelfBrdcst : [toOthersBrdcst]
              in case (ws^.typeTbl) ! targetId of
                PCType  -> actionOnPC  . serialize . mkStdDesig targetId ws targetSing False $ ris
                MobType -> actionOnMob . theOnLower $ targetSing
                _       -> wrapSend mq cols "Sorry, but action commands may only target people."
            x -> patternMatchFail "actionCmd helper" [ showText x ]
          else wrapSend mq cols "You don't see anyone here."
        where
          mkSerializedDesig d | T.head toOthers == '%' = serialize d
                              | otherwise              = serialize d { isCap = False }
actionCmd act (ActionParams { plaMsgQueue, plaCols }) = wrapSend plaMsgQueue plaCols $ case act of
  (HasTarget {}) -> "This action command requires a single target."
  (Versatile {}) -> "This action command may be used with at most one target."
  x              -> patternMatchFail "actionCmd" [ showText x ]


mkPronouns :: Id -> WorldState -> (T.Text, T.Text)
mkPronouns i ws = let (view sex -> s) = (ws^.mobTbl) ! i in (mkPossPronoun *** mkReflexive) . dup $ s
