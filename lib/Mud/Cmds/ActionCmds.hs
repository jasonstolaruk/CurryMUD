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
    , ActionCmd "bite"        (HasTarget "You bite @!"
                                         "% bites you!"
                                         "% bites @!")
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
    , ActionCmd "bounce"      (NoTarget  "You bounce up and down."
                                         "% bounces up and down.")
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
    , ActionCmd "comfort"     (HasTarget "You comfort @."
                                         "% comforts you."
                                         "% comforts @.")
    , ActionCmd "cough"       (Versatile "You cough."
                                         "% coughs."
                                         "You cough at @."
                                         "% coughs at you."
                                         "% coughs at @.")
    , ActionCmd "cower"       (Versatile "You cower in fear."
                                         "% cowers in fear."
                                         "You cower before @ in fear."
                                         "% cowers before you in fear."
                                         "% cowers before @ in fear.")
    , ActionCmd "cringe"      (Versatile "You cringe."
                                         "% cringes."
                                         "You cringe at @."
                                         "% cringes at you."
                                         "% cringes at @.")
    , ActionCmd "cry"         (NoTarget  "You cry."
                                         "% cries.")
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
    , ActionCmd "hesitate"    (NoTarget  "You hesitate."
                                         "% hesitates.")
    , ActionCmd "jump"        (NoTarget  "You jump up and down."
                                         "% jumps up and down.")
    , ActionCmd "tears"       (NoTarget  "Tears roll down your face."
                                         "Tears roll down %'s face.") ]


actionCmds :: [Cmd]
actionCmds = S.foldr helper [] actionCmdSet
  where
    helper (ActionCmd actionCmdName actionCmdType) = (Cmd { cmdName = actionCmdName
                                                          , action  = actionCmd actionCmdType
                                                          , cmdDesc = "" } :)


-----


actionCmd :: ActionCmdType -> Action
actionCmd (HasTarget {}) (NoArgs   _ mq cols) = wrapSend mq cols "This action command requires a single target."
actionCmd act            (NoArgs'' i        ) = case act of
  (NoTarget  toSelf toOthers      ) -> helper toSelf toOthers
  (Versatile toSelf toOthers _ _ _) -> helper toSelf toOthers
  x                                 -> patternMatchFail "actionCmd" [ showText x ]
  where
    helper toSelf toOthers = readWSTMVar >>= \ws ->
        let (d, _, _, _, _) = mkCapStdDesig i ws
            toSelfBrdcst    = (nlnl toSelf, [i])
            serialized | T.head toOthers == '%' = serialize d
                       | otherwise              = serialize d { isCap = False }
            toOthers'       = T.replace "%" serialized toOthers
            toOthersBrdcst  = (nlnl toOthers', i `delete` pcIds d)
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
                          toTarget'      = T.replace "%" serialized toTarget
                          toTargetBrdcst = (nlnl toTarget', [targetId])
                          toOthers'      = T.replace "@" targetDesig . T.replace "%" serialized $ toOthers
                          toOthersBrdcst = (nlnl toOthers', pcIds d \\ [ i, targetId ])
                      in do
                          logPlaOut (bracketQuote "action command") i [ parsePCDesig i ws toSelf' ]
                          bcast $ toSelfBrdcst : toTargetBrdcst : [toOthersBrdcst]
                  actionOnMob targetNoun    =
                      let toSelf'        = T.replace "@" targetNoun toSelf
                          toSelfBrdcst   = (nlnl toSelf', [i])
                          serialized     = mkSerializedDesig d
                          toOthers'      = T.replace "@" targetNoun . T.replace "%" serialized $ toOthers
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
