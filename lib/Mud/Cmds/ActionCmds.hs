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
    [ ActionCmd "admire" (HasTarget "You admire @."
                                    "% admires you."
                                    "% admires @.")
    , ActionCmd "blink"  (Versatile "You blink."
                                    "% blinks."
                                    "You blink at @."
                                    "% blinks at you."
                                    "% blinks at @.")
    , ActionCmd "jump"   (NoTarget  "You jump up and down."
                                    "% jumps up and down.") ]


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
            toOthers'       = T.replace "%" (serialize d) toOthers
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
                          toTarget'      = T.replace "%" (serialize d) toTarget
                          toTargetBrdcst = (nlnl toTarget', [targetId])
                          toOthers'      = T.replace "@" targetDesig . T.replace "%" (serialize d) $ toOthers
                          toOthersBrdcst = (nlnl toOthers', pcIds d \\ [ i, targetId ])
                      in do
                          logPlaOut (bracketQuote "action command") i [ parsePCDesig i ws toSelf' ]
                          bcast $ toSelfBrdcst : toTargetBrdcst : [toOthersBrdcst]
                  actionOnMob targetNoun    =
                      let toSelf'        = T.replace "@" targetNoun toSelf
                          toSelfBrdcst   = (nlnl toSelf', [i])
                          toOthers'      = T.replace "@" targetNoun . T.replace "%" (serialize d) $ toOthers
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
actionCmd act (ActionParams { plaMsgQueue, plaCols }) = wrapSend plaMsgQueue plaCols $ case act of
  (HasTarget {}) -> "This action command requires a single target."
  (Versatile {}) -> "This action command may be used with at most one target."
  x              -> patternMatchFail "actionCmd" [ showText x ]
