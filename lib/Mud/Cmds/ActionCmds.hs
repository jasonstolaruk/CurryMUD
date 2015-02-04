{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-} -- TODO: Confirm.

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
import Data.Monoid ((<>), mempty)
import qualified Data.Set as S (Set, fromList, foldr)
import qualified Data.Text as T

{-
import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Misc
import Mud.Data.State.ActionParams.Util
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Pla
import Mud.NameResolution
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Padding
import Mud.Util.Wrapping
-}


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.ActionCmds"


-----


logPlaOut :: T.Text -> Id -> [T.Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Cmds.ActionCmds"


-- ==================================================


actionCmdSet :: S.Set ActionCmd
actionCmdSet = S.fromList [ ActionCmd { actionCmdName   = "admire"
                                      , actionCmdAction = actionCmdAdmire
                                      , actionCmdType   = HasTarget "You admire Hanako." }
                          , ActionCmd { actionCmdName   = "blink"
                                      , actionCmdAction = actionCmdBlink
                                      , actionCmdType   = Versatile "You blink." "You blink at Hanako." } ]


actionCmds :: [Cmd]
actionCmds = S.foldr helper [] actionCmdSet
  where
    helper ac = (Cmd { cmdName = actionCmdName ac, action = actionCmdAction ac, cmdDesc = "" } :)


-----


actionCmdAdmire :: Action
actionCmdAdmire _ = return ()


-----


actionCmdBlink :: Action
actionCmdBlink (NoArgs'' i) = readWSTMVar >>= \ws ->
    let (d, _, _, _, _) = mkCapStdDesig i ws
        toSelfMsg       = "You blink."
        toSelfBrdcst    = (nlnl toSelfMsg, [i])
        toOthersMsg     = serialize d <> " blinks."
        toOthersBrdcst  = (nlnl toOthersMsg, i `delete` pcIds d)
    in logPlaOut (bracketQuote "action command") i [toSelfMsg] >> bcast (toSelfBrdcst : [toOthersBrdcst])
actionCmdBlink (OneArg i mq cols target) = readWSTMVar >>= \ws ->
    let (d, _, _, ri, ris@((i `delete`) -> ris')) = mkCapStdDesig i ws
        c                                         = (ws^.coinsTbl) ! ri
    in if (not . null $ ris') || (c /= mempty)
      then case resolveRmInvCoins i ws [target] ris' c of
        (_,                    [ Left  [sorryMsg] ]) -> wrapSend mq cols sorryMsg
        (_,                    Right _:_           ) -> wrapSend mq cols "Sorry, but action commands cannot be used \
                                                                         \with coins."
        ([ Left sorryMsg    ], _                   ) -> wrapSend mq cols sorryMsg
        ([ Right (_:_:_)    ], _                   ) -> wrapSend mq cols "Sorry, but you can only target one person at \
                                                                         \a time with action commands."
        ([ Right [targetId] ], _                   ) ->
          let (view sing -> targetSing) = (ws^.entTbl) ! targetId
              actionOnPC targetDesig    =
                  let toSelfMsg      = "You blink at " <> targetDesig <> "."
                      toSelfBrdcst   = (nlnl toSelfMsg, [i])
                      toTargetMsg    = serialize d <> " blinks at you."
                      toTargetBrdcst = (nlnl toTargetMsg, [targetId])
                      toOthersMsg    = T.concat [ serialize d, " blinks at ", targetDesig, "." ]
                      toOthersBrdcst = (nlnl toOthersMsg, pcIds d \\ [ i, targetId ])
                  in do
                      logPlaOut (bracketQuote "action command") i [ parsePCDesig i ws toSelfMsg ]
                      bcast $ toSelfBrdcst : toTargetBrdcst : [toOthersBrdcst]
              actionOnMob targetNoun    =
                  let toSelfMsg      = "You blink at " <> targetNoun <> "."
                      toSelfBrdcst   = (nlnl toSelfMsg, [i])
                      toOthersMsg    = T.concat [ serialize d, " blinks at ", targetNoun, "." ]
                      toOthersBrdcst = (nlnl toOthersMsg, i `delete` pcIds d)
                  in do
                      logPlaOut (bracketQuote "action command") i [toSelfMsg]
                      bcast $ toSelfBrdcst : [toOthersBrdcst]
          in case (ws^.typeTbl) ! targetId of
            PCType  -> actionOnPC  . serialize . mkStdDesig targetId ws targetSing False $ ris
            MobType -> actionOnMob . theOnLower $ targetSing
            _       -> wrapSend mq cols "Sorry, but action commands may only target people."
        x -> patternMatchFail "TODO" [ showText x ]
      else wrapSend mq cols "You don't see anyone here."
actionCmdBlink _ = return () -- TODO
