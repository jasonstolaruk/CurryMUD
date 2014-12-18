{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.Login (interpName) where

import Mud.Cmds.Pla
import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util
import Mud.Interp.CentralDispatch
import Mud.Logging hiding (logPla)
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Logging as L (logPla)
import qualified Mud.Util as U (patternMatchFail)

import Control.Lens.Getter (view)
import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Data.IntMap.Lazy ((!))
import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Interp.Login"


-----


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Interp.Login"


-- ==================================================


-- TODO: Boot a player who tries too many times.
interpName :: Interp
interpName (capitalize . T.toLower -> cn) (NoArgs' i mq)
  | l <- T.length cn, l < 3 || l > 12 = sorryIllegalName mq "Your name must be between three and twelve characters \
                                                            \long."
  | T.any (`elem` illegalChars) cn    = sorryIllegalName mq "Your name cannot include any numbers or symbols."
  | otherwise                         = do
      prompt mq . nl' $ "Your name will be " <> dblQuote cn <> ", is that OK? [yes/no]"
      void . modifyPla i interp $ interpConfirmName cn
  where
    illegalChars    = [ '!' .. '@' ] ++ [ '[' .. '`' ] ++ [ '{' .. '~' ]
interpName _  (WithArgs _ mq _ _) = sorryIllegalName mq "Your name must be a single word."
interpName cn p                   = patternMatchFail "interpName" [ cn, showText p ]


sorryIllegalName :: MsgQueue -> T.Text -> MudStack ()
sorryIllegalName mq msg = do
    send mq . nl' . nl $ msg
    prompt mq "Let's try this again. By what name are you known?"


interpConfirmName :: Sing -> Interp
interpConfirmName s _ (NoArgs' i mq) = do
    void . modifyEnt i sing $ s
    initPlaLog i s
    (T.pack . view hostName -> host) <- modifyPla i interp centralDispatch
    logPla "interpConfirmName" i $ "(new player) logged on from " <> host <> "."
    notifyArrival i
    prompt mq . nl' $ ">"
interpConfirmName s cn p = patternMatchFail "interpConfirmName" [ s, cn, showText p ]


notifyArrival :: Id -> MudStack ()
notifyArrival i = readWSTMVar >>= \ws ->
    let (view sing -> s) = (ws^.entTbl) ! i
    in bcastOthersInRm i . nlnl $ mkSerializedNonStdDesig i ws s A <> " has arrived in the game."
