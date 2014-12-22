{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.Login (interpName) where

import Mud.Cmds.Pla
import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util
import Mud.Interp.CentralDispatch
import Mud.Logging hiding (logPla)
import Mud.TheWorld.Ids
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Logging as L (logPla)
import qualified Mud.Util as U (patternMatchFail)

import Control.Concurrent.STM.TMVar (putTMVar)
import Control.Lens (at)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (?~), (.~), (^.))
import Control.Monad (void)
import Data.IntMap.Lazy ((!))
import Data.List (delete)
import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Interp.Login"


-----


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Interp.Login"


-- ==================================================


-- TODO: Boot a player who tries too many times?
interpName :: Interp
interpName (capitalize . T.toLower -> cn) (NoArgs' i mq)
  | l <- T.length cn, l < 3 || l > 12 = promptRetryName mq "Your name must be between three and twelve characters long."
  | T.any (`elem` illegalChars) cn    = promptRetryName mq "Your name cannot include any numbers or symbols."
  | otherwise                         = do
      prompt mq . nl' $ "Your name will be " <> dblQuote cn <> ", is that OK? [yes/no]"
      void . modifyPla i interp $ interpConfirmName cn
  where
    illegalChars = [ '!' .. '@' ] ++ [ '[' .. '`' ] ++ [ '{' .. '~' ]
interpName _  (WithArgs _ mq _ _) = promptRetryName mq "Your name must be a single word."
interpName cn p                   = patternMatchFail "interpName" [ cn, showText p ]


promptRetryName :: MsgQueue -> T.Text -> MudStack ()
promptRetryName mq msg = do
    send mq . nl' $ if (not . T.null $ msg) then nl msg else ""
    prompt mq "Let's try this again. By what name are you known?"


interpConfirmName :: Sing -> Interp
interpConfirmName s cn (NoArgs i mq cols) = case yesNo cn of
  Just True -> do
      void . modifyEnt i sing $ s
      (views hostName T.pack -> host) <- modifyPla i interp centralDispatch
      initPlaLog i s
      logPla "interpConfirmName" i $ "(new player) logged on from " <> host <> "."
      movePC
      notifyArrival i
      send mq . nl $ ""
      showMotd mq cols
      look ActionParams { plaId       = i
                        , plaMsgQueue = mq
                        , plaCols     = cols
                        , args        = [] }
      prompt mq ">"
  Just False -> promptRetryName mq "" >> (void . modifyPla i interp $ interpName)
  Nothing    -> promptRetryYesNo mq
  where
    movePC = onWS $ \(t, ws) ->
        let p         = (ws^.pcTbl)  ! i
            p'        = p & rmId .~ iHill
            originIs  = (ws^.invTbl) ! iWelcome
            originIs' = i `delete` originIs
            destIs    = (ws^.invTbl) ! iHill
            destIs'   = sortInv ws $ destIs ++ [i]
        in putTMVar t (ws & pcTbl.at  i        ?~ p'
                          & invTbl.at iWelcome ?~ originIs'
                          & invTbl.at iHill    ?~ destIs')
interpConfirmName _ _  (WithArgs _ mq _ _) = promptRetryYesNo mq
interpConfirmName s cn p                   = patternMatchFail "interpConfirmName" [ s, cn, showText p ]


yesNo :: T.Text -> Maybe Bool
yesNo (T.toLower -> a) | a `T.isPrefixOf` "yes" = Just True
                       | a `T.isPrefixOf` "no"  = Just False
                       | otherwise              = Nothing


notifyArrival :: Id -> MudStack ()
notifyArrival i = readWSTMVar >>= \ws ->
    let (view sing -> s) = (ws^.entTbl) ! i
    in bcastOthersInRm i . nlnl $ mkSerializedNonStdDesig i ws s A <> " has arrived in the game."


promptRetryYesNo :: MsgQueue -> MudStack ()
promptRetryYesNo mq = prompt mq . T.concat $ [ "Please answer ", dblQuote "yes", " or ", dblQuote "no", "." ]
