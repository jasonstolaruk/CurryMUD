{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.EffectFuns ( effectFuns
                           , instaEffectFuns ) where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import           Mud.Data.State.Util.Random
import qualified Mud.Misc.Logging as L (logPla)
import           Mud.TheWorld.Liqs
import           Mud.Util.Misc

import           Control.Monad (when)
import           Data.List (delete)
import           Data.Monoid ((<>))
import           Data.Text (Text)


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Misc.EffectFuns"


-- ==================================================


-- Effect functions are run on a new thread every second.
effectFuns :: [(FunName, EffectFun)]
effectFuns = [ (oilTag,         oilEffectFun     )
             , (potTinnitusTag, tinnitusEffectFun) ]


-- Instantaneous effect functions are run once.
instaEffectFuns :: [(FunName, InstaEffectFun)]
instaEffectFuns = pure (potTinnitusTag, tinnitusInstaEffectFun)


-----


oilEffectFun :: EffectFun
oilEffectFun i secs = let f = getState >>= \ms -> when (isLoggedIn . getPla i $ ms) . rndmDo_ 25 . helper $ ms
                      in when (isZero $ secs `mod` 5) f
  where
    helper ms = let (mq, cols) = getMsgQueueColumns i ms
                    d          = mkStdDesig i ms DoCap
                    bs         = pure (serialize d <> "'s stomach rumbles loudly.", i `delete` desigIds d)
                in do logPla "oilEffectFun" i "stomach rumbling due to consumption of lamp oil."
                      wrapSend mq cols "Your stomach rumbles loudly."
                      bcastIfNotIncogNl i bs


tinnitusEffectFun :: EffectFun -- Potion of instant tinnitus.
tinnitusEffectFun i secs = when (isZero $ secs `mod` 5) $ getState >>= \ms ->
    let (mq, cols) = getMsgQueueColumns i ms
    in when (isLoggedIn . getPla i $ ms) . rndmDo_ 25 . wrapSend mq cols $ "There is an awful ringing in your ears."


tinnitusInstaEffectFun :: InstaEffectFun -- Potion of tinnitus.
tinnitusInstaEffectFun i = getMsgQueueColumns i <$> getState >>= \(mq, cols) ->
    wrapSend mq cols "There is a terrible ringing in your ears."
