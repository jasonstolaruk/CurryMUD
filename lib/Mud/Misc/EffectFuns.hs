{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.EffectFuns ( effectFuns
                           , instaEffectFuns
                           , tinnitusEffectFunName
                           , tinnitusInstaEffectFunName ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Random
import Mud.Util.Misc


effectFuns :: [(FunName, EffectFun)]
effectFuns = pure (tinnitusEffectFunName, tinnitusEffectFun)


instaEffectFuns :: [(FunName, InstaEffectFun)]
instaEffectFuns = pure (tinnitusInstaEffectFunName, tinnitusInstaEffectFun)


-----


tinnitusEffectFunName :: FunName
tinnitusEffectFunName = "Effect_tinnitus"


tinnitusEffectFun :: EffectFun
tinnitusEffectFun i secs
  | isZero $ secs `mod` 5 = rndmDo 25 $ getMsgQueueColumns i <$> getState >>= \(mq, cols) ->
      wrapSend mq cols "You hear an awful ringing in your ears."
  | otherwise = unit


tinnitusInstaEffectFunName :: FunName
tinnitusInstaEffectFunName = "InstaEffect_tinnitus"


tinnitusInstaEffectFun :: InstaEffectFun
tinnitusInstaEffectFun i = getMsgQueueColumns i <$> getState >>= \(mq, cols) ->
    wrapSend mq cols "You hear a terrible ringing in your ears."
