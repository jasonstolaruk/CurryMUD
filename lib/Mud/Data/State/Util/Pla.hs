{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module Mud.Data.State.Util.Pla where

import Mud.Data.State.State
import Mud.Data.State.Util.STM

import Control.Applicative ((<$>))
import Control.Concurrent.STM.TMVar (putTMVar)
import Control.Lens (at)
import Control.Lens.Getter (view)
import Control.Lens.Operators ((&), (?~), (.~))
import Control.Lens.Setter (ASetter)
import Data.IntMap.Lazy ((!))
import Data.Maybe (isNothing)


getPla :: Id -> MudStack Pla
getPla i = (! i) <$> readTMVarInNWS plaTblTMVar


getPlaColumns :: Id -> MudStack Int
getPlaColumns i = view columns <$> getPla i


getPlaIsWiz :: Id -> MudStack Bool
getPlaIsWiz i = view isWiz <$> getPla i


getPlaPageLines :: Id -> MudStack Int
getPlaPageLines i = view pageLines <$> getPla i


isDfltPrompt :: Id -> MudStack Bool
isDfltPrompt i = isNothing . view interp <$> getPla i


-----


putPla :: Id -> Pla -> MudStack ()
putPla i p = modifyNWS plaTblTMVar $ \pt ->
    pt & at i ?~ p


-----


modifyPla :: Id -> ASetter Pla Pla a b -> b -> MudStack Pla
modifyPla i lens val = onNWS plaTblTMVar $ \(ptTMVar, pt) ->
    let p  = pt ! i
        p' = p & lens .~ val
    in putTMVar ptTMVar (pt & at i ?~ p') >> return p'
