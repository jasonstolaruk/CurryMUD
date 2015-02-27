module Mud.Data.State.Util.Pla where

import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Util.Misc

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Lens (at)
import Control.Lens.Operators ((&), (?~), (^.))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.IntMap.Lazy ((!))
{-
import Control.Applicative ((<$>))
import Control.Concurrent.STM.TMVar (putTMVar)
import Control.Lens.Getter (view, views)
import Control.Lens.Setter (ASetter)
import Data.Maybe (isNothing)
-}


-- TODO: Can we get rid of, or entirely rewrite, this module?


modifyPlaFlag :: Id -> PlaFlags -> Bool -> MudStack ()
modifyPlaFlag i flag b = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = (md^.plaTblTVar) |$| readTVar >=> \pt ->
        let p = setPlaFlag flag b $ pt ! i
        in writeTVar (md^.plaTblTVar) (pt & at i ?~ p)


{-
getPla :: Id -> MudStack Pla
getPla i = (! i) <$> readTMVarInNWS plaTblTMVar


getPlaColumns :: Id -> MudStack Int
getPlaColumns i = view columns <$> getPla i


getPlaIsAdmin :: Id -> MudStack Bool
getPlaIsAdmin i = getPlaFlag IsAdmin <$> getPla i


getPlaPageLines :: Id -> MudStack Int
getPlaPageLines i = view pageLines <$> getPla i


getPlaIsDfltPrompt :: Id -> MudStack Bool
getPlaIsDfltPrompt i = views interp isNothing <$> getPla i


-----


putPla :: Id -> Pla -> MudStack ()
putPla i p = modifyNWS plaTblTMVar $ \pt ->
    pt & at i ?~ p


-----


modifyPla :: Id -> ASetter Pla Pla a b -> b -> MudStack Pla
modifyPla i lens val = onNWS plaTblTMVar $ \(ptTMVar, pt) ->
    let p = (pt ! i) & lens .~ val
    in putTMVar ptTMVar (pt & at i ?~ p) >> return p
-}
