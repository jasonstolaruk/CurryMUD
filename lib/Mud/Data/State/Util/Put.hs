module Mud.Data.State.Util.Put where

import Mud.Data.State.MudData

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Lens (at)
import Control.Lens.Operators ((.~), (?~), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)


putArm :: Id -> Ent -> Obj -> Arm -> MudStack ()
putArm i e o a = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar) $ at i ?~ ArmType
        modifyTVar (md^.entTblTVar)  $ at i ?~ e
        modifyTVar (md^.objTblTVar)  $ at i ?~ o
        modifyTVar (md^.armTblTVar)  $ at i ?~ a


-----


putCloth :: Id -> Ent -> Obj -> Cloth -> MudStack ()
putCloth i e o c = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar)  $ at i ?~ ClothType
        modifyTVar (md^.entTblTVar)   $ at i ?~ e
        modifyTVar (md^.objTblTVar)   $ at i ?~ o
        modifyTVar (md^.clothTblTVar) $ at i ?~ c


-----


putCon :: Id -> Ent -> Obj -> Inv -> Coins -> Maybe Cloth -> Con -> MudStack ()
putCon i e o is coi mc con = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar)  $ at i ?~ ConType
        modifyTVar (md^.entTblTVar)   $ at i ?~ e
        modifyTVar (md^.objTblTVar)   $ at i ?~ o
        modifyTVar (md^.invTblTVar)   $ at i ?~ is
        modifyTVar (md^.coinsTblTVar) $ at i ?~ coi
        modifyTVar (md^.clothTblTVar) $ at i .~ mc
        modifyTVar (md^.conTblTVar)   $ at i ?~ con


-----


putMob :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> MudStack ()
putMob i e is c em m = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar)  $ at i ?~ MobType
        modifyTVar (md^.entTblTVar)   $ at i ?~ e
        modifyTVar (md^.invTblTVar)   $ at i ?~ is
        modifyTVar (md^.coinsTblTVar) $ at i ?~ c
        modifyTVar (md^.eqTblTVar)    $ at i ?~ em
        modifyTVar (md^.mobTblTVar)   $ at i ?~ m


-----


putObj :: Id -> Ent -> Obj -> MudStack ()
putObj i e o = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar) $ at i ?~ ObjType
        modifyTVar (md^.entTblTVar)  $ at i ?~ e
        modifyTVar (md^.objTblTVar)  $ at i ?~ o


-----


putPC :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> PC -> MudStack ()
putPC i e is c em m p = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar)  $ at i ?~ PCType
        modifyTVar (md^.entTblTVar)   $ at i ?~ e
        modifyTVar (md^.invTblTVar)   $ at i ?~ is
        modifyTVar (md^.coinsTblTVar) $ at i ?~ c
        modifyTVar (md^.eqTblTVar)    $ at i ?~ em
        modifyTVar (md^.mobTblTVar)   $ at i ?~ m
        modifyTVar (md^.pcTblTVar)    $ at i ?~ p


-----


putRm :: Id -> Inv -> Coins -> Rm -> MudStack ()
putRm i is c r = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar)  $ at i ?~ RmType
        modifyTVar (md^.invTblTVar)   $ at i ?~ is
        modifyTVar (md^.coinsTblTVar) $ at i ?~ c
        modifyTVar (md^.rmTblTVar)    $ at i ?~ r


-----


putWpn :: Id -> Ent -> Obj -> Wpn -> MudStack ()
putWpn i e o w = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar) $ at i ?~ WpnType
        modifyTVar (md^.entTblTVar)  $ at i ?~ e
        modifyTVar (md^.objTblTVar)  $ at i ?~ o
        modifyTVar (md^.wpnTblTVar)  $ at i ?~ w
