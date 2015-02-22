module Mud.Data.State.Util.Put where

import Mud.Data.State.State
import Mud.Data.State.Util.STM

import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Lens (at)
import Control.Lens.Operators ((&), (?~), (.~))
import Control.Monad.Reader (ask)
import Control.Monad.STM (atomically)


putArm :: Id -> Ent -> Obj -> Arm -> MudStack () -- TODO: Can we "(& at i ?~ ObjType)"?
putArm i e o a = liftIO . atomically . helperSTM = << ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar) $ \tbl -> tbl & at i ?~ ArmType
        modifyTVar (md^.entTblTVar)  $ \tbl -> tbl & at i ?~ e
        modifyTVar (md^.objTblTVar)  $ \tbl -> tbl & at i ?~ o
        modifyTVar (md^.armTblTVar)  $ \tbl -> tbl & at i ?~ a


-----


putCloth :: Id -> Ent -> Obj -> Cloth -> MudStack ()
putCloth i e o c = liftIO . atomically . helperSTM = << ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar) $ \tbl -> tbl & at i ?~ ClothType
        modifyTVar (md^.entTblTVar)  $ \tbl -> tbl & at i ?~ e
        modifyTVar (md^.objTblTVar)  $ \tbl -> tbl & at i ?~ o
        modifyTVar (md^.objTblTVar)  $ \tbl -> tbl & at i ?~ c


-----


putCon :: Id -> Ent -> Obj -> Inv -> Coins -> Maybe Cloth -> Con -> MudStack ()
putCon i e o is coi mc con = liftIO . atomically . helperSTM = << ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar)  $ \tbl -> tbl & at i ?~ ConType
        modifyTVar (md^.entTblTVar)   $ \tbl -> tbl & at i ?~ e
        modifyTVar (md^.objTblTVar)   $ \tbl -> tbl & at i ?~ o
        modifyTVar (md^.invTblTVar)   $ \tbl -> tbl & at i ?~ is
        modifyTVar (md^.coinsTblTVar) $ \tbl -> tbl & at i ?~ coi
        modifyTVar (md^.clothTblTVar) $ \tbl -> tbl & at i ?~ mc
        modifyTVar (md^.conTblTVar)   $ \tbl -> tbl & at i ?~ con


-----


putMob :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> MudStack ()
putMob i e is c em m = liftIO . atomically . helperSTM = << ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar)  $ \tbl -> tbl & at i ?~ MobType
        modifyTVar (md^.entTblTVar)   $ \tbl -> tbl & at i ?~ e
        modifyTVar (md^.invTblTVar)   $ \tbl -> tbl & at i ?~ is
        modifyTVar (md^.coinsTblTVar) $ \tbl -> tbl & at i ?~ c
        modifyTVar (md^.eqTblTVar)    $ \tbl -> tbl & at i ?~ em
        modifyTVar (md^.mobTblTVar)   $ \tbl -> tbl & at i ?~ m


-----


putObj :: Id -> Ent -> Obj -> MudStack ()
putObj i e o = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar) $ \tbl -> tbl & at i ?~ ObjType
        modifyTVar (md^.entTblTVar)  $ \tbl -> tbl & at i ?~ e
        modifyTVar (md^.objTblTVar)  $ \tbl -> tbl & at i ?~ o


-----


putPC :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> PC -> MudStack ()
putPC i e is c em m p = liftIO . atomically . helperSTM = << ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar)  $ \tbl -> tbl & at i ?~ PCType
        modifyTVar (md^.entTblTVar)   $ \tbl -> tbl & at i ?~ e
        modifyTVar (md^.invTblTVar)   $ \tbl -> tbl & at i ?~ is
        modifyTVar (md^.coinsTblTVar) $ \tbl -> tbl & at i ?~ c
        modifyTVar (md^.eqTblTVar)    $ \tbl -> tbl & at i ?~ em
        modifyTVar (md^.mobTblTVar)   $ \tbl -> tbl & at i ?~ m
        modifyTVar (md^.pcTblTVar)    $ \tbl -> tbl & at i ?~ p


-----


putRm :: Id -> Inv -> Coins -> Rm -> MudStack ()
putRm i is c r = liftIO . atomically . helperSTM = << ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar)  $ \tbl -> tbl & at i ?~ RmType
        modifyTVar (md^.invTblTVar)   $ \tbl -> tbl & at i ?~ is
        modifyTVar (md^.coinsTblTVar) $ \tbl -> tbl & at i ?~ c
        modifyTVar (md^.rmTblTVar)    $ \tbl -> tbl & at i ?~ r


-----


putWpn :: Id -> Ent -> Obj -> Wpn -> MudStack ()
putWpn i e o w = liftIO . atomically . helperSTM = << ask
  where
    helperSTM md = do
        modifyTVar (md^.typeTblTVar) $ \tbl -> tbl & at i ?~ WpnType
        modifyTVar (md^.entTblTVar)  $ \tbl -> tbl & at i ?~ e
        modifyTVar (md^.objTblTVar)  $ \tbl -> tbl & at i ?~ o
        modifyTVar (md^.wpnTblTVar)  $ \tbl -> tbl & at i ?~ w
