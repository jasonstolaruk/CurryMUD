module Mud.Data.State.Util.Put where

import Mud.Data.State.MudData

import Control.Lens (at)
import Control.Lens.Operators ((.~), (?~), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.IORef (atomicModifyIORef)


putArm :: Id -> Ent -> Obj -> Arm -> MudStack ()
putArm i e o a = liftIO . helper =<< ask
  where
    helper md = atomicModifyIORef (md^.mudStateIORef) $ \ms ->
        let ms' = ms & armTbl.at  i ?~ a
                     & entTbl.at  i ?~ e
                     & objTbl.at  i ?~ o
                     & typeTbl.at i ?~ ArmType
        in (ms', ())


-----


putCloth :: Id -> Ent -> Obj -> Cloth -> MudStack ()
putCloth i e o c = liftIO . helper =<< ask
  where
    helper md = atomicModifyIORef (md^.mudStateIORef) $ \ms ->
        let ms' = ms & clothTbl.at i ?~ c
                     & entTbl.at   i ?~ e
                     & objTbl.at   i ?~ o
                     & typeTbl.at  i ?~ ClothType
        in (ms', ())


-----


putCon :: Id -> Ent -> Obj -> Inv -> Coins -> Maybe Cloth -> Con -> MudStack ()
putCon i e o is coi mc con = liftIO . helper =<< ask
  where
    helper md = atomicModifyIORef (md^.mudStateIORef) $ \ms ->
        let ms' = ms & clothTbl.at i .~ mc
                     & coinsTbl.at i ?~ coi
                     & conTbl.at   i ?~ con
                     & entTbl.at   i ?~ e
                     & invTbl.at   i ?~ is
                     & objTbl.at   i ?~ o
                     & typeTbl.at  i ?~ ConType
        in (ms', ())


-----


putMob :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> MudStack ()
putMob i e is c em m = liftIO . helper =<< ask
  where
    helper md = atomicModifyIORef (md^.mudStateIORef) $ \ms ->
        let ms' = ms & coinsTbl.at i ?~ c
                     & entTbl.at   i ?~ e
                     & eqTbl.at    i ?~ em
                     & invTbl.at   i ?~ is
                     & mobTbl.at   i ?~ m
                     & objTbl.at   i ?~ o
                     & typeTbl.at  i ?~ MobType
        in (ms', ())


-----


putObj :: Id -> Ent -> Obj -> MudStack ()
putObj i e o = liftIO . helper =<< ask
  where
    helper md = atomicModifyIORef (md^.mudStateIORef) $ \ms ->
        let ms' = ms & entTbl.at  i ?~ e
                     & objTbl.at  i ?~ o
                     & typeTbl.at i ?~ ObjType
        in (ms', ())


-----


putPC :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> PC -> MudStack ()
putPC i e is c em m p = liftIO . helper =<< ask
  where
    helper md = atomicModifyIORef (md^.mudStateIORef) $ \ms ->
        let ms' = ms & coinsTbl.at i ?~ c
                     & entTbl.at   i ?~ e
                     & eqTbl.at    i ?~ em
                     & invTbl.at   i ?~ is
                     & mobTbl.at   i ?~ m
                     & pcTbl.at    i ?~ p
                     & typeTbl.at  i ?~ PCType
        in (ms', ())


-----


putRm :: Id -> Inv -> Coins -> Rm -> MudStack ()
putRm i is c r = liftIO . helper =<< ask
  where
    helper md = atomicModifyIORef (md^.mudStateIORef) $ \ms ->
        let ms' = ms & coinsTbl.at i ?~ c
                     & invTbl.at   i ?~ is
                     & rmTbl.at    i ?~ r
                     & typeTbl.at  i ?~ RmType
        in (ms', ())


-----


putWpn :: Id -> Ent -> Obj -> Wpn -> MudStack ()
putWpn i e o w = liftIO . helper =<< ask
  where
    helper md = atomicModifyIORef (md^.mudStateIORef) $ \ms ->
        let ms' = ms & entTbl.at  i ?~ e
                     & objTbl.at  i ?~ o
                     & typeTbl.at i ?~ WpnType
                     & wpnTbl.at  i ?~ w
        in (ms', ())
