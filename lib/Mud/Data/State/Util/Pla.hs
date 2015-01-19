{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE ViewPatterns #-}

module Mud.Data.State.Util.Pla where

import Mud.Data.State.State
import Mud.Data.State.Util.STM

import Control.Applicative ((<$>))
import Control.Concurrent.STM.TMVar (putTMVar)
import Control.Lens (at, over)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (?~), (.~))
import Control.Lens.Setter (ASetter)
import Data.Bits (clearBit, setBit, testBit)
import Data.IntMap.Lazy ((!))
import Data.Maybe (isNothing)


getPla :: Id -> MudStack Pla
getPla i = (! i) <$> readTMVarInNWS plaTblTMVar


getPlaColumns :: Id -> MudStack Int
getPlaColumns i = view columns <$> getPla i


getPlaIsAdmin :: Id -> MudStack Bool
getPlaIsAdmin i = plaIsAdmin <$> getPla i


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
    let p  = pt ! i
        p' = p & lens .~ val
    in putTMVar ptTMVar (pt & at i ?~ p') >> return p'


-----


data PlaFlag = IsAdmin
             | IsFirstAdminTell deriving Enum


plaHasFlag :: PlaFlag -> Pla -> Bool
plaHasFlag (fromEnum -> flagBitNum) (view plaFlags -> flags) = flags `testBit` flagBitNum


setPlaFlag :: PlaFlag -> Bool -> Pla -> Pla
setPlaFlag (fromEnum -> flagBitNum) b = over plaFlags (flip f flagBitNum)
  where
    f = case b of True  -> setBit
                  False -> clearBit


plaIsAdmin :: Pla -> Bool
plaIsAdmin = plaHasFlag IsAdmin


setPlaIsAdmin :: Bool -> Pla -> Pla
setPlaIsAdmin = setPlaFlag IsAdmin
