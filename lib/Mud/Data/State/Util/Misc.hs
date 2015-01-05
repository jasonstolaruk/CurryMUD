{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Data.State.Util.Misc ( BothGramNos
                                , allKeys
                                , findPCIds
                                , getEffBothGramNos
                                , getEffName
                                , getMqtPt
                                , getSexRace
                                , getUnusedId
                                , mkPlur
                                , mkPlurFromBoth
                                , mkPronoun
                                , mkReflexive
                                , mkUnknownPCEntName
                                , sortInv
                                , statefulFork
                                , statefulFork_ ) where

import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.StateInIORefT
import Mud.Data.State.Util.STM
import Mud.Util.Misc hiding (patternMatchFail)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (readTMVar)
import Control.Lens (_1, _2, both, over)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.IntMap.Lazy ((!))
import Data.List ((\\), foldl', sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.IntMap.Lazy as IM (IntMap, keys)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util.Misc"


-- ============================================================


allKeys :: WorldState -> Inv
allKeys = views typeTbl IM.keys


findPCIds :: WorldState -> [Id] -> [Id]
findPCIds ws haystack = [ i | i <- haystack, (ws^.typeTbl) ! i == PCType ]


getEffBothGramNos :: Id -> WorldState -> Id -> BothGramNos
getEffBothGramNos i ws i'
  | e <- (ws^.entTbl) ! i', mn <- e^.entName = case mn of
    Nothing | (view introduced -> intros) <- (ws^.pcTbl)  ! i
            , n                           <- e^.sing
            , (pp *** pp -> (s, r))       <- getSexRace i' ws
            -> if n `elem` intros
              then (n, "")
              else over both ((s <>) . (" " <>)) (r, pluralize r)
    Just _  -> (view sing *** view plur) . dup $ e
  where
    pluralize "dwarf" = "dwarves"
    pluralize "elf"   = "elves"
    pluralize r       = r <> "s"


getEffName :: Id -> WorldState -> Id -> T.Text
getEffName i ws i'@(((ws^.entTbl) !) -> e) = fromMaybe helper $ e^.entName
  where
    helper | n `elem` intros    = uncapitalize n
           | otherwise          = mkUnknownPCEntName i' ws
    n                           = e^.sing
    (view introduced -> intros) = (ws^.pcTbl) ! i


getMqtPt :: MudStack (IM.IntMap MsgQueue, IM.IntMap Pla)
getMqtPt = do
    (mqtTMVar, ptTMVar) <- (,) <$> getNWSRec msgQueueTblTMVar <*> getNWSRec plaTblTMVar
    liftIO . atomically $  (,) <$> readTMVar mqtTMVar         <*> readTMVar ptTMVar


getSexRace :: Id -> WorldState -> (Sex, Race)
getSexRace i ws = (view sex *** view race) . (((ws^.mobTbl) !) *** ((ws^.pcTbl) !)) . dup $ i


getUnusedId :: WorldState -> Id
getUnusedId = head . (\\) [0..] . allKeys


mkPlur :: Ent -> Plur
mkPlur e@(view plur -> p) | T.null p  = e^.sing <> "s"
                          | otherwise = p


type BothGramNos = (Sing, Plur)


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p


mkPronoun :: Sex -> T.Text
mkPronoun Male   = "he"
mkPronoun Female = "she"
mkPronoun s      = patternMatchFail "mkPronoun" [ showText s ]


mkReflexive :: Sex -> T.Text
mkReflexive Male   = "himself"
mkReflexive Female = "herself"
mkReflexive s      = patternMatchFail "mkReflexive" [ showText s ]


mkUnknownPCEntName :: Id -> WorldState -> T.Text
mkUnknownPCEntName i ws | (view sex  -> s) <- (ws^.mobTbl) ! i
                        , (view race -> r) <- (ws^.pcTbl)  ! i = (T.singleton . T.head . pp $ s) <> pp r


sortInv :: WorldState -> Inv -> Inv
sortInv ws is | (foldl' helper ([], []) . zip is -> (pcIs, nonPCIs)) <- [ (ws^.typeTbl) ! i | i <- is ]
              = (pcIs ++) . sortNonPCs $ nonPCIs
  where
    helper a (i, t) | t == PCType      = over _1 (++ [i]) a
                    | otherwise        = over _2 (++ [i]) a
    sortNonPCs is'                     = map (view _1) . sortBy nameThenSing . zip3 is' (names is') . sings $ is'
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    names is'                          = [ let e = (ws^.entTbl) ! i in fromJust $ e^.entName | i <- is' ]
    sings is'                          = [ let e = (ws^.entTbl) ! i in e^.sing               | i <- is' ]


statefulFork :: StateInIORefT MudState IO () -> MudStack MudState
statefulFork f = get >>= \s ->
    (liftIO . void . forkIO . void . runStateInIORefT f $ s) >> return s


statefulFork_ :: StateInIORefT MudState IO () -> MudStack ()
statefulFork_ f = liftIO . void . forkIO . void . runStateInIORefT f =<< get
