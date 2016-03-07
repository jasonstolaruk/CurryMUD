{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.Threads.Act ( DrinkBundle(..)
                       , drinkAct
                       , startAct
                       , stopAct
                       , stopActs
                       , stopNpcActs ) where

import Mud.Cmds.Util.Misc
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Threads.Misc
import Mud.Util.List
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (catch, finally, handle)
import Control.Lens (at, to, views)
import Control.Lens.Operators ((&), (.~), (?~), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.List (delete)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified Data.Map.Lazy as M (elems)
import qualified Data.Text as T


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Act"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Act"


-- ==================================================


startAct :: Id -> ActType -> MudStack () -> MudStack ()
startAct i actType f = do
    logPla "startAct" i $ pp actType <> " act started."
    a <- runAsync . threadAct i actType $ f
    tweak $ mobTbl.ind i.actMap.at actType ?~ a


stopAct :: Id -> ActType -> MudStack ()
stopAct i actType = views (at actType) (maybe unit throwDeath) . getActMap i =<< getState


stopActs :: Id -> MudStack ()
stopActs i = logPla "stopActs" i "stopping all acts." >> (mapM_ throwWait . M.elems . getActMap i =<< getState)


stopNpcActs :: MudStack ()
stopNpcActs = logNotice "stopNpcActs" "stopping NPC acts." >> (mapM_ stopActs =<< getNpcIds <$> getState)


threadAct :: Id -> ActType -> MudStack () -> MudStack ()
threadAct i actType f = let a = (>> f) . setThreadType $ case actType of Attacking -> undefined -- TODO
                                                                         Drinking  -> DrinkingThread i
                                                                         Eating    -> EatingThread   i
                                                                         Moving    -> MovingThread   i
                            b = do
                                tweak $ mobTbl.ind i.actMap.at actType .~ Nothing
                                logPla "threadAct" i $ pp actType <> " act finished."
                        in handle (threadExHandler . mkThreadName i $ actType) $ a `finally` b


mkThreadName :: Id -> ActType -> Text
mkThreadName i actType = quoteWith' (pp actType, showText i) " "


-- ==================================================


drinkAct :: DrinkBundle -> MudStack ()
drinkAct DrinkBundle { .. } =
    let a = do
            send drinkMq . multiWrap drinkCols . dropEmpties $ [ T.concat [ "You begin drinking "
                                                                          , drinkLiq^.liqName.to theOnLower
                                                                          , " from the "
                                                                          , drinkSing
                                                                          , "..." ]
                                                               , drinkLiq^.drinkDesc ]
            d <- flip (mkStdDesig drinkId) DoCap <$> getState
            bcastIfNotIncogNl drinkId . pure $ ( T.concat [ serialize d, " begins drinking from ", aOrAn drinkSing, "." ]
                                               , drinkId `delete` desigIds d )
            tweak $ mobTbl.ind drinkId.nowDrinking ?~ (drinkLiq, drinkSing)
            loop 0 `catch` die (Just drinkId) (mkThreadName drinkId Drinking)
        b = tweak $ mobTbl.ind drinkId.nowDrinking .~ Nothing
    in a `finally` b
  where
    loop x@(succ -> x') = do
        liftIO . threadDelay $ 1 * 10 ^ 6
        now <- liftIO getCurrentTime
        consume drinkId . pure . StomachCont (drinkLiq^.liqId.to Left) now $ False
        (ms, newCont) <- modifyState $ \ms ->
            let Just (_, m) = getVesselCont drinkTargetId ms
                newCont     = m == 1 ? Nothing :? Just (drinkLiq, pred m)
                ms'         = ms & vesselTbl.ind drinkTargetId.vesselCont .~ newCont
            in (ms', (ms', newCont))
        let (stomAvail, stomSize) = calcStomachAvailSize drinkId ms
            d                     = mkStdDesig drinkId ms DoCap
            bcastHelper b         = bcastIfNotIncogNl drinkId . pure $ ( T.concat [ serialize d
                                                                                  , " finishes drinking from "
                                                                                  , aOrAn drinkSing
                                                                                  , b |?| " after draining it dry"
                                                                                  , "." ]
                                                                       , drinkId `delete` desigIds d )
        if | isNothing newCont -> (>> bcastHelper True) . ioHelper x' $
               [ T.concat [ "You drain the "
                          , drinkSing
                          , " dry after "
                          , showText x'
                          , " mouthful"
                          , theLetterS $ x /= 0
                          , "." ]
               , mkFullDesc stomAvail stomSize ]
           | stomAvail == 0 -> (>> bcastHelper False) . ioHelper x' . pure . T.concat $
               [ "You are so full after "
               , showText x'
               , " mouthful"
               , theLetterS $ x /= 0
               , " that you have to stop drinking. You don't feel so good..." ]
           | x' == drinkAmt -> (>> bcastHelper False) . ioHelper x' $ [ "You finish drinking."
                                                                      , mkFullDesc stomAvail stomSize ]
           | otherwise -> loop x'
    ioHelper m ts = multiWrapSend drinkMq drinkCols (dropEmpties ts) >> promptHelper >> logHelper
      where
        promptHelper = sendDfltPrompt drinkMq drinkId
        logHelper    =
            logPla "drinkAct loop" drinkId . T.concat $ [ "drank "
                                                        , showText m
                                                        , " mouthfuls"
                                                        , theLetterS $ m /= 1
                                                        , " of "
                                                        , drinkLiq^.liqName.to aOrAnOnLower
                                                        , " "
                                                        , let DistinctLiqId i = drinkLiq^.liqId
                                                          in parensQuote . showText $ i
                                                        , " from "
                                                        , aOrAn drinkSing
                                                        , "." ]
