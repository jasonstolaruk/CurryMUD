{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards, TupleSections, ViewPatterns #-}

module Mud.Threads.Act ( drinkAct
                       , startAct
                       , stopAct
                       , stopActs
                       , stopNpcActs ) where

import Mud.Cmds.Util.Misc
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
import Data.Maybe (isJust)
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


startAct :: Id -> ActType -> Fun -> MudStack ()
startAct i actType f = do
    logPla "startAct" i $ pp actType <> " act started."
    a <- runAsync . threadAct i actType $ f
    tweak $ mobTbl.ind i.actMap.at actType ?~ a


stopAct :: Id -> ActType -> MudStack ()
stopAct i actType = views (at actType) (maybeVoid throwDeath) . getActMap i =<< getState


stopActs :: Id -> MudStack ()
stopActs i = do { logPla "stopActs" i "stopping all acts."; mapM_ throwWait . M.elems . getActMap i =<< getState }


stopNpcActs :: MudStack ()
stopNpcActs = do { logNotice "stopNpcActs" "stopping NPC acts."; mapM_ stopActs =<< getNpcIds <$> getState }


threadAct :: Id -> ActType -> Fun -> MudStack ()
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
            send drinkerMq . multiWrap drinkerCols . dropEmpties $ [ T.concat [ "You begin drinking "
                                                                              , renderLiqNoun drinkLiq the
                                                                              , " from the "
                                                                              , drinkVesselSing
                                                                              , "..." ]
                                                                   , drinkLiq^.liqDrinkDesc ]
            d <- flip (mkStdDesig drinkerId) DoCap <$> getState
            bcastIfNotIncogNl drinkerId . pure $ ( T.concat [ serialize d
                                                            , " begins drinking from "
                                                            , renderVesselSing
                                                            , "." ]
                                                 , drinkerId `delete` desigIds d )
            tweak $ mobTbl.ind drinkerId.nowDrinking ?~ (drinkLiq, drinkVesselSing)
            loop 0 `catch` die (Just drinkerId) (mkThreadName drinkerId Drinking)
        b = tweak $ mobTbl.ind drinkerId.nowDrinking .~ Nothing
    in a `finally` b
  where
    renderVesselSing    = drinkVesselSing |&| (isJust drinkVesselId ? aOrAn :? the)
    loop x@(succ -> x') = do
        liftIO . threadDelay $ 1 * 10 ^ 6
        now <- liftIO getCurrentTime
        consume drinkerId . pure . StomachCont (drinkLiq^.liqId.to Left) now $ False
        (ms, newCont) <- case drinkVesselId of
          Just i  -> modifyState $ \ms -> let Just (_, m) = getVesselCont i ms
                                              newCont     = m == 1 ? Nothing :? Just (drinkLiq, pred m)
                                              ms'         = ms & vesselTbl.ind i.vesselCont .~ newCont
                                          in (ms', (ms', Right newCont))
          Nothing -> (, Left ()) <$> getState
        let (stomAvail, _) = calcStomachAvailSize drinkerId ms
            d              = mkStdDesig drinkerId ms DoCap
            bcastHelper b  = bcastIfNotIncogNl drinkerId . pure $ ( T.concat [ serialize d
                                                                             , " finishes drinking from "
                                                                             , renderVesselSing
                                                                             , b |?| " after draining it dry"
                                                                             , "." ]
                                                                  , drinkerId `delete` desigIds d )
        if | newCont == Right Nothing -> (>> bcastHelper True) . ioHelper x' $ T.concat [ "You drain the "
                                                                                        , drinkVesselSing
                                                                                        , " dry after "
                                                                                        , showText x'
                                                                                        , " mouthful"
                                                                                        , theLetterS $ x /= 0
                                                                                        , "." ]
           | stomAvail == 0 -> (>> bcastHelper False) . ioHelper x' . T.concat $ [ "You are so full after "
                                                                                 , showText x'
                                                                                 , " mouthful"
                                                                                 , theLetterS $ x /= 0
                                                                                 , " that you have to stop drinking. \
                                                                                   \You don't feel so good..." ]
           | x' == drinkAmt -> (>> bcastHelper False) . ioHelper x' $ "You finish drinking."
           | otherwise      -> loop x'
    ioHelper m t = wrapSend drinkerMq drinkerCols t >> promptHelper >> logHelper
      where
        promptHelper = sendDfltPrompt drinkerMq drinkerId
        logHelper    =
            logPla "drinkAct loop" drinkerId . T.concat $ [ "drank "
                                                          , showText m
                                                          , " mouthful"
                                                          , theLetterS $ m /= 1
                                                          , " of "
                                                          , renderLiqNoun drinkLiq aOrAn
                                                          , " "
                                                          , let DistinctLiqId i = drinkLiq^.liqId
                                                            in parensQuote . showText $ i
                                                          , " from "
                                                          , renderVesselSing
                                                          , "." ]
