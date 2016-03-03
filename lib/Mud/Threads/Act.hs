{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards #-}

module Mud.Threads.Act ( DrinkBundle(..)
                       , drinkAct
                       , startAct ) where

import Mud.Cmds.Util.Misc
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.MsgQueue
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
import qualified Mud.Misc.Logging as L (logPla)

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (finally, handle)
import Control.Lens (at, to)
import Control.Lens.Operators ((&), (.~), (?~), (^.))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (delete)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified Data.Text as T


default (Int)


-----


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Act"


-- ==================================================


startAct :: Id -> ActType -> MudStack () -> MudStack ()
startAct i actType f = getState >>= \ms -> do
    when (getType i ms == PCType) . logPla  "startAct" i $ "starting act: " <> pp actType
    a <- runAsync . threadAct i actType $ f
    tweak $ mobTbl.ind i.actMap.at actType ?~ a


threadAct :: Id -> ActType -> MudStack () -> MudStack ()
threadAct i actType f = let a = (>> f) . setThreadType $ case actType of Drinking -> DrinkingThread i
                                                                         Eating   -> EatingThread   i
                                                                         Moving   -> MovingThread   i
                            b = tweak $ mobTbl.ind i.actMap.at actType .~ Nothing
                        in handle (threadExHandler tn) $ a `finally` b -- TODO: We probably want to use "dieSilently" here...
  where
    tn = quoteWith' (pp actType, showText i) " "


data DrinkBundle = DrinkBundle { drinkId       :: Id
                               , drinkMq       :: MsgQueue
                               , drinkCols     :: Cols
                               , drinkTargetId :: Id
                               , drinkSing     :: Sing
                               , drinkLiq      :: Liq
                               , drinkAmt      :: Mouthfuls }


drinkAct :: DrinkBundle -> MudStack () -- TODO: Logging.
drinkAct DrinkBundle { .. } = do
    multiSendHelper . dropEmpties $ [ T.concat [ "You begin drinking "
                                               , drinkLiq^.liqName.to theOnLower
                                               , " from the "
                                               , drinkSing
                                               , "." ]
                                    , drinkLiq^.drinkDesc ]
    d <- flip (mkStdDesig drinkId) DoCap <$> getState
    bcastIfNotIncogNl drinkId . pure $ ( T.concat [ serialize d, " begins drinking from ", aOrAn drinkSing, "." ]
                                       , drinkId `delete` desigIds d )
    loop 0
  where
    sendHelper      = wrapSend      drinkMq drinkCols
    multiSendHelper = multiWrapSend drinkMq drinkCols
    loop x = do
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
        if | isNothing newCont -> (>> bcastHelper True) . multiSendHelper $
               [ T.concat [ "You drain the "
                          , drinkSing
                          , " dry after "
                          , showText . succ $ x
                          , " mouthful"
                          , theLetterS $ x /= 0
                          , "." ]
               , mkFullDesc stomAvail stomSize ]
           | stomAvail == 0 -> (>> bcastHelper False) . sendHelper . T.concat $
               [ "You are so full after "
               , showText . succ $ x
               , " mouthful"
               , theLetterS $ x /= 0
               , " that you have to stop drinking. You don't feel so good..." ]
           | succ x == drinkAmt -> (>> bcastHelper False) . multiSendHelper $ [ "You finish drinking."
                                                                              , mkFullDesc stomAvail stomSize ]
           | otherwise          -> sendHelper "tick" >> (loop . succ $ x) -- TODO
