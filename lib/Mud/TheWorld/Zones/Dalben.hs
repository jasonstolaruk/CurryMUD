{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Mud.TheWorld.Zones.Dalben ( createDalben
                                 , dalbenHooks
                                 , dalbenRmActionFuns ) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Make
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Put
import Mud.Misc.CurryTime
import Mud.TheWorld.Misc
import Mud.TheWorld.Zones.DalbenIds
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Data.Text as T
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Lens (_1, _2, _3, _4)
import Control.Lens.Operators ((%~), (&), (.~), (<>~))
import Control.Monad.IO.Class (liftIO)
import Data.Bits (zeroBits)
import Data.List ((\\), delete)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Map.Lazy as M (fromList)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.Zones.Dalben"


-- ==================================================
-- Hooks:


dalbenHooks :: [(HookName, HookFun)]
dalbenHooks = [ (lookSundialHookName,  lookSundialHookFun )
              , (lookMoondialHookName, lookMoondialHookFun) ]


-----


lookSundialHook :: Hook
lookSundialHook = Hook lookSundialHookName . pure $ "sundial"


lookSundialHookName :: HookName
lookSundialHookName = "Dalben_iDalbenWelcome_lookSundial"


lookSundialHookFun :: HookFun
lookSundialHookFun i Hook { .. } _ a@(_, (ms, _, _, _), _) =
  a & _1    %~  (\\ hookTriggers)
    & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                  in pure (serialize selfDesig <> " looks at the sundial.", i `delete` desigIds selfDesig) )
    & _2._4 <>~ pure (bracketQuote hookName <> " looked at sundial")
    & _3    .~  pure helper
  where
    helper = do (ms', CurryTime { .. }) <- (,) <$> getState <*> liftIO getCurryTime
                let (mq, cols) = getMsgQueueColumns i ms'
                wrapSend mq cols $ if isNight curryHour
                  then "Alas, the sundial is useless when the sun isn't out."
                  else T.concat [ "The sundial reads ", showText curryHour, ":", padTwoDigits $ (curryMin `div` 5) * 5, "." ]


-----


lookMoondialHook :: Hook
lookMoondialHook = Hook lookMoondialHookName . pure $ "moondial"


lookMoondialHookName :: HookName
lookMoondialHookName = "Dalben_iDalbenWelcome_lookMoondial"


lookMoondialHookFun :: HookFun -- TODO: Also useless during New Moon.
lookMoondialHookFun i Hook { .. } _ a@(_, (ms, _, _, _), _) =
  a & _1    %~  (\\ hookTriggers)
    & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                  in pure (serialize selfDesig <> " looks at the moondial.", i `delete` desigIds selfDesig) )
    & _2._4 <>~ pure (bracketQuote hookName <> " looked at moondial")
    & _3    .~  pure helper
  where
    helper = do (ms', CurryTime { .. }) <- (,) <$> getState <*> liftIO getCurryTime
                let (mq, cols) = getMsgQueueColumns i ms'
                wrapSend mq cols $ if isNight curryHour
                  then T.concat [ "The moondial reads ", showText curryHour, ":", padTwoDigits $ (curryMin `div` 5) * 5, "." ]
                  else "Alas, the moondial is useless when the moon isn't out."


-- ==================================================
-- Room action functions:


dalbenRmActionFuns :: [(FunName, RmActionFun)]
dalbenRmActionFuns = []


-- ==================================================
-- Zone definition:


createDalben :: MudStack ()
createDalben = do
  logNotice "createDalben" "creating Dalben."

  putRm iDalbenWelcome
        []
        mempty
        (mkRm (RmTemplate "Welcome to Dalben"
            "Hello!\n\
            \There is a sundial and a moondial here.\n\
            \There is a trash bin here."
            Nothing
            Nothing
            zeroBits
            []
            (0, 0, 0)
            OutsideEnv
            (Just "Welcome")
            (M.fromList [ ("look", [ lookSundialHook, lookMoondialHook, lookTrashHook ])
                        , ("put",  [ putTrashHook                                     ]) ])
            [ trashRmAction ]
            []))

  putRmTeleName iDalbenWelcome "dalben"
