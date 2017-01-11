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
import Mud.Util.Operators
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
              , (lookMoondialHookName, lookMoondialHookFun)
              , (readSundialHookName,  readSundialHookFun )
              , (readMoondialHookName, readMoondialHookFun) ]


-----


lookSundialHook :: Hook
lookSundialHook = Hook lookSundialHookName . pure $ "sundial"


lookSundialHookName :: HookName
lookSundialHookName = "Dalben_iDalbenWelcome_lookSundial"


lookSundialHookFun :: HookFun
lookSundialHookFun = mkGenericHookFun (mkDialDesc "sun") "looks at the sundial." "looked at sundial"


mkDialDesc :: Text -> Text
mkDialDesc t = T.concat [ "The "
                        , t
                        , "dial is inlaid into a 3-foot-high square base of white marble. Two triangles affixed \
                          \perpendicular to the "
                        , t
                        , "dial's flat plate cast a shadow when the "
                        , t
                        , "'s light hits them; the time can be determined based on where that shadow falls on the \
                          \plate."
                        , theNl
                        , "You can "
                        , dblQuote "read"
                        , " the "
                        , t
                        , "dial to tell the time." ]


-----


lookMoondialHook :: Hook
lookMoondialHook = Hook lookMoondialHookName . pure $ "moondial"


lookMoondialHookName :: HookName
lookMoondialHookName = "Dalben_iDalbenWelcome_lookMoondial"


lookMoondialHookFun :: HookFun
lookMoondialHookFun = mkGenericHookFun (mkDialDesc "moon") "looks at the moondial." "looked at moondial"


-----


readSundialHook :: Hook
readSundialHook = Hook readSundialHookName . pure $ "sundial"


readSundialHookName :: HookName
readSundialHookName = "Dalben_iDalbenWelcome_readSundial"


readSundialHookFun :: HookFun
readSundialHookFun i Hook { .. } _ a@(_, (ms, _, _, _), _) =
    a & _1    %~  (\\ hookTriggers)
      & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                    in pure (serialize selfDesig <> " reads the sundial.", i `delete` desigIds selfDesig) )
      & _2._4 <>~ pure (bracketQuote hookName <> " read sundial")
      & _3    .~  pure helper
  where
    helper = do (ms', CurryTime { .. }) <- (,) <$> getState <*> liftIO getCurryTime
                let (mq, cols) = getMsgQueueColumns i ms'
                wrapSend mq cols $ if isNight curryHour
                  then "Alas, the sundial is useless when the sun isn't out."
                  else T.concat [ "The sundial reads ", showText curryHour, ":", formatMins curryMin, "." ]


formatMins :: Min -> Text
formatMins x = padTwoDigits $ (x `div` 5) * 5


-----


readMoondialHook :: Hook
readMoondialHook = Hook readMoondialHookName . pure $ "moondial"


readMoondialHookName :: HookName
readMoondialHookName = "Dalben_iDalbenWelcome_readMoondial"


readMoondialHookFun :: HookFun
readMoondialHookFun i Hook { .. } _ a@(_, (ms, _, _, _), _) =
    a & _1    %~  (\\ hookTriggers)
      & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                    in pure (serialize selfDesig <> " reads the moondial.", i `delete` desigIds selfDesig) )
      & _2._4 <>~ pure (bracketQuote hookName <> " read moondial")
      & _3    .~  pure (uncurry helper . getMsgQueueColumns i $ ms)
  where
    helper mq cols = liftIO getCurryTime >>= \CurryTime { .. } ->
        let f msg = isNight curryHour ? msg :? "Alas, the moondial is useless when the moon isn't out."
        in wrapSend mq cols . f $ case getMoonPhaseForDayOfMonth curryDayOfMonth of
          Just NewMoon -> "On account of the moon being absent from the sky tonight, you can't take a reading off the \
                          \moondial."
          _            -> T.concat [ "The moondial reads ", showText curryHour, ":", formatMins curryMin, "." ]


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
            \A few feet from a sundial stands a similar moondial.\n\
            \There is a trash bin here."
            Nothing
            Nothing
            zeroBits
            []
            (0, 0, 0)
            OutsideEnv
            (Just "Welcome")
            (M.fromList [ ("look", [ lookSundialHook, lookMoondialHook, lookTrashHook ])
                        , ("put",  [ putTrashHook                                     ])
                        , ("read", [ readSundialHook, readMoondialHook                ]) ])
            [ trashRmAction ]
            []))

  putRmTeleName iDalbenWelcome "dalben"
