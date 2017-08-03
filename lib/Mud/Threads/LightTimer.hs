{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.LightTimer (threadLightTimer) where

-- import           Mud.Data.State.MsgQueue
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
-- import           Mud.Data.State.Util.Output
import           Mud.Threads.Misc
-- import           Mud.TopLvlDefs.Misc
import           Mud.Util.Misc
-- import           Mud.Util.Operators
-- import           Mud.Util.Quoting
-- import           Mud.Util.Text

import           Control.Exception.Lifted (catch, finally)
import           Control.Lens (at)
import           Control.Lens.Operators ((%~), (.~))
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid ((<>))
-- import           Data.Text (Text)
-- import qualified Data.Text as T
import           GHC.Stack (HasCallStack)


threadLightTimer :: HasCallStack => Id -> MudStack ()
threadLightTimer i = helper `catch` threadExHandler (Just i) "light timer"
  where
    helper = descSingId i <$> getState >>= \singId ->
        let f = sequence_ [ setThreadType . LightTimer $ i, loop `catch` die Nothing ("light timer for " <> singId) ]
        in f `finally` cleanUp
    loop = getState >>= \ms ->
        let secs = getLightSecs i ms
        in if secs > 0
          then do liftIO . delaySecs $ 1
                  tweak $ lightTbl.ind i.lightSecs %~ pred
                  loop
          else undefined -- TODO: Inform everyone in the room that the light has gone out.
    cleanUp = tweaks [ lightTbl     .ind i.lightIsLit .~ False
                     , lightAsyncTbl.at  i            .~ Nothing ]
