{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Logging where

import Mud.StateDataTypes
import Mud.TopLvlDefs
import Mud.Util

import Control.Exception (IOException, SomeException)
import Control.Exception.Lifted (throwIO)
import Control.Lens.Operators ((.=), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)
import Data.Text.Strict.Lens (packed)
import System.Log (Priority(..))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (close, setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger (errorM, noticeM, setHandlers, setLevel, updateGlobalLogger)


initLogging :: MudStack ()
initLogging = do
    initLog "notice.log" NOTICE "currymud.notice" >>= \gh -> logHandles.noticeHandle .= Just gh
    initLog "error.log"  ERROR  "currymud.error"  >>= \gh -> logHandles.errorHandle  .= Just gh
  where
    initLog fn p ln = do
        gh <- liftIO . fileHandler (logDir ++ fn) $ p
        let h = setFormatter gh . simpleLogFormatter $ "[$time $loggername] $msg"
        liftIO . updateGlobalLogger ln . setHandlers $ [h]
        liftIO . updateGlobalLogger ln . setLevel $ p
        return gh


closeLogs :: MudStack ()
closeLogs = do
    liftIO . logNotice "Mud.Logging" "closeLogs" $ "closing the logs"
    mnh <- gets (^.logHandles.noticeHandle)
    meh <- gets (^.logHandles.errorHandle)
    mapM_ closeThem [mnh, meh]
  where
    closeThem = maybe' (liftIO . close)


logNotice :: String -> String -> String -> IO ()
logNotice modName funName msg = noticeM "currymud.notice" $ concat [ modName, " ", funName, ": ", msg, "." ]


logIOEx :: String -> String -> IOException -> IO ()
logIOEx modName funName e = errorM "currymud.error" $ concat [ modName, " ", funName, ": ", dblQuoteStr . show $ e ]


logAndDispIOEx :: String -> String -> IOException -> MudStack ()
logAndDispIOEx modName funName e = do
    liftIO . errorM "currymud.error" $ msg
    output $ msg^.packed
  where
    msg = concat [ modName, " ", funName, ": ", dblQuoteStr . show $ e ]


logIOExRethrow :: String -> String -> IOException -> IO ()
logIOExRethrow modName funName e = do
    errorM "currymud.error" $ concat [ modName, " ", funName, ": unexpected exception; rethrowing." ]
    throwIO e


logExMsg :: String -> String -> String -> SomeException -> IO ()
logExMsg modName funName msg e = errorM "currymud.error" $ concat [ modName, " ", funName, ": ", msg, ". ", dblQuoteStr . show $ e ]
