{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Threads.Receive (threadReceive) where

import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Misc.Database
import Mud.Threads.Misc
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Telnet.Chars
import Mud.Util.List
import Mud.Util.Misc
import Mud.Util.Telnet
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logPla)

import Control.Exception.Lifted (catch)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hGetLine)
import System.IO (Handle, hFlush, hIsEOF)


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Receive"


-- ==================================================


threadReceive :: Handle -> Id -> MsgQueue -> MudStack ()
threadReceive h i mq = sequence_ [ setThreadType . Receive $ i, loop `catch` plaThreadExHandler i "receive" ]
  where
    loop = mIf (liftIO . hIsEOF $ h)
               (sequence_ [ logPla "threadReceive loop" i "connection dropped.", writeMsg mq Dropped ])
               go
    go = do
        (parseTelnet -> (msg, telnetDatas)) <- liftIO . T.hGetLine $ h
        interpTelnet h i mq telnetDatas
        writeMsg mq . FromClient . remDelimiters $ msg
        loop
      where
        remDelimiters = T.foldr helper ""
        helper c acc  | T.singleton c `notInfixOf` delimiters = c `T.cons` acc
                      | otherwise                             = acc
        delimiters    = T.pack [ stdDesigDelimiter, nonStdDesigDelimiter, desigDelimiter ]


interpTelnet :: Handle -> Id -> MsgQueue -> [TelnetData] -> MudStack ()
interpTelnet _ _ _  []  = unit
interpTelnet h i mq tds = do
    (ts, host) <- (,) <$> liftIO mkTimestamp <*> (T.pack . getCurrHostName i) `fmap` getState
    withDbExHandler_ "interpTelnet" . insertDbTblTelnetChars . TelnetCharsRec ts host . commas . map pp $ tds
    let logTType pair = case findDelimitedSubList pair tds of
          Nothing -> unit
          Just [] -> unit
          Just xs -> case T.concat . map fromTelnetData $ xs of
            ""  -> unit
            txt -> withDbExHandler_ "interpTelnet" . insertDbTblTType . TTypeRec ts host . T.strip $ txt
    logTType (ttypeLeft, right)
    logTType (gmcpLeft,  right)
    when (gmcpLeft `isInfixOf` tds) $ send mq telnetWon'tGMCP >> liftIO (hFlush h)
  where
    fromTelnetData (TCode  _) = ""
    fromTelnetData (TOther c) = T.singleton c
    ttypeLeft = map TCode [ TelnetIAC, TelnetSB, TelnetTTYPE, TelnetIS ]
    gmcpLeft  = map TCode [ TelnetIAC, TelnetSB, TelnetGMCP            ] ++ map TOther "Core.Hello"
    right     = map TCode [ TelnetIAC, TelnetSE                        ]
