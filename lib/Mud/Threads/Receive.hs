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
import Mud.Util.List
import Mud.Util.Misc
import Mud.Util.Telnet
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logPla)

import Control.Exception.Lifted (catch)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hGetLine)
import System.IO (Handle, hIsEOF)


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
        interpTelnet i telnetDatas
        writeMsg mq . FromClient . remDelimiters $ msg
        loop
      where
        remDelimiters = T.foldr helper ""
        helper c acc  | T.singleton c `notInfixOf` delimiters = c `T.cons` acc
                      | otherwise                             = acc
        delimiters    = T.pack [ stdDesigDelimiter, nonStdDesigDelimiter, desigDelimiter ]


interpTelnet :: Id -> [TelnetData] -> MudStack ()
interpTelnet _ []  = unit
interpTelnet i tds = getState >>= \ms -> do
    let h = T.pack . getCurrHostName i $ ms
    withDbExHandler_ "interpTelnet" . insertDbTblTelnetChars . TelnetCharsRec h . commas . map pp $ tds
    case findDelimitedSubList (left, right) tds of
      Nothing -> unit
      Just [] -> unit
      Just xs -> case T.concat . map fromTelnetData $ xs of
        ""  -> unit
        txt -> liftIO mkTimestamp >>= \ts ->
            withDbExHandler_ "interpTelnet" . insertDbTblTType . TTypeRec ts h $ txt
  where
    left                      = map TCode [ TelnetIAC, TelnetSB, TelnetTTYPE, TelnetIS ]
    right                     = map TCode [ TelnetIAC, TelnetSE                        ]
    fromTelnetData (TCode  _) = ""
    fromTelnetData (TOther c) = T.singleton c
