{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Threads.Receive (threadReceive) where

import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Output
import Mud.Misc.Database
import Mud.Threads.Misc
import Mud.TopLvlDefs.Chars
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
        interpTelnet telnetDatas
        writeMsg mq . FromClient . remDelimiters $ msg
        loop
      where
        remDelimiters = T.foldr helper ""
        helper c acc  | T.singleton c `notInfixOf` delimiters = c `T.cons` acc
                      | otherwise                             = acc
        delimiters    = T.pack [ stdDesigDelimiter, nonStdDesigDelimiter, desigDelimiter ]


interpTelnet :: [TelnetData] -> MudStack ()
interpTelnet []  = unit
interpTelnet tds = logThem
-- telnetTTypeResponseL = T.pack [ telnetIAC, telnetSB, telnetTTYPE, telnetIS ] -- TODO: Delete.
-- telnetTTypeResponseR = T.pack [ telnetIAC, telnetSE                        ] -- TODO: Delete.

-- Assumes "telnetTTypeResponseL" is infix of msg.
-- parseTelnetTTypeResponse :: Text -> (Text, Text) -- TODO: Delete.
-- parseTelnetTTypeResponse msg | (l, T.drop (T.length telnetTTypeResponseL) -> r) <- T.breakOn telnetTTypeResponseL msg
--                              = second (l |&|) $ case T.breakOn telnetTTypeResponseR r of
--                                  (ttype, "") -> (ttype, id)
--                                  (ttype, r') -> (ttype, (<> T.drop (T.length telnetTTypeResponseR) r'))

  where
    logThem = withDbExHandler_ "interpTelnet" . insertDbTblTelnetChars . TelnetCharsRec . commas . map pp $ tds
    -- tTypeHelper :: MudStack Text
    -- tTypeHelper = let (ttype, msg') = parseTelnetTTypeResponse msg
    --               in (,) <$> getState <*> liftIO mkTimestamp >>= \(ms, ts) -> do
    --                   let h = T.pack . getCurrHostName i $ ms
    --                   withDbExHandler_ "handleFromClient" . insertDbTblTType . TTypeRec ts h $ ttype
    --                   return msg'
