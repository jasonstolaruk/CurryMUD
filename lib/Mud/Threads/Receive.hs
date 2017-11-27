{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Threads.Receive (threadReceive) where

import           Mud.Cmds.Util.Misc
import           Mud.Data.Misc
import           Mud.Data.State.MsgQueue
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import           Mud.Misc.Database
import qualified Mud.Misc.Logging as L (logNotice, logPla)
import           Mud.Threads.Misc
import           Mud.TopLvlDefs.Chars
import           Mud.Util.List
import           Mud.Util.Misc
import           Mud.Util.Telnet
import           Mud.Util.Text

import           Control.Exception.Lifted (handle)
import           Control.Lens.Operators ((%~))
import           Control.Monad.IO.Class (liftIO)
import           Data.List (isInfixOf)
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hGetLine)
import           System.IO (Handle, hIsEOF)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Receive"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Receive"


-- ==================================================


threadReceive :: HasCallStack => Handle -> Id -> MsgQueue -> MudStack ()
threadReceive h i mq = handle (plaThreadExHandler i "receive") $ setThreadType (Receive i) >> loop
  where
    loop = mIf (liftIO . hIsEOF $ h)
               (sequence_ [ logPla "threadReceive loop" i "connection dropped.", writeMsg mq Dropped ])
               go
    go   = do (parseTelnet -> (msg, telnetDatas)) <- liftIO . T.hGetLine $ h
              interpTelnet i telnetDatas
              writeMsg mq . FromClient . remDelimiters $ msg
              loop
      where
        remDelimiters = T.foldr helper ""
        helper c acc  | T.singleton c `notInfixOf` delimiters = c `T.cons` acc
                      | otherwise                             = acc
        delimiters    = T.pack [ corpseDesigDelimiter, nonStdDesigDelimiter, sectionDelimiter, stdDesigDelimiter
                               , verbObjDelimiter ]


interpTelnet :: HasCallStack => Id -> [TelnetData] -> MudStack ()
interpTelnet _ []  = unit
interpTelnet i tds = do
    p@(ts, host) <- (,) <$> liftIO mkTimestamp <*> (T.pack . getCurrHostName i) `fmap` getState
    withDbExHandler_ "interpTelnet" . insertDbTblTelnetChars . TelnetCharsRec ts host . commas . map pp $ tds
    ttypeHelper p
    gmcpHelper
  where
    ttypeHelper (ts, host) = logTType (ttypeLeft, right) >> logTType (gmcpLeft, right)
      where
        logTType pair = case findDelimitedSubList pair tds of
            Nothing -> unit
            Just [] -> unit
            Just xs -> case concatMapTxt fromTelnetData xs of
              ""  -> unit
              txt -> withDbExHandler_ "interpTelnet" . insertDbTblTType . TTypeRec ts host . T.strip $ txt
        ttypeLeft = map TCode [ TelnetIAC, TelnetSB, TelnetTTYPE, TelnetIS ]
        gmcpLeft  = map TCode [ TelnetIAC, TelnetSB, TelnetGMCP            ] ++ map TOther "Core.Hello"
        fromTelnetData (TCode  _) = ""
        fromTelnetData (TOther c) = T.singleton c
    right      = map TCode [ TelnetIAC, TelnetSE ]
    gmcpHelper | ((||) <$> (gmcpWill  `isInfixOf`) <*> (gmcpDo    `isInfixOf`)) tds = setFlag True
               | ((||) <$> (gmcpWon't `isInfixOf`) <*> (gmcpDon't `isInfixOf`)) tds = setFlag False
               | otherwise = unit
      where
        setFlag b = getSing i <$> getState >>= \s -> let msg = T.concat [ "turning GMCP ", onOff b, " for ", s, "." ]
                                                     in sequence_ [ logNotice "interpTelnet gmcpHelper setFlag" msg
                                                                  , tweak $ plaTbl.ind i %~ setPlaFlag IsGmcp b ]
        gmcpWill  = mkCodes TelnetWILL
        gmcpWon't = mkCodes TelnetWON'T
        gmcpDo    = mkCodes TelnetDO
        gmcpDon't = mkCodes TelnetDON'T
        mkCodes x = map TCode [ TelnetIAC, x, TelnetGMCP ]
