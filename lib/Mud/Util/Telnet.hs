{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Telnet where

import Mud.TopLvlDefs.Telnet.Chars
import Mud.Util.Operators
import Mud.Util.Text

import Control.Arrow (second)
import Data.Char (ord)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


isTelnetTTypeResponse :: Text -> Bool
isTelnetTTypeResponse = (telnetTTypeResponseL `T.isInfixOf`)


-----


parseTelnet :: Text -> Maybe [Text]
parseTelnet msg = case helper msg of [] -> Nothing
                                     xs -> Just xs
  where
    helper :: Text -> [Text]
    helper "" = []
    helper t  | telnetIAC_SB `T.isInfixOf` t =
                  let (left, T.drop 2 -> right) = T.breakOn telnetIAC_SB t
                  in (helper left ++) . ([ "IAC", "SB" ] ++) $ case T.breakOn telnetIAC_SE right of
                    (_,   ""              ) -> mkAsciiNoTxts right -- There is no IAC SE.
                    (sub, T.drop 2 -> rest) -> mkAsciiNoTxts sub ++ [ "IAC", "SE" ] ++ helper rest
              | otherwise = case T.breakOn (T.singleton telnetIAC) t of
                (_, "") -> [] -- There is no IAC.
                (_, t') -> let (codes, rest) = T.splitAt 3 t'
                           in "IAC" : mkAsciiNoTxts (T.drop 1 codes) ++ helper rest
    mkAsciiNoTxts txt = [ showText . ord $ c | c <- T.unpack txt ]


-----


-- Assumes "telnetTTypeResponseL" is infix of msg.
parseTelnetTTypeResponse :: Text -> (Text, Text)
parseTelnetTTypeResponse msg | (l, T.drop (T.length telnetTTypeResponseL) -> r) <- T.breakOn telnetTTypeResponseL msg
                             = second (l |&|) $ case T.breakOn telnetTTypeResponseR r of
                                 (ttype, "") -> (ttype, id)
                                 (ttype, r') -> (ttype, (<> T.drop (T.length telnetTTypeResponseR) r'))

-----


stripTelnet :: Text -> Text
stripTelnet t
  | T.singleton telnetIAC `T.isInfixOf` t, (left, right) <- T.breakOn (T.singleton telnetIAC) t = left <> helper right
  | otherwise = t
  where
    -- IAC should be followed by 2 bytes.
    -- The exception is IAC SB, in which case a subnegotiation sequence continues until IAC SE.
    helper (T.uncons -> Just (_, T.uncons -> Just (x, T.uncons -> Just (_, rest))))
      | x == telnetSB = case T.breakOn (T.singleton telnetSE) rest of (_, "")              -> ""
                                                                      (_, T.tail -> rest') -> stripTelnet rest'
      | otherwise     = stripTelnet rest
    helper _ = ""
