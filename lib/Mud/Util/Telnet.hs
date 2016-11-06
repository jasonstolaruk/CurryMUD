{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Telnet where

import Mud.Data.Misc
import Mud.TopLvlDefs.Telnet.Chars
import Mud.TopLvlDefs.Telnet.CodeMap
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Char (ord)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.IntMap.Lazy as IM (lookup)
import qualified Data.Text as T


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Util.Telnet"


-- ==================================================


parseTelnet :: Text -> (Text, [TelnetData])
parseTelnet = f ("", [])
  where
    f pair      "" = pair
    f (msg, td) t  | p@(_, right) <- T.breakOn (T.singleton telnetIAC) t, ()!# right = helper p
                   | otherwise = (msg <> t, td) -- There is no IAC.
      where
        -- IAC should be followed by 2 bytes.
        -- The exceptions are IAC IAC (which is an escaped 255), and IAC SB (in which case a subnegotiation sequence
        -- continues until IAC SE).
        helper (left, T.uncons -> Just (_ {- IAC -}, T.uncons -> Just (x, rest))) | x == telnetIAC =
            f (msg <> left, td ++ replicate 2 (TCode TelnetIAC)) rest
        helper (left, T.uncons -> Just (_ {- IAC -}, T.uncons -> Just (x, T.uncons -> Just (y, rest))))
          | x == telnetSB = case T.breakOn (T.singleton telnetSE) rest of
            -- There is no SE.
            (malformed, ""             ) -> let telnets = [ TCode TelnetIAC, TCode TelnetSB ] ++ mkTelnetDatas others
                                                others  = y : T.unpack malformed
                                            in (msg <> left, td ++ telnets)
            (codes,     T.tail -> rest') -> let telnets = [ TCode TelnetIAC, TCode TelnetSB ] ++ mkTelnetDatas others
                                                others  = y : T.unpack codes ++ pure telnetSE
                                            in f (msg <> left, td ++ telnets) rest'
          | otherwise = f (msg <> left, td ++ pure (TCode TelnetIAC) ++ mkTelnetDatas [ x, y ]) rest
        -- Malformed. There is a single IAC and nothing else, or an IAC followed by just one byte.
        helper (left, right) | right == T.singleton telnetIAC = (msg <> left, td ++ pure (TCode TelnetIAC))
                             | T.length right == 2 = let telnets = TCode TelnetIAC : mkTelnetDatas others
                                                         others  = T.unpack . T.drop 1 $ right
                                                     in (msg <> left, td ++ telnets)
        helper pair = patternMatchFail "parseTelnet" . showText $ pair
    mkTelnetDatas = map g
      where
        g c = case ord c `IM.lookup` telnetCodeMap of Nothing -> TOther c
                                                      Just tc -> TCode tc
