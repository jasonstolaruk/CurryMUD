{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Telnet where

import           Mud.Data.Misc
import           Mud.TopLvlDefs.Telnet.Chars
import           Mud.TopLvlDefs.Telnet.CodeMap
import qualified Mud.Util.Misc as U (patternMatchFail)
import           Mud.Util.Misc hiding (patternMatchFail)
import           Mud.Util.Operators
import           Mud.Util.Text

import           Data.Char (ord)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.IntMap.Strict as IM (lookup)
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
        helper (left, T.uncons -> Just (_ {- IAC -}, T.uncons -> Just (x, rest)))
          | x == telnetIAC = -- IAC IAC: an escaped 255.
              f (msg <> left, td ++ replicate 2 (TCode TelnetIAC)) rest
        helper (left, T.uncons -> Just (_ {- IAC -}, T.uncons -> Just (x, T.uncons -> Just (y, rest))))
          | x == telnetSB = -- IAC SB: a subnegotiation sequence should continue until IAC SE.
              case T.breakOn (T.singleton telnetSE) rest of
                (malformed, "") -> -- There is no SE.
                  let telnets = [ TCode TelnetIAC, TCode TelnetSB ] ++ mkTelnetDatas others
                      others  = y : T.unpack malformed
                  in (msg <> left, td ++ telnets)
                (codes, T.tail -> rest') ->
                  let telnets = [ TCode TelnetIAC, TCode TelnetSB ] ++ mkTelnetDatas others
                      others  = y : T.unpack codes ++ pure telnetSE
                  in f (msg <> left, td ++ telnets) rest'
          | x `elem` [ telnetWILL, telnetWON'T, telnetDO, telnetDON'T ] =
              -- It's a 3-byte command (including the IAC).
              f (msg <> left, td ++ pure (TCode TelnetIAC) ++ mkTelnetDatas [ x, y ]) rest
          | otherwise =
              -- It's a 2-byte command (including the IAC).
              f (msg <> left, td ++ pure (TCode TelnetIAC) ++ mkTelnetDatas (pure x)) $ y `T.cons` rest
        -- There is a single IAC and nothing else, or an IAC followed by just one byte.
        helper (left, right) | right == T.singleton telnetIAC = (msg <> left, td ++ pure (TCode TelnetIAC))
                             | T.length right == 2 = let telnets = TCode TelnetIAC : mkTelnetDatas others
                                                         others  = T.unpack . T.drop 1 $ right
                                                     in (msg <> left, td ++ telnets)
        helper pair = patternMatchFail "parseTelnet f helper" . showTxt $ pair
    mkTelnetDatas = map g
      where
        g c = case ord c `IM.lookup` telnetCodeMap of Nothing -> TOther c
                                                      Just tc -> TCode tc
