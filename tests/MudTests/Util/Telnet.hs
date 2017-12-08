{-# LANGUAGE OverloadedStrings #-}

module MudTests.Util.Telnet where

import           Mud.Data.Misc
import           Mud.TopLvlDefs.Telnet.Chars
import           Mud.Util.Quoting
import           Mud.Util.Telnet

import           Data.Char (chr)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.Tasty.HUnit ((@?=), Assertion)

test_parseTelnet_null :: Assertion
test_parseTelnet_null = actual @?= expected
  where
    actual   = parseTelnet ""
    expected = ("", [])

test_parseTelnet_noTelnet :: Assertion
test_parseTelnet_noTelnet = actual @?= expected
  where
    actual   = parseTelnet "test"
    expected = ("test", [])

test_parseTelnet_telnetTxt :: Assertion
test_parseTelnet_telnetTxt = actual @?= expected
  where
    actual   = parseTelnet telnetTxt
    expected = ("", telnetDatas)

telnetTxt :: Text -- This is what Mudlet sends on connect.
telnetTxt = T.pack . map chr $ [ 255 -- IAC
                               , 252 -- WON'T
                               , 3   -- SUPPRESS GO AHEAD
                               , 255 -- IAC
                               , 250 -- SB
                               , 201 -- GMCP
                               , 67  -- C
                               , 111 -- o
                               , 114 -- r
                               , 101 -- e
                               , 46  -- .
                               , 83  -- S
                               , 117 -- u
                               , 112 -- p
                               , 112 -- p
                               , 111 -- o
                               , 114 -- r
                               , 116 -- t
                               , 115 -- s
                               , 46  -- .
                               , 83  -- S
                               , 101 -- e
                               , 116 -- t
                               , 32  -- (space)
                               , 91  -- [
                               , 93  -- ]
                               , 255 -- IAC
                               , 240 {- SE -} ]

telnetDatas :: [TelnetData]
telnetDatas = [ TCode TelnetIAC
              , TCode TelnetWON'T
              , TCode TelnetSUPPRESS_GA
              , TCode TelnetIAC
              , TCode TelnetSB
              , TCode TelnetGMCP
              , TOther 'C'
              , TOther 'o'
              , TOther 'r'
              , TOther 'e'
              , TOther '.'
              , TOther 'S'
              , TOther 'u'
              , TOther 'p'
              , TOther 'p'
              , TOther 'o'
              , TOther 'r'
              , TOther 't'
              , TOther 's'
              , TOther '.'
              , TOther 'S'
              , TOther 'e'
              , TOther 't'
              , TOther ' '
              , TOther '['
              , TOther ']'
              , TCode TelnetIAC
              , TCode TelnetSE ]

test_parseTelnet_leading :: Assertion
test_parseTelnet_leading = actual @?= expected
  where
    actual   = parseTelnet $ telnetTxt <> "test"
    expected = ("test", telnetDatas)

test_parseTelnet_trailing :: Assertion
test_parseTelnet_trailing = actual @?= expected
  where
    actual   = parseTelnet $ "test" <> telnetTxt
    expected = ("test", telnetDatas)

test_parseTelnet_leadingAndTrailing :: Assertion
test_parseTelnet_leadingAndTrailing = actual @?= expected
  where
    actual   = parseTelnet . quoteWith telnetTxt $ "test"
    expected = ("test", concat . replicate 2 $ telnetDatas)

test_parseTelnet_intercalated :: Assertion
test_parseTelnet_intercalated = actual @?= expected
  where
    actual   = parseTelnet . T.intercalate telnetTxt $ [ "test1", "test2", "test3", "test4", "test5" ]
    expected = ("test1test2test3test4test5", concat . replicate 4 $ telnetDatas)

test_parseTelnet_escapedIAC :: Assertion
test_parseTelnet_escapedIAC = actual @?= expected
  where
    actual   = parseTelnet $ "abc" <> T.replicate 2 (T.singleton telnetIAC) <> "def"
    expected = ("abcdef", replicate 2 . TCode $ TelnetIAC)

-- A single IAC, nothing but.
test_parseTelnet_malformed1 :: Assertion
test_parseTelnet_malformed1 = actual @?= expected
  where
    actual   = parseTelnet . T.singleton $ telnetIAC
    expected = ("", pure . TCode $ TelnetIAC)

-- A single IAC, trailing.
test_parseTelnet_malformed2 :: Assertion
test_parseTelnet_malformed2 = actual @?= expected
  where
    actual   = parseTelnet $ "abc" `T.snoc` telnetIAC
    expected = ("abc", pure . TCode $ TelnetIAC)

-- IAC + 'x'.
test_parseTelnet_malformed3 :: Assertion
test_parseTelnet_malformed3 = actual @?= expected
  where
    actual   = parseTelnet . T.pack $ [ telnetIAC, 'x' ]
    expected = ("", [ TCode TelnetIAC, TOther 'x' ])

-- IAC + 'x', trailing.
test_parseTelnet_malformed4 :: Assertion
test_parseTelnet_malformed4 = actual @?= expected
  where
    actual   = parseTelnet . T.pack $ "abc" ++ [ telnetIAC, 'x' ]
    expected = ("abc", [ TCode TelnetIAC, TOther 'x' ])

-- IAC + SB.
test_parseTelnet_malformed5 :: Assertion
test_parseTelnet_malformed5 = actual @?= expected
  where
    actual   = parseTelnet . T.pack $ [ telnetIAC, telnetSB ]
    expected = ("", [ TCode TelnetIAC, TCode TelnetSB ])

-- IAC + SB, trailing.
test_parseTelnet_malformed6 :: Assertion
test_parseTelnet_malformed6 = actual @?= expected
  where
    actual   = parseTelnet . T.pack $ "abc" ++ [ telnetIAC, telnetSB ]
    expected = ("abc", [ TCode TelnetIAC, TCode TelnetSB ])

-- IAC + SE, trailing.
test_parseTelnet_malformed7 :: Assertion
test_parseTelnet_malformed7 = actual @?= expected
  where
    actual   = parseTelnet . T.pack $ "abc" ++ [ telnetIAC, telnetSE ]
    expected = ("abc", [ TCode TelnetIAC, TCode TelnetSE ])

-- IAC + SB + 'x'.
test_parseTelnet_malformed8 :: Assertion
test_parseTelnet_malformed8 = actual @?= expected
  where
    actual   = parseTelnet . T.pack $ [ telnetIAC, telnetSB, 'x' ]
    expected = ("", [ TCode TelnetIAC, TCode TelnetSB, TOther 'x' ])

-- IAC + SB + 'x', trailing.
test_parseTelnet_malformed9 :: Assertion
test_parseTelnet_malformed9 = actual @?= expected
  where
    actual   = parseTelnet . T.pack $ "abc" ++ [ telnetIAC, telnetSB, 'x' ]
    expected = ("abc", [ TCode TelnetIAC, TCode TelnetSB, TOther 'x' ])

-- IAC + SB + "xyz".
test_parseTelnet_malformed10 :: Assertion
test_parseTelnet_malformed10 = actual @?= expected
  where
    actual   = parseTelnet . T.pack $ telnetIAC : telnetSB : "xyz"
    expected = ("", [ TCode TelnetIAC, TCode TelnetSB, TOther 'x', TOther 'y', TOther 'z' ])
