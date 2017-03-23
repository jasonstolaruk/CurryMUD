{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Data.State.ActionParams.Misc ( capitalizeMsg
                                        , formatMsgArgs
                                        , formatMsgWithTargetArgs
                                        , punctuateMsg ) where

import           Mud.Util.Misc (PatternMatchFail)
import qualified Mud.Util.Misc as U (patternMatchFail)
import           Mud.Util.Text

import           Data.Char (isLetter)
import           Data.Text (Text)
import qualified Data.Text as T


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Data.State.ActionParams.Misc"


-- ==================================================


type Args = [Text]


formatMsgArgs :: Args -> Text
formatMsgArgs [] = ""
formatMsgArgs as = capitalizeMsg . punctuateMsg . T.unwords $ as


capitalizeMsg :: Text -> Text
capitalizeMsg x@(T.uncons         -> Just (_,  "")) = T.toUpper  x
capitalizeMsg   (T.break isLetter ->      ("", x )) = capitalize x
capitalizeMsg   (T.break isLetter ->      (x,  "")) = x
capitalizeMsg x@(T.break isLetter -> (T.uncons -> Just (c, ""), y)) | c `elem` punc = c `T.cons` capitalize y
                                                                    | otherwise     = x
  where
    punc = "('\"" :: String
capitalizeMsg x = x


punctuateMsg :: Text -> Text
punctuateMsg = \case "" -> ""
                     x@(T.uncons -> Just (c, "")) | isPunc c  -> x
                                                  | otherwise -> c `T.cons` "."
                     x@(T.last   -> c)            | isPunc c  -> x
                                                  | otherwise -> prd x
  where
    isPunc = (`elem` (".?!" :: String))


formatMsgWithTargetArgs :: Args -> (Text, Text)
formatMsgWithTargetArgs ((capitalize . T.toLower -> target):(formatMsgArgs -> msg)) = (target, msg)
formatMsgWithTargetArgs as = patternMatchFail "formatMsgWithTargetArgs" . showText $ as
