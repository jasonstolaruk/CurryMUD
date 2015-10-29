{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Data.State.ActionParams.Misc ( Args
                                        , capitalizeMsg
                                        , formatMsgArgs
                                        , formatMsgWithTargetArgs
                                        , punctuateMsg ) where

import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Char (isLetter)
import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Data.State.ActionParams.Util"


-- ==================================================


type Args = [T.Text]


formatMsgArgs :: Args -> T.Text
formatMsgArgs [] = ""
formatMsgArgs as = capitalizeMsg . punctuateMsg . T.unwords $ as


capitalizeMsg :: T.Text -> T.Text
capitalizeMsg x@(T.uncons         -> Just (_, "")) = T.toUpper  x
capitalizeMsg   (T.break isLetter ->      ("", x)) = capitalize x
capitalizeMsg   (T.break isLetter ->      (x, "")) = x
capitalizeMsg x@(T.break isLetter -> (T.uncons -> Just (c, ""), y)) | c `elem` punc = c `T.cons` capitalize y
                                                                    | otherwise     = x
  where
    punc = "('\"" :: String
capitalizeMsg x = x


punctuateMsg :: T.Text -> T.Text
punctuateMsg = \case "" -> ""
                     x@(T.uncons -> Just (c, "")) | isPunc c  -> x
                                                  | otherwise -> c `T.cons` "."
                     x@(T.last   -> c)            | isPunc c  -> x
                                                  | otherwise -> x <> "."
  where
    isPunc = (`elem` (".?!" :: String))


formatMsgWithTargetArgs :: Args -> (T.Text, T.Text)
formatMsgWithTargetArgs ((capitalize . T.toLower -> target):(formatMsgArgs -> msg)) = (target, msg)
formatMsgWithTargetArgs as = patternMatchFail "formatMsgWithTargetArgs" [ showText as ]
