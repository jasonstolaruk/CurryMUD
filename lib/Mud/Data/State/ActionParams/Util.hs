{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Data.State.ActionParams.Util ( Args
                                        , formatMsgArgs
                                        , formatMsgWithTargetArgs ) where

import Mud.Util.Misc hiding (patternMatchFail)
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
formatMsgArgs as = capitalizeMsg . punctuateMsg . T.intercalate " " $ as


capitalizeMsg :: T.Text -> T.Text
capitalizeMsg x@(T.uncons         -> Just (_, "")) = T.toUpper  x
capitalizeMsg   (T.break isLetter ->      ("", x)) = capitalize x
capitalizeMsg   (T.break isLetter ->      (x, "")) = x
capitalizeMsg x@(T.break isLetter -> (T.uncons -> Just (c, ""), y)) | c `elem` "('\"" = c `T.cons` capitalize y
                                                                    | otherwise       = x
capitalizeMsg x = patternMatchFail "capitalizeMsg" [x]


punctuateMsg :: T.Text -> T.Text
punctuateMsg x@(T.uncons -> Just (c, "")) | c `elem` ".?!" = x
                                          | otherwise      = c `T.cons` "."
punctuateMsg x@(T.last   -> c)            | c `elem` ".?!" = x
                                          | otherwise      = x <> "."


formatMsgWithTargetArgs :: Args -> (T.Text, T.Text)
formatMsgWithTargetArgs ((capitalize . T.toLower -> target):(formatMsgArgs -> msg)) = (target, msg)
formatMsgWithTargetArgs as = patternMatchFail "formatMsgWithTargetArgs" [ showText as ]
