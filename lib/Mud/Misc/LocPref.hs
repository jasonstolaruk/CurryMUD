{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Misc.LocPref ( hasLocPref
                        , singleArgInvEqRm
                        , sortArgsInvEqRm
                        , stripLocPref ) where

import Mud.Data.Misc
import Mud.TopLvlDefs.Chars
import Mud.Util.Misc (PatternMatchFail)
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (_1, _2, _3)
import Control.Lens.Operators ((%~), (&))
import Data.Text (Text)
import qualified Data.Text as T


patternMatchFail :: PatternMatchFail a
patternMatchFail = U.patternMatchFail "Mud.Misc.LocPref"


-- ==================================================


pattern InvPref      :: Text
pattern EqPref       :: Text
pattern RmPref       :: Text
pattern Rest         :: String
pattern SelectorChar :: Char
pattern AtLst1       :: [a]

pattern InvPref      <- (T.unpack -> ('i':Rest))
pattern EqPref       <- (T.unpack -> ('e':Rest))
pattern RmPref       <- (T.unpack -> ('r':Rest))
pattern Rest         <- (SelectorChar:AtLst1)
pattern SelectorChar <- ((selectorChar `compare`) -> EQ)
pattern AtLst1       <- (_:_)


-----


hasLocPref :: Text -> Bool
hasLocPref = \case InvPref -> True
                   EqPref  -> True
                   RmPref  -> True
                   _       -> False


-----


singleArgInvEqRm :: InInvEqRm -> Text -> (InInvEqRm, Text)
singleArgInvEqRm dflt arg = case sortArgsInvEqRm dflt . pure $ arg of
  ([a], [],  [] ) -> (InInv, a)
  ([],  [a], [] ) -> (InEq,  a)
  ([],  [],  [a]) -> (InRm,  a)
  x               -> patternMatchFail "singleArgInvEqRm" . showText $ x


-----


sortArgsInvEqRm :: InInvEqRm -> Args -> (Args, Args, Args)
sortArgsInvEqRm dflt = foldr f mempty
  where
    f arg acc = case arg of InvPref -> getLens InInv `g` dropPref arg
                            EqPref  -> getLens InEq  `g` dropPref arg
                            RmPref  -> getLens InRm  `g` dropPref arg
                            xs      -> getLens dflt  `g` xs
      where
        getLens = \case InInv -> _1
                        InEq  -> _2
                        InRm  -> _3
        lens `g` rest = acc & lens %~ (rest :)
        dropPref      = T.tail . T.tail


-----


stripLocPref :: Text -> Text
stripLocPref arg = hasLocPref arg ? T.drop 2 arg :? arg
