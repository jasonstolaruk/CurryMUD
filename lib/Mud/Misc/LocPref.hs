{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Misc.LocPref ( hasLocPref
                        , singleArgInvEqRm
                        , sortArgsInvEqRm
                        , stripLocPref ) where

import           Mud.Data.Misc
import           Mud.TopLvlDefs.Chars
import           Mud.Util.Misc (PatternMatchFail)
import qualified Mud.Util.Misc as U (patternMatchFail)
import           Mud.Util.Operators
import           Mud.Util.Text

import           Control.Lens (_1, _2, _3)
import           Control.Lens.Operators ((&), (%~))
import           Data.Text (Text)
import qualified Data.Text as T


patternMatchFail :: (Show a) => PatternMatchFail a b
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
    f arg acc = case arg of InvPref -> getLens InInv 🍮 dropPref arg
                            EqPref  -> getLens InEq  🍮 dropPref arg
                            RmPref  -> getLens InRm  🍮 dropPref arg
                            xs      -> getLens dflt  🍮 xs
      where
        getLens = \case InInv -> _1
                        InEq  -> _2
                        InRm  -> _3
        lens 🍮 rest = acc & lens %~ (rest :)
        dropPref = T.tail . T.tail


-----


stripLocPref :: Text -> Text
stripLocPref arg = hasLocPref arg ? T.drop 2 arg :? arg
