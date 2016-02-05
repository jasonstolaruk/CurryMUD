{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.TheWorld.Misc ( commonHooks
                         , commonRmActionFuns
                         , lookTrashHook
                         , putTrashHook
                         , trashRmAction ) where

import Mud.Cmds.Msgs.Advice
import Mud.Cmds.Msgs.Dude
import Mud.Cmds.Msgs.Sorry
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Coins
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Random
import Mud.Misc.LocPref
import Mud.TheWorld.Zones.AdminZoneIds
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logPlaOut)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Concurrent (threadDelay)
import Control.Lens (_1, _2, _3, _4)
import Control.Lens.Operators ((%~), (&), (.~), (<>~))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\), delete, foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


patternMatchFail :: Text -> [Text] -> a
patternMatchFail = U.patternMatchFail "Mud.TheWorld.Misc"


-----


logPlaOut :: Text -> Id -> [Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.TheWorld.Misc"


-- ==================================================
-- Common hooks:


commonHooks :: [(HookName, HookFun)]
commonHooks = [ (lookTrashHookName, lookTrashHookFun)
              , (putTrashHookName,  putTrashHookFun ) ]


-----


lookTrashHook :: Hook
lookTrashHook = Hook lookTrashHookName [ "trash", "bin" ]


lookTrashHookName :: HookName
lookTrashHookName = "(common)_lookTrash"


lookTrashHookFun :: HookFun
lookTrashHookFun i Hook { .. } _ a@(_, (ms, _, _, _)) =
    let selfDesig = mkStdDesig i ms DoCap
    in a &    _1 %~  (\\ triggers)
         & _2._2 <>~ pure trashDesc
         & _2._3 <>~ pure (serialize selfDesig <> " looks at the trash bin.", i `delete` desigIds selfDesig)
         & _2._4 <>~ pure (bracketQuote hookName <> " looked at trash bin")
  where
    -- TODO: "Thank you for keeping xxx clean."
    trashDesc = "The trash bin is an oblong metal container, about 3 feet tall, with a lid connected to the body by \
                \hinges. Affixed to the lid is a bronze plate, on which the following has been neatly etched:\n\
                \\"Magic Trash Bin: you may dispose of unwanted items by \"trash\"ing them here. Items placed in this \
                \bin are magically expunged, and entirely unrecoverable.\"\n\
                \Carefully lifting open the lid and peaking inside, you find only an ominous darkness; not even the \
                \bottom of the bin is visible."


-----


putTrashHook :: Hook
putTrashHook = Hook putTrashHookName  [ "trash", "bin" ]


putTrashHookName :: HookName
putTrashHookName = "(common)_putTrash"


putTrashHookFun :: HookFun
putTrashHookFun i _ _ a@(as, (ms, _, _, _)) = a & _2 .~ trashHelper i ms as


trashHelper :: Id -> MudState -> Args -> GenericIntermediateRes
trashHelper i ms as =
    let (inInvs, inEqs, inRms) = sortArgsInvEqRm InInv as
        sorryInEq              = inEqs |!| sorryTrashInEq
        sorryInRm              = inRms |!| sorryTrashInRm
        invCoins               = getInvCoins i ms
        d                      = mkStdDesig  i ms DoCap
        (eiss, ecs)            = uncurry (resolveMobInvCoins i ms inInvs) invCoins
        (ms', toSelfs, bs)     = foldl' (helperTrashEitherInv   i d) (ms,  [],      []         ) eiss
        gir                    =         helperTrashEitherCoins i d  (ms', toSelfs, bs, toSelfs) ecs
    in (belchHelper gir &) $ if ()# invCoins
      then _2 .~ pure dudeYourHandsAreEmpty
      else _2 %~ (dropBlanks [ sorryInEq, sorryInRm ] ++)
  where
    belchHelper gir@(_, _, _, logMsgs) = onTrue (()!# logMsgs) (_1.opList <>~ pure op) gir
    op = rndmDo 10 $ let msg = "The lid of the trash bin momentarily opens of its own accord as a loud belch is \
                               \emitted from inside the container."
                     in do
                         liftIO . threadDelay $ 3 * 10 ^ 6
                         getState >>= \ms' -> bcastNl . pure $ (msg, findMobIds ms' . getMobRmInv i $ ms')



-- ==================================================
-- Common room action functions:


commonRmActionFuns :: [(RmActionFunName, RmActionFun)]
commonRmActionFuns = pure (trashRmActionFunName, trash)


-----


trashRmAction :: RmAction
trashRmAction = RmAction "trash" trashRmActionFunName


trashRmActionFunName :: RmActionFunName
trashRmActionFunName = "(common)_trash"


trash :: RmActionFun
trash p@AdviseNoArgs          = advise p [] adviceTrashNoArgs
trash (LowerNub i mq cols as) = helper |&| modifyState >=> \(toSelfs, bs, logMsgs) -> do
    multiWrapSend mq cols toSelfs
    bcastIfNotIncogNl i bs
    logMsgs |#| logPlaOut "trash" i
  where
    helper ms = let (ms', toSelfs, bs, logMsgs) = trashHelper i ms as in (ms', (toSelfs, bs, logMsgs))
trash p = patternMatchFail "trash" [ showText p ]


helperTrashEitherInv :: Id
                    -> Desig
                    -> (MudState, [Text], [Broadcast])
                    -> Either Text Inv
                    -> (MudState, [Text], [Broadcast])
helperTrashEitherInv i d a@(ms, _, _) = \case
  Left  msg -> a & _2 <>~ pure msg
  Right is  -> let (toSelfs, bs) = mkTrashInvDescs i ms d is
               in a & _1.invTbl.ind i          %~  (\\ is)
                    & _1.invTbl.ind iTrashDump %~  (sortInv ms . (++ is))
                    & _2                       <>~ toSelfs
                    & _3                       <>~ bs


mkTrashInvDescs :: Id -> MudState -> Desig -> Inv -> ([Text], [Broadcast])
mkTrashInvDescs i ms d (mkNameCountBothList i ms -> ncbs) = unzip . map helper $ ncbs
  where
    helper (_, c, (s, _)) | c == 1 =
        ("You place the " <> s <> rest, (T.concat [ serialize d, " places ", aOrAn s, rest ], otherIds))
    helper (_, c, b) =
        ("You place " <> rest', (serialize d <> " places " <> rest', otherIds))
      where
        rest' = T.concat [ showText c, " ", mkPlurFromBoth b, rest ]
    rest     = " into the trash bin."
    otherIds = i `delete` desigIds d


helperTrashEitherCoins :: Id
                       -> Desig
                       -> GenericIntermediateRes
                       -> [Either [Text] Coins]
                       -> GenericIntermediateRes
helperTrashEitherCoins i d (ms, toSelfs, bs, logMsgs) ecs =
    let (ms', toSelfs', logMsgs', c) = foldl' helper (ms, toSelfs, logMsgs, mempty) ecs
    in (ms', toSelfs', bs ++ mkTrashCoinsDescOthers i d c, logMsgs')
  where
    helper a = \case
      Left  msgs -> a & _2 <>~ msgs
      Right c    -> let toSelfs' = mkTrashCoinsDescsSelf c
                    in a & _1.coinsTbl.ind i          %~  (<> negateCoins c)
                         & _1.coinsTbl.ind iTrashDump %~  (<>             c)
                         & _2                         <>~ toSelfs'
                         & _3                         <>~ toSelfs'
                         & _4                         <>~ c


mkTrashCoinsDescOthers :: Id -> Desig -> Coins -> [Broadcast]
mkTrashCoinsDescOthers i d c =
  c |!| [ (T.concat [ serialize d, " deposits ", aCoinSomeCoins c, " into the trash bin." ], i `delete` desigIds d) ]


mkTrashCoinsDescsSelf :: Coins -> [Text]
mkTrashCoinsDescsSelf = mkCoinsMsgs helper
  where
    helper 1 cn = T.concat [ "You deposit ", aOrAn cn,             " into the trash bin." ]
    helper a cn = T.concat [ "You deposit ", showText a, " ", cn, "s into the trash bin." ]
