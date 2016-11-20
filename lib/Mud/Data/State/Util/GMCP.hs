{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.GMCP ( gmcpRmInfo
                                , gmcpVitals
                                , gmcpZoom ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.TheWorld.Zones.ZoneMap
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (both, each, views)
import Control.Lens.Operators ((%~), (&), (^.))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


default (Int)


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util.GMCP"


-- ==================================================


colon, comma :: Text
colon = ": "
comma = ", "


-----


gmcpRmInfo :: Id -> MudState -> Text
gmcpRmInfo i ms = "Room.Info " <> curlyQuote (spaced rest)
  where
    rest = T.concat [ dblQuote "area_name"    <> colon
                    , dblQuote zoneName       <> comma
                    , dblQuote "room_id"      <> colon
                    , showText ri             <> comma
                    , dblQuote "room_name"    <> colon
                    , dblQuote roomName       <> comma
                    , dblQuote "x_coord"      <> colon
                    , showText xCoord         <> comma
                    , dblQuote "y_coord"      <> colon
                    , showText yCoord         <> comma
                    , dblQuote "z_coord"      <> colon
                    , showText zCoord         <> comma
                    , dblQuote "room_env"     <> colon
                    , env                     <> comma
                    , dblQuote "room_label"   <> colon
                    , label                   <> comma
                    , dblQuote "last_room_id" <> colon
                    , showText lastId         <> comma
                    , mkDir ]
    ri                       = getRmId i ms
    zoneName                 = getZoneForRmId ri
    rm                       = getRm ri ms
    roomName                 = rm^.rmName
    (xCoord, yCoord, zCoord) = rm^.rmCoords
    env                      = views rmEnv   (showText . envToColorInt) rm
    label                    = views rmLabel (dblQuote . fromMaybeEmp ) rm
    lastId                   = getLastRmId i ms
    mkDir                    = views rmLinks dirHelper . getRm lastId $ ms
      where
        dirHelper links =
            let f (StdLink    dir destId _    ) | destId == ri = mkStdDir . linkDirToCmdName $ dir
                f (NonStdLink n   destId _ _ _) | destId == ri =
                    case n of "u"   -> g
                              "d"   -> g
                              "in"  -> g
                              "out" -> g
                              _     -> pure . T.concat $ [ dblQuote "dir",         colon, "-1", comma
                                                         , dblQuote "special_dir", colon, dblQuote n ]
                  where
                    g = mkStdDir n
                f _ = []
                mkStdDir t = pure . T.concat $ [ dblQuote "dir",         colon, showText dirInt, comma
                                               , dblQuote "special_dir", colon, dblQuote "-1" ]
                  where
                    dirInt = case filter ((== t) . fst) dirs of
                      [pair] -> snd pair
                      _      -> patternMatchFail "gmcpRmInfo mkDir dirHelper mkStdDir dirInt" t
                    dirs = zip [ "n", "ne", "nw", "e", "w", "s", "se", "sw", "u", "d", "in", "out" ] [1..]
            in case concatMap f links of (x:_) -> x
                                         []    -> T.concat [ dblQuote "dir",         colon, "-1", comma
                                                           , dblQuote "special_dir", colon, dblQuote "-1" ]


-- Numbers correspond to Mudlet's user-adjustable mapper colors.
envToColorInt :: RmEnv -> Int
envToColorInt InsideEnv  = 268 -- Light blue.
envToColorInt OutsideEnv = 266 -- Light green.
envToColorInt ShopEnv    = 265 -- Light red.
envToColorInt SpecialEnv = 270 -- Light cyan.
envToColorInt NoEnv      = 264 -- Light black.


-----


gmcpVitals :: Id -> MudState -> Text
gmcpVitals i ms = "Char.Vitals " <> curlyQuote (spaced rest)
  where
    rest = T.concat [ dblQuote "curr_hp" <> colon
                    , hpCurr             <> comma
                    , dblQuote "max_hp"  <> colon
                    , hpMax              <> comma
                    ----------
                    , dblQuote "curr_mp" <> colon
                    , mpCurr             <> comma
                    , dblQuote "max_mp"  <> colon
                    , mpMax              <> comma
                    ----------
                    , dblQuote "curr_pp" <> colon
                    , ppCurr             <> comma
                    , dblQuote "max_pp"  <> colon
                    , ppMax              <> comma
                    ----------
                    , dblQuote "curr_fp" <> colon
                    , fpCurr             <> comma
                    , dblQuote "max_fp"  <> colon
                    , fpMax ]
    ((hpCurr, hpMax), (mpCurr, mpMax), (ppCurr, ppMax), (fpCurr, fpMax)) = f
    f = getPts i ms & each %~ (both %~ (dblQuote . showText))


-----


gmcpZoom :: Int -> Text
gmcpZoom zoom = "Zoom " <> curlyQuote (spaced rest)
  where
    rest = T.concat [ dblQuote "zoom"
                    , colon
                    , showText zoom ]
