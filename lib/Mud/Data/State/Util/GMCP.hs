{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.GMCP where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.TheWorld.Zones.ZoneMap
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Lens (both, each, views)
import Control.Lens.Operators ((%~), (&), (^.))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


colon, comma :: Text
colon = ": "
comma = ", "


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
                    , dblQuote "last_room_id" <> colon
                    , showText lastId         <> comma
                    , mkDir ]
    ri                       = getRmId i ms
    zoneName                 = getZoneForRmId ri
    rm                       = getRm ri ms
    roomName                 = rm^.rmName
    (xCoord, yCoord, zCoord) = rm^.rmCoords
    lastId                   = getLastRmId i ms
    mkDir                    = views rmLinks dirHelper . getRm lastId $ ms
      where
        dirHelper links =
            let f (StdLink    dir destId _    ) | destId == ri = pure . T.concat $ [ dblQuote "dir"
                                                                                   , colon
                                                                                   , dblQuote . linkDirToCmdName $ dir ]
                f (NonStdLink n   destId _ _ _) | destId == ri = pure . T.concat $ [ dblQuote "special_dir"
                                                                                   , colon
                                                                                   , dblQuote n ]
                f _                                            = []
            in case concatMap f links of (x:_) -> x
                                         []    -> T.concat [ dblQuote "teleport"
                                                           , colon
                                                           , "true" ]
