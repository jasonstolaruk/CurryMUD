{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.GMCP where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.TheWorld.Zones.ZoneMap
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Lens (both, each, to, views)
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
    rest = T.concat [ dblQuote "hp"     <> colon
                    , hpCurr            <> comma
                    , dblQuote "max_hp" <> colon
                    , hpMax             <> comma
                    ----------
                    , dblQuote "mp"     <> colon
                    , mpCurr            <> comma
                    , dblQuote "max_mp" <> colon
                    , mpMax             <> comma
                    ----------
                    , dblQuote "pp"     <> colon
                    , ppCurr            <> comma
                    , dblQuote "max_pp" <> colon
                    , ppMax             <> comma
                    ----------
                    , dblQuote "fp"     <> colon
                    , fpCurr            <> comma
                    , dblQuote "max_fp" <> colon
                    , fpMax ]
    ((hpCurr, hpMax), (mpCurr, mpMax), (ppCurr, ppMax), (fpCurr, fpMax)) = f
    f = getPts i ms & each %~ (both %~ (dblQuote . showText))


gmcpRmInfo :: Id -> MudState -> Text
gmcpRmInfo i ms = "Room.Info " <> curlyQuote (spaced rest)
  where
    rest = T.concat [ dblQuote "zone_id"    <> colon
                    , showText zoneId       <> comma
                    , dblQuote "zone_name"  <> colon
                    , dblQuote zoneName     <> comma
                    , dblQuote "room_id"    <> colon
                    , showText ri           <> comma
                    , dblQuote "room_name"  <> colon
                    , dblQuote roomName     <> comma
                    , dblQuote "x_coord"    <> colon
                    , dblQuote xCoord       <> comma
                    , dblQuote "y_coord"    <> colon
                    , dblQuote yCoord       <> comma
                    , dblQuote "z_coord"    <> colon
                    , dblQuote zCoord       <> comma
                    , dblQuote "room_exits" <> colon
                    , exits ]
    ri                       = getRmId i ms
    (zoneId, zoneName)       = getZoneForRmId ri
    rm                       = getRm ri ms
    roomName                 = rm^.rmName
    (xCoord, yCoord, zCoord) = rm^.rmCoords.to (each %~ showText)
    exits                    = curlyQuote . spaced . views rmLinks (commas . map mkExitTxt) $ rm
      where
        mkExitTxt (StdLink    dir destId _    ) = f (linkDirToCmdName dir) destId
        mkExitTxt (NonStdLink n   destId _ _ _) = f n                      destId
        f n destId                              = dblQuote n <> ": " <> showText destId
