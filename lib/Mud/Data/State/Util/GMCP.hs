{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.GMCP where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.TheWorld.Zones.ZoneRmMap
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Lens (both, each)
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
    rest = T.concat [ dblQuote "hp"    <> colon
                    , hpCurr           <> comma
                    , dblQuote "maxhp" <> colon
                    , hpMax            <> comma
                    ----------
                    , dblQuote "mp"    <> colon
                    , mpCurr           <> comma
                    , dblQuote "maxmp" <> colon
                    , mpMax            <> comma
                    ----------
                    , dblQuote "pp"    <> colon
                    , ppCurr           <> comma
                    , dblQuote "maxpp" <> colon
                    , ppMax            <> comma
                    ----------
                    , dblQuote "fp"    <> colon
                    , fpCurr           <> comma
                    , dblQuote "maxfp" <> colon
                    , fpMax ]
    ((hpCurr, hpMax), (mpCurr, mpMax), (ppCurr, ppMax), (fpCurr, fpMax)) = f
    f = getPts i ms & each %~ (both %~ (dblQuote . showText))


gmcpRoomInfo :: Id -> MudState -> Text
gmcpRoomInfo i ms = "Room.Info " <> curlyQuote (spaced rest)
  where
    rest = T.concat [ dblQuote "num"       <> colon
                    , "1234"               <> comma
                    , dblQuote "room name" <> colon
                    , dblQuote name        <> comma
                    , dblQuote "room area" <> colon
                    , dblQuote area        <> comma
                    , dblQuote "exits"     <> colon
                    , exits ]
    rm    = getMobRm i ms
    name  = rm^.rmName
    area  = getZoneNameForRmId i
    exits = curlyQuote . spaced $ t
      where
        t = "\"n\": 1234, \"se\": 5678"
