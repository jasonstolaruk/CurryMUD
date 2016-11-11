{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.GMCP where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.TheWorld.Zones.ZoneRmMap
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


gmcpRmInfo :: Id -> MudState -> Text
gmcpRmInfo i ms = "Room.Info " <> curlyQuote (spaced rest)
  where
    rest = T.concat [ dblQuote "num"       <> colon
                    , showText ri          <> comma
                    , dblQuote "room name" <> colon
                    , dblQuote name        <> comma
                    , dblQuote "room area" <> colon
                    , dblQuote area        <> comma
                    , dblQuote "exits"     <> colon
                    , exits ]
    ri    = getRmId i  ms
    rm    = getRm   ri ms
    name  = rm^.rmName
    area  = getZoneNameForRmId ri
    exits = curlyQuote . spaced . views rmLinks (commas . map mkExitTxt) $ rm
      where
        mkExitTxt (StdLink    dir destId _    ) = f (linkDirToCmdName dir) destId
        mkExitTxt (NonStdLink n   destId _ _ _) = f n                      destId
        f n destId                              = dblQuote n <> ": " <> showText destId
