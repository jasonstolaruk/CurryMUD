{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.GMCP where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Util.Text
import Mud.Util.Quoting

import Control.Lens (both, each)
import Control.Lens.Operators ((%~), (&))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


colon, comma :: Text
colon = ": "
comma = ", "


gmcpVitals :: Id -> MudState -> Text
gmcpVitals i ms = T.concat [ "Char.Vitals { "
                           , dblQuote "hp"    <> colon
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
                           , fpMax            <> " }" ]
  where
    ((hpCurr, hpMax), (mpCurr, mpMax), (ppCurr, ppMax), (fpCurr, fpMax)) = f
    f = getPts i ms & each %~ (both %~ (dblQuote . showText))


gmcpRoomInfo :: Id -> MudState -> Text
gmcpRoomInfo _ _  = T.concat [ "Room.Info { "
                             , dblQuote "num"       <> colon
                             , "1234"               <> comma
                             , dblQuote "room name" <> colon
                             , dblQuote name        <> comma
                             , dblQuote "room area" <> colon
                             , dblQuote area        <> comma
                             , dblQuote "exits"     <> colon
                             , exits <> " }" ]
  where
    name  = "name"
    area  = "area"
    exits = quoteWith' ("{ ", " }") t
      where
        t = "\"n\": 1234, \"se\": 5678"
