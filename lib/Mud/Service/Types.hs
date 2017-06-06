{-# LANGUAGE DataKinds, TypeOperators #-}

module Mud.Service.Types where

import Data.Text (Text)
import Servant (Get, JSON, (:<|>)(..), (:>))


type AdminAPI =
         "hello"  :> Get '[JSON] Text
    :<|> "hellos" :> Get '[JSON] [Text]
