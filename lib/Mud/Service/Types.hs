{-# LANGUAGE DataKinds, DeriveGeneric, GeneralizedNewtypeDeriving, TypeOperators #-}

module Mud.Service.Types where

import Mud.Data.State.MudData

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)
import Servant (Capture, FromHttpApiData, Get, JSON, ToHttpApiData, (:<|>)(..), (:>))


type AdminAPI =
         "pla" :> "all"                   :> Get '[JSON] [Object Pla]
    :<|> "pla" :> Capture "id" CaptureInt :> Get '[JSON] (Object Pla)


newtype CaptureInt = CaptureInt { fromCaptureInt :: Int } deriving (FromHttpApiData, ToHttpApiData)


data Object a = Object { objectId :: Id
                       , object   :: a } deriving Generic


instance (ToJSON   a) => ToJSON   (Object a)
instance (FromJSON a) => FromJSON (Object a)
