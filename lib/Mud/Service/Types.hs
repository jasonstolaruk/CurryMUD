{-# LANGUAGE DataKinds, DeriveGeneric, GeneralizedNewtypeDeriving, TypeOperators #-}

module Mud.Service.Types where

import Mud.Data.State.MudData
import Mud.Misc.Database

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (Capture, FromHttpApiData, Get, Header, Headers, JSON, NoContent, PostNoContent, Raw, ReqBody, ToHttpApiData, (:<|>)(..), (:>))
import Servant.Auth.Server (Auth, FromJWT, SetCookie, ToJWT)


type API auths = (Auth auths Login :> Protected) :<|> Unprotected


data Login = Login { username :: Text
                   , password :: Text } deriving Generic


instance FromJSON Login
instance ToJSON   Login
instance FromJWT  Login
instance ToJWT    Login


type Protected =
       "pla" :> "all"                   :> Get '[JSON] [Object Pla]
  :<|> "pla" :> Capture "id" CaptureInt :> Get '[JSON] (Object Pla)
  :<|> "db"  :> "alertexecrec" :> "all" :> Get '[JSON] [AlertExecRec]


data Object a = Object { objectId :: Id
                       , object   :: a } deriving Generic


instance (ToJSON   a) => ToJSON   (Object a)
instance (FromJSON a) => FromJSON (Object a)


newtype CaptureInt = CaptureInt { fromCaptureInt :: Int } deriving (FromHttpApiData, ToHttpApiData)


type Unprotected =
       "login" :> ReqBody       '[JSON] Login
               :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                  , Header "Set-Cookie" SetCookie ] NoContent)
  :<|> Raw
