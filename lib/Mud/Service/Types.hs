{-# LANGUAGE DataKinds, DeriveGeneric, GeneralizedNewtypeDeriving, TypeOperators #-}

module Mud.Service.Types where

import Mud.Data.State.MudData

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (Capture, FromHttpApiData, Get, Header, Headers, JSON, NoContent, PostNoContent, Raw, ReqBody, ToHttpApiData, (:<|>)(..), (:>))
import Servant.Auth.Server (SetCookie)


data Object a = Object { objectId :: Id
                       , object   :: a } deriving Generic


instance (ToJSON   a) => ToJSON   (Object a)
instance (FromJSON a) => FromJSON (Object a)


type AdminAPI = -- TODO: Rename?
       "pla" :> "all"                   :> Get '[JSON] [Object Pla]
  :<|> "pla" :> Capture "id" CaptureInt :> Get '[JSON] (Object Pla)


newtype CaptureInt = CaptureInt { fromCaptureInt :: Int } deriving (FromHttpApiData, ToHttpApiData)


-----


{-
TODO: Transition to Protected/Unprotected scheme.
https://github.com/mchaver/servant-auth-and-elm-example/blob/master/src/Server.hs
https://github.com/plow-technologies/servant-auth
-}
type Protected =
       "hello" :> Get '[JSON] Text
  :<|> "user"  :> Get '[JSON] (Object Login)


data Login = Login { username :: Text
                   , password :: Text } deriving Generic


type Unprotected =
       "login" :> ReqBody       '[JSON] Login
               :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                  , Header "Set-Cookie" SetCookie ] NoContent)
  :<|> Raw
