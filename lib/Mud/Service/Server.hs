{-# LANGUAGE OverloadedStrings #-}

module Mud.Service.Server where

import Mud.Service.Types

import Data.Text (Text)
import Servant (Handler, Proxy(..), Server, (:<|>)(..))


adminAPI :: Proxy AdminAPI
adminAPI = Proxy


-- To test: "curl http://localhost:8081/hello".
server :: Server AdminAPI
server = getHello
    :<|> getHellos
  where
    getHello :: Handler Text
    getHello = return "hello"

    getHellos :: Handler [Text]
    getHellos = return [ "hello1", "hello2" ]
