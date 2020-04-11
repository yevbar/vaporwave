{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib 
  ( webAppEntry
  ) where

import Servant(serve, Proxy(..), Server, Handler, JSON, Get, Raw, serveDirectoryFileServer, (:>), (:<|>) (..))
import Data.Aeson(ToJSON)
import GHC.Generics(Generic)
import Network.HTTP.Types(status200)
import Network.Wai(Application, responseLBS)
import Network.Wai.Handler.Warp(run)

import Client(method)

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "greet" :> Get '[JSON] Message
          :<|> Raw

data User = User
  { name :: String
  , email :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User

users :: [User]
users =
  [ User "Isaac Newton"     "isaac@newton.co.uk"
  , User "Albert Einstein"  "ae@mc2.org"
  ]

userHandler :: [User]
userHandler = users

data Message = Message
  { message :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Message

m :: Message
m = Message "Hello from Servant"

messageHandler :: Message
messageHandler = m

server :: Server UserAPI
server = return userHandler
    :<|> return messageHandler
    :<|> (serveDirectoryFileServer "static")

catchAll :: Application
catchAll req respond = respond $
  responseLBS
  status200
  [("Content-Type", "text/plain")]
  "Hello world!"

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

webAppEntry :: IO ()
webAppEntry = do
  putStrLn method
  putStrLn "Running on locahost:6868"
  run 6868 app
