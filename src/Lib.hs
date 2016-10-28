{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp
  ) where

import Data.Proxy
import Data.Text
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 (p, Html)


type API = Get '[JSON] Text
  :<|> "create" :> Get '[HTML] Html
  :<|> "create" :> Post '[HTML] Html
  :<|> Raw

server :: Server API
server = return "hello world!!!"
  :<|> createPageHandler
  :<|> createPostHandler
  :<|> serveDirectory "web"

createPageHandler = return page
  where page :: Html
        page = p "hello"

createPostHandler = return page
  where page :: Html
        page = p "create post handler"

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

startApp :: Int -> IO ()
startApp port = run port (logStdoutDev app)
