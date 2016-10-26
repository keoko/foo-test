{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Proxy
import Data.Text
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 (p, Html)


type API = Get '[JSON] Text
  :<|> "create" :> Get '[HTML] Html
  :<|> Raw

apiHandler :: Server API
apiHandler = return "hello world!!!"
  :<|> createPageHandler
  :<|> serveDirectory "web"

createPageHandler = return page
  where page :: Html
        page = p "hello"

proxy :: Proxy API
proxy = Proxy

main :: IO ()
main = run 8080 $ serve proxy apiHandler
