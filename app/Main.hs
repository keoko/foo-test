{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Proxy
import Data.Text
import Network.Wai.Handler.Warp
import Servant

type API = Get '[JSON] Text
  :<|> "code" :> Raw
--type API = "code" :> Raw

apiHandler :: Server API
apiHandler = return "hello world!!!"
  :<|> serveDirectory "tutorial"
--apiHandler = serveDirectory "tutorial"

proxy :: Proxy API
proxy = Proxy

main :: IO ()
main = run 8080 $ serve proxy apiHandler
