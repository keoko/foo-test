{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Proxy
import Data.Text
import Network.Wai.Handler.Warp
import Servant
import System.Environment

type API = Get '[JSON] Text

apiHandler :: Server API
apiHandler = return "hello world"

proxy :: Proxy API
proxy = Proxy

main :: IO ()
main = run 8080 $ serve proxy apiHandler
