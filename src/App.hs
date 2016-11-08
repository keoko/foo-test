{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module App
 ( startApp, migrateAll
 ) where

import Api (Api, api)
import Models
import Data.Proxy
import Data.Text as T (Text, unpack, pack)
import Data.ByteString as BS (pack, unpack, ByteString)
-- import Data.ByteString.Lazy as BS (ByteString, readFile, pack, unpack)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 (p, Html, toHtml)
import Control.Applicative
import Data.Aeson
import Data.Monoid
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Text.Regex.PCRE
import Data.List (sortOn)
import Database.Persist
import Database.Persist.Sql
import Data.Aeson
import System.Random
import System.IO.Unsafe

import Database.Persist.TH
import Database.Persist.Sqlite



server :: ConnectionPool -> Server Api
server pool = getInterview pool :<|> deleteInterview pool

getInterview pool interviewId = liftIO $
  runSqlPersistMPool (selectFirst [InterviewId ==. interviewId] []) pool

deleteInterview pool interviewId = liftIO $ do
  runSqlPersistMPool (delete interviewId) pool
  return NoContent

-- createPostHandler =
--   throwError (err301 { errHeaders = [("Location", "/dashboard.html")]})

newCode =
  take 10 $ randomRs ('a','z') $ unsafePerformIO newStdGen

app :: ConnectionPool -> Application
app pool = serve api $ server pool

startApp :: Int -> ConnectionPool -> IO ()
startApp port pool = run port (logStdoutDev $ app pool)
