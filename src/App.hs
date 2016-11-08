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
server pool = getInterview pool



getInterview pool interviewId = liftIO $
  runSqlPersistMPool (selectFirst [InterviewId ==. interviewId] []) pool

  --runSqlPersistMPool (insert $ User "pepe" 10) pool

-- createPostHandler _ = return page
--   where page :: Html
--         page = p "create post handler"

-- createPostHandler =
--   returnFile "web/dashboard.html"

-- createPostHandler =
--   throwError (err301 { errHeaders = [("Location", "/dashboard.html")]})

newCode =
  take 10 $ randomRs ('a','z') $ unsafePerformIO newStdGen

-- todo
-- 1. create new interview
-- 2. redirect to the new interview
-- createPostHandler pool createInterviewRequest = liftIO createInterview
  -- --throwError (err301 { errHeaders = [("Location", "/dashboard.html")]})
  -- where page :: Html
        -- page = p $ toHtml $ head $ tail $ questions createInterviewRequest
        -- createInterview :: IO (Key Interview)
        -- createInterview = flip runSqlPersistMPool pool $ do
          -- let interviewCode = newCode
          -- interview <- insert $ Interview $ T.pack interviewCode
          -- insertMany $ map (\(interviewId,position,content) -> Question interviewId position (T.pack content)) $ zip3 (repeat interview) [1..] (questions createInterviewRequest)
          -- return interview

app :: ConnectionPool -> Application
app pool = serve api $ server pool

startApp :: Int -> ConnectionPool -> IO ()
startApp port pool = run port (logStdoutDev $ app pool)
