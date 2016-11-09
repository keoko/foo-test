{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}

module App
 ( startApp, migrateAll
 ) where

import Api (Api, api)
import Models
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Control.Monad.IO.Class
import System.Random
import System.IO.Unsafe
import Database.Persist.Sqlite



server :: ConnectionPool -> Server Api
server pool = getAllInterviews pool :<|> getInterview pool :<|> updateInterview pool :<|> deleteInterview pool

getAllInterviews pool = liftIO $
  runSqlPersistMPool (selectList [] []) pool

getInterview pool interviewId = liftIO $
  runSqlPersistMPool (selectFirst [InterviewId ==. interviewId] []) pool

updateInterview pool interviewId interview = liftIO $ do
  runSqlPersistMPool (replace interviewId interview) pool
  return NoContent

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
