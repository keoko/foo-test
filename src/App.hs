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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import System.Random
import System.IO.Unsafe
import Database.Persist.Sqlite


type HandlerResponse a = ExceptT ServantErr IO a

server :: ConnectionPool -> Server Api
server pool = getAllInterviews pool
              :<|> getInterview pool
              :<|> createInterview pool
              :<|> updateInterview pool
              :<|> deleteInterview pool
              :<|> applyForInterview pool

getAllInterviews :: ConnectionPool -> HandlerResponse [Entity Interview]
getAllInterviews pool = liftIO $
  runSqlPersistMPool (selectList [] []) pool

getInterview :: ConnectionPool -> InterviewId -> HandlerResponse InterviewWithQuestions
getInterview pool interviewId = do
  interview <- getInterview' pool interviewId
  questions <- liftIO $ getQuestions pool interviewId
  return $ InterviewWithQuestions (interview' interview) (questions' questions)
  where
    interview' (Entity _ i) = interviewName i
    questions' = map (\(Entity _ q) -> questionContent q)

getInterview' :: ConnectionPool -> InterviewId -> HandlerResponse (Entity Interview)
getInterview' pool interviewId = do
  maybeInterview <- liftIO $ runSqlPersistMPool (selectFirst [InterviewId ==. interviewId] []) pool
  case maybeInterview of
    Nothing ->
      throwError err404
    Just interview ->
      return interview

getQuestions :: ConnectionPool -> InterviewId -> IO [Entity Question]
getQuestions pool interviewId =
  liftIO $ runSqlPersistMPool (selectList [QuestionInterviewId ==. interviewId] []) pool


createInterview :: ConnectionPool -> InterviewWithQuestions -> HandlerResponse NoContent
createInterview pool interviewWithQuestions = do
  -- @todo check that interview does not exist
  liftIO $ runSqlPersistMPool queries pool
  return NoContent
  where queries = do
          interviewId <- insert $ Interview (name interviewWithQuestions)
          insertQuestionQueries interviewId $ questions interviewWithQuestions

updateInterview :: ConnectionPool -> InterviewId -> InterviewWithQuestions -> HandlerResponse NoContent
updateInterview pool interviewId interviewWithQuestions = do
  getInterview' pool interviewId
  liftIO $ runSqlPersistMPool queries pool
  return NoContent
  where queries = do
          deleteWhere [QuestionInterviewId ==. interviewId]
          insertQuestionQueries interviewId $ questions interviewWithQuestions
          let interview' = Interview $ name interviewWithQuestions
          replace interviewId interview'


deleteInterview :: ConnectionPool -> InterviewId -> HandlerResponse NoContent
deleteInterview pool interviewId = do
  getInterview' pool interviewId
  liftIO $ runSqlPersistMPool (delete interviewId) pool
  return NoContent

applyForInterview :: ConnectionPool -> LoginForm -> HandlerResponse NoContent
applyForInterview pool loginForm = do
  getInterview' pool (interviewId loginForm)
  liftIO $ runSqlPersistMPool insertTestQuery pool
  return NoContent
  where insertTestQuery = insert $ FooTest (interviewId loginForm) (userName loginForm) (userEmail loginForm)


-- newCode =
--   take 10 $ randomRs ('a','z') $ unsafePerformIO newStdGen

app :: ConnectionPool -> Application
app pool = serve api $ server pool

startApp :: Int -> ConnectionPool -> IO ()
startApp port pool = run port (logStdoutDev $ app pool)


-- Helper functions
insertQuestionQueries interviewId questions =
  mapM_ (\(p, q) -> insert $ Question interviewId p q) $ zip [1..] questions
