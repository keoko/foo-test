{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module Api where

import           Data.Proxy
import           Database.Persist (Entity)
import           Models
import           Servant.API      ((:<|>), (:>), Capture, Delete, Get, JSON,
                                   NoContent, Post, Put, ReqBody)
type Api =
  "interview"
              :> Get '[JSON] [Entity Interview]
  :<|> "interview" :> Capture "id" InterviewId
              :> Get '[JSON] InterviewWithQuestions
  :<|> "interview"
              :> ReqBody '[JSON] InterviewWithQuestions
              :> Post '[JSON] NoContent
  :<|> "interview" :> Capture "id" InterviewId
              :> ReqBody '[JSON] InterviewWithQuestions
              :> Put '[JSON] NoContent
  :<|> "interview" :> Capture "id" InterviewId
              :> Delete '[JSON] NoContent
  :<|> "foo-test" :> ReqBody '[JSON] LoginForm
               :> Post '[JSON] NoContent
  -- :<|> "apply" :> Capture "id" ApplicationId
  --              :> ReqBody '[JSON] ApplicationForm
  --              :> Put '[JSON] NoContent

api :: Proxy Api
api = Proxy
