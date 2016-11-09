{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module Api where

import           Data.Proxy
import           Database.Persist (Entity)
import           Models           (Interview, InterviewId)
import           Servant.API      ((:<|>), (:>), Capture, Delete, Get, JSON,
                                   NoContent, Post, Put, ReqBody)
type Api =
  "interview"
              :> Get '[JSON] [Entity Interview]
  :<|> "interview" :> Capture "id" InterviewId
              :> Get '[JSON] (Maybe (Entity Interview))
  :<|> "interview" :> Capture "id" InterviewId
              :> Delete '[JSON] NoContent


api :: Proxy Api
api = Proxy
