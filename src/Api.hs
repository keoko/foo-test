{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module Api where

import           Data.Proxy
import           Database.Persist (Entity)
import           Models           (Interview, InterviewId)
import           Servant.API      ((:<|>), (:>), Capture, Delete, Get, JSON,
                                   NoContent, Post, Put, ReqBody)
type Api =
  "interview" :> Capture "id" InterviewId
              :> Get '[JSON] (Maybe (Entity Interview))

api :: Proxy Api
api = Proxy
