{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module Api where

import           Data.Proxy
import           Database.Persist (Entity)
import           Models           (Interview, InterviewId)
import           Servant.API      ((:<|>), (:>), Capture, Delete, Get, JSON,
                                   NoContent, Post, Put, ReqBody)
type Api = -- Get '[JSON] Text
  "create" :> Get '[JSON] InterviewId
  -- :<|> "create" :> ReqBody '[FormUrlEncoded] CreateInterviewRequest :> Post '[JSON] (Key Interview)
--  :<|> "check" :> ReqBody '[FormUrlEncoded] CheckRequest :> Post '[HTML] Html
--  :<|> Raw

api :: Proxy Api
api = Proxy
