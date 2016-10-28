{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( startApp
  ) where

import Data.Proxy
import Data.Text
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 (p, Html)
import Control.Applicative
import Data.Aeson
import Data.Monoid

data CheckRequest = CheckRequest { code :: Text }
data CheckResult = Correct | Wrong

checkCode (CheckRequest code) =
  if code == "secret"
  then return $ p "correct"
  else return $ p "wrong"


type API = "login" :> ReqBody '[FormUrlEncoded] User :> Post '[HTML] Html
  :<|> "check" :> ReqBody '[FormUrlEncoded] CheckRequest :> Post '[HTML] Html
  :<|> Raw

data Login = LoggedIn | NotLoggedIn
  deriving (Eq, Show)

instance ToJSON Login where
  toJSON = toJSON . show

data User = User
  { email :: Text
  , password :: Text
  } deriving (Eq, Show)

instance FromFormUrlEncoded User where
  fromFormUrlEncoded inputs =
    User <$> lkp "email" <*> lkp "password"

    where lkp input_label = case lookup input_label inputs of
                 Nothing -> Left $ unpack $ "label " <> input_label <> " not found"
                 Just v    -> Right v

server :: Server API
server =
  serveUser
  :<|> checkCode
  :<|> serveDirectory "web"


serveUser usr =
  (if email usr == "admin@admin.com" && password usr == "1234"
   then return $ p "logged in!!!!!"
   else return $ p "not logged in!!!!")



instance FromFormUrlEncoded CheckRequest where
  --fromFormUrlEncoded :: [(Text, Text)] -> Either String CheckRequest
  fromFormUrlEncoded [("code", c)] = Right (CheckRequest c)
  fromFormUrlEncoded _             = Left "expected a single field `code`"

-- type API = Get '[JSON] Text
--   :<|> "create" :> Get '[HTML] Html
--   :<|> "create" :> Post '[HTML] Html
--   :<|> "check" :> ReqBody '[FormUrlEncoded] CheckRequest :> Post '[HTML] Html
--   :<|> Raw

-- server :: Server API
-- server = return "hello world!!!"
--   :<|> createPageHandler
--   :<|> createPostHandler
--   :<|> checkCode
--   :<|> serveDirectory "web"


-- createPageHandler = return page
--   where page :: Html
--         page = p "hello"

-- createPostHandler = return page
--   where page :: Html
--         page = p "create post handler"

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

startApp :: Int -> IO ()
startApp port = run port (logStdoutDev app)
