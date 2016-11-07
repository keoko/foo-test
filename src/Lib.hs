{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib 
 ( startApp
 ) where

import Data.Proxy
import Data.Text hiding (head, map, filter, null, tail)
import Data.ByteString.Lazy as BS (ByteString, readFile)
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

-- data CheckRequest = CheckRequest { code :: Text }
-- data CheckResult = Correct | Wrong

-- instance FromFormUrlEncoded CheckRequest where
--   --fromFormUrlEncoded :: [(Text, Text)] -> Either String CheckRequest
--   fromFormUrlEncoded [("code", c)] = Right (CheckRequest c)
--   fromFormUrlEncoded _             = Left "expected a single field `code`"

data CreateInterviewRequest = CreateInterviewRequest
  { questions :: [String]
  , initialForm :: Int
  , maxNumForms :: Int
  , minNumForms :: Int
  , totalForms :: Int
  }


decodeInterviewQuestions :: [(Text, Text)]-> [String]
decodeInterviewQuestions xs = getQuestionSentences . sortByIndex . filterQuestions $ map extractQuestionNumber xs
  where
    getQuestionSentences = map snd
    sortByIndex = sortOn (\(x,_) -> read ((head x) !! 1) :: Int)
    filterQuestions = filter (not . null . fst)
    extractQuestionNumber (k, v) = ((((unpack k) :: String) =~ ("question-([0-9]+)-content" :: String)) :: [[String]], (unpack v)) 


instance FromFormUrlEncoded CreateInterviewRequest where
  fromFormUrlEncoded inputs =
    Right $ CreateInterviewRequest questions (lkp "question-INITIAL_FORMS") (lkp "question-MAX_NUM_FORMS") (lkp "question-MIN_NUM_FORMS") (lkp "question-TOTAL_FORMS")

    where lkp input_label = case lookup input_label inputs of
                 Nothing -> 0
                 Just v    -> read (unpack v) :: Int
          questions = decodeInterviewQuestions inputs

-- data Login = LoggedIn | NotLoggedIn
--   deriving (Eq, Show)

-- instance ToJSON Login where
--   toJSON = toJSON . show

-- data User = User
--   { email :: Text
--   , password :: Text
--   } deriving (Eq, Show)

-- instance FromFormUrlEncoded User where
--   fromFormUrlEncoded inputs =
--     User <$> lkp "email" <*> lkp "password"

--     where lkp input_label = case lookup input_label inputs of
--                  Nothing -> Left $ unpack $ "label " <> input_label <> " not found"
--                  Just v    -> Right v

-- checkCode (CheckRequest code) =
--   if code == "secret"
--   then return $ p "correct"
--   else return $ p "wrong"


-- serveUser usr =
--   (if email usr == "admin@admin.com" && password usr == "1234"
--    then return $ p "logged in!!!!!"
--    else return $ p "not logged in!!!!")


-- -- type API = "login" :> ReqBody '[FormUrlEncoded] User :> Post '[HTML] Html
-- --   :<|> "check" :> ReqBody '[FormUrlEncoded] CheckRequest :> Post '[HTML] Html
-- --   :<|> Raw


-- -- server :: Server API
-- -- server =
-- --   serveUser
-- --   :<|> checkCode
-- --   :<|> serveDirectory "web"

-- newtype RawHtml = RawHtml { unRaw :: ByteString }

-- -- tell Servant how to render the newtype to html page, in this case simply unwrap it
-- instance MimeRender HTML RawHtml where
--     mimeRender _ =  unRaw

-- returnFile fileName =
--     fmap RawHtml (liftIO $  BS.readFile fileName)

type API = Get '[JSON] Text
  :<|> "create" :> Get '[HTML] Html
  :<|> "create" :> ReqBody '[FormUrlEncoded] CreateInterviewRequest :> Post '[HTML] Html
--  :<|> "check" :> ReqBody '[FormUrlEncoded] CheckRequest :> Post '[HTML] Html
  :<|> Raw


server :: Server API
server = return "hello world!!!"
   :<|> createPageHandler
   :<|> createPostHandler
 --  :<|> checkCode
   :<|> serveDirectory "web"


createPageHandler = return page
  where page :: Html
        page = p "hello"

-- createPostHandler _ = return page
--   where page :: Html
--         page = p "create post handler"

-- createPostHandler =
--   returnFile "web/dashboard.html"

-- createPostHandler =
--   throwError (err301 { errHeaders = [("Location", "/dashboard.html")]})

createPostHandler createInterviewRequest = return page
  where page :: Html
        page = p $ toHtml $ head $ tail $ questions createInterviewRequest

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

startApp :: Int -> IO ()
startApp port = run port (logStdoutDev app)
