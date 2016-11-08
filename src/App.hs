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
    extractQuestionNumber (k, v) = ((((T.unpack k) :: String) =~ ("question-([0-9]+)-content" :: String)) :: [[String]], (T.unpack v)) 


instance FromFormUrlEncoded CreateInterviewRequest where
  fromFormUrlEncoded inputs =
    Right $ CreateInterviewRequest questions (lkp "question-INITIAL_FORMS") (lkp "question-MAX_NUM_FORMS") (lkp "question-MIN_NUM_FORMS") (lkp "question-TOTAL_FORMS")

    where lkp input_label = case lookup input_label inputs of
                 Nothing -> 0
                 Just v    -> read (T.unpack v) :: Int
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


server :: ConnectionPool -> Server Api
server pool = -- return "hello world!!!"
   createPageHandler pool
--   :<|> createPostHandler pool
 --  :<|> checkCode
 --  :<|> serveDirectory "web"


createPageHandler pool = liftIO $ interviewAdd
  where
    interviewAdd :: IO (Key Interview)
    interviewAdd = flip runSqlPersistMPool pool $ do
      insert $ Interview "pepe"

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
