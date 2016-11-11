{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Data.Aeson

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Interview json
  name String
  UniqueName name
  deriving Eq Read Show
Question json
  interviewId InterviewId
  position Int
  content String
  deriving Eq Read Show
FooTest json
  interviewId InterviewId
  name String
  email String
  deriving Eq Read Show
Answer json
  testId FooTestId
  position Int
  content String
  deriving Eq Read Show
|]

data InterviewWithQuestions = InterviewWithQuestions
  { name :: String
  , questions :: [String]
  } deriving (Eq, Show, Generic)

instance ToJSON InterviewWithQuestions
instance FromJSON InterviewWithQuestions


data LoginForm = LoginForm
  { userName :: String
  , userEmail :: String
  , interviewId :: InterviewId
  } deriving (Eq, Show, Generic)

instance ToJSON LoginForm
instance FromJSON LoginForm


data TestResponseForm = TestResponseForm
  { fooTestId :: FooTestId
  , answers :: [String]
  } deriving (Eq, Show, Generic)

instance ToJSON TestResponseForm
instance FromJSON TestResponseForm
