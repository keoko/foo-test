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
|]

data InterviewWithQuestions = InterviewWithQuestions
  { name :: String
  , questions :: [String]
  } deriving (Eq, Show, Generic)

instance ToJSON InterviewWithQuestions
