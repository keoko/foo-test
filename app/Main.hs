module Main where

import Lib (startApp, migrateAll)
import           Control.Monad.Logger (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Data.String.Conversions


main :: IO ()
main = do
  let port = 8080
  let sqliteFile = "sqlite.db"
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  putStrLn $ "Running in http://localhost:" ++ show port ++ "/"
  startApp port pool
