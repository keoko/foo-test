module Main where

import Lib (startApp)

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Running in http://localhost:" ++ show port ++ "/"
  startApp port
