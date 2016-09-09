{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T

import Lucid
import Data.Text.Lazy (toStrict)

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root helloSpock -- $ text "Hello World!"
       get ("hello" <//> var) $ \name ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))

-- helloSpock :: SpockAction database session state ()
-- helloSpock = html "Hello, <em>Spock!</em>"

helloSpockHTML :: Html ()
helloSpockHTML = html_ (do head_  (title_ "Hello!")
                           body_ (p_ "Hello Spock!!!"))

helloSpock :: SpockAction database session state ()
helloSpock = lucid helloSpockHTML

lucid :: Html () -> SpockAction database session state ()
lucid document = html (toStrict (renderText document))
