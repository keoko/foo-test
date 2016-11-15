module App.Routes where

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Prelude (($))
import Pux.Router (end, router, lit)

data Route = Home | Interview | NotFound

match :: String -> Route
-- match url = fromMaybe NotFound $ router url $
--  Home <$ end
match url = fromMaybe NotFound $ router url $ interview <|> home
  where
    interview = Interview <$ (lit "interview") <* end
    home = Home <$ end
