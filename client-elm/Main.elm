module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view model =
    div []
        [ h1 [] [ text model ] ]


main =
    view "hello world"
