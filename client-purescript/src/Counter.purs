module App.Counter where

import Data.String (toUpper)
import Prelude ((+), (-), const, show)
import Pux.Html (Html, div, span, button, text, textarea)
import Pux.Html.Events (onInput, FormEvent, onClick)

data Action = Increment | Decrement | TextChange FormEvent

type State = String

init :: State
init = ""

update :: Action -> State -> State
update (TextChange ev) state = toUpper ev.target.value
update _ state = state
--update Increment state = state + 1
--update Decrement state = state - 1

view :: State -> Html Action
view state =
  div
    []
    [ button [ onClick (const Increment) ] [ text "Increment" ]
    , span [] [ text (show state) ]
    ,  div [] [ textarea [ onInput TextChange ] [ text state ] ]
    , button [ onClick (const Decrement) ] [ text "Decrement" ]
    ]
