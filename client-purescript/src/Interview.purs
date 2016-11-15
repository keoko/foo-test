module App.Interview where


import Pux.Html (Html, (#), div, h2, text, textarea)

view :: forall state action. state -> Html action
view state =
  div []
    [ h2 [] [ text "Create an Interview" ]
    , text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    , questionsBlock state ]

questionsBlock state =
  div [] [text "questions"]
