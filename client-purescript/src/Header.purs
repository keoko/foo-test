module App.Header where

import Pux.Router (link)
import Pux.Html (Html, div, h1, p, text, nav, ul, li)

view _ =
    div [] [ h1 [] [ text "Foo Test" ]
           , navigation ]

navigation =
  nav [] [ ul
           [] 
           [ li [] [ link "/" [] [ text "Home" ] ]
           , li [] [ link "/interview" [] [ text "Create an Interview" ] ]
           ]
         ]
