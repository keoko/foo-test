module App.Layout where

import App.Counter as Counter
import App.Header as Header
import App.NotFound as NotFound
import App.Interview as Interview
import App.Routes (Route(Home, NotFound, Interview))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text, nav, ul, li)

data Action
  = Child (Counter.Action)
  | PageView Route

type State =
  { route :: Route
  , count :: Counter.State }

init :: State
init =
  { route: NotFound
  , count: Counter.init }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Child action) state = state { count = Counter.update action state.count }

view :: State -> Html Action
view state =
  div
    []
    [ Header.view state
    , case state.route of
        Home -> map Child $ Counter.view state.count
        -- Home -> p [] [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." ]

        NotFound -> NotFound.view state
        Interview -> Interview.view state
    ]

