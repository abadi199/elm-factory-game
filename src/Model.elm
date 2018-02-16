module Model exposing (Model, Window, initialModel)

import Hero exposing (Hero)
import Window


type alias Model =
    Window (Hero {})


type alias Window a =
    { a
        | windowSize : Window.Size
    }


initialModel : Window.Size -> Model
initialModel windowSize =
    { windowSize = windowSize
    , heroPosition = Hero.Stationary { x = 100, y = 200 }
    , heroWidth = 50
    , heroHeight = 100
    , heroSpeedPPms = 0.75
    }
