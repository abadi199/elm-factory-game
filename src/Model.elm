module Model exposing (Model, initialModel)

import Window


type alias Model =
    { windowSize : Window.Size }


initialModel : Window.Size -> Model
initialModel windowSize =
    { windowSize = windowSize }
