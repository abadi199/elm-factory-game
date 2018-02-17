module Model exposing (Model, Window, initialModel)

import Dict
import Hero exposing (Hero)
import Machine exposing (Machines)
import Projector
import Window


type alias Model =
    Window (Hero (Machines {}))


type alias Window a =
    { a
        | widthRatio : Float
        , heightRatio : Float
        , windowSize : Window.Size
    }


initialModel : Window.Size -> Model
initialModel windowSize =
    { widthRatio = Projector.widthRatio windowSize
    , heightRatio = Projector.widthRatio windowSize
    , windowSize = windowSize
    , heroPosition = Hero.Stationary { x = 100, y = 200 }
    , heroWidth = 50
    , heroHeight = 100
    , heroSpeedInPixelPerMillisecond = 0.75
    , machines =
        Dict.fromList
            [ ( "A", Machine.create { x = 500, y = 200 } )
            , ( "B", Machine.create { x = 1000, y = 200 } )
            , ( "C", Machine.create { x = 1500, y = 200 } )
            ]
    }
