module Model exposing (Model, Window, initialModel)

import Dict
import FallingObject exposing (FallingObjects)
import Hero exposing (Hero)
import Machine exposing (Machines)
import Projector
import Random.Pcg exposing (Seed)
import Window


type alias Model =
    Window (World (Hero (Machines (FallingObjects {}))))


type alias Window a =
    { a
        | widthRatio : Float
        , heightRatio : Float
        , windowSize : Window.Size
    }


type alias World a =
    { a | floorPositionY : Float }


initialModel : Seed -> Window.Size -> Model
initialModel seed windowSize =
    { widthRatio = Projector.widthRatio windowSize
    , heightRatio = Projector.widthRatio windowSize
    , windowSize = windowSize
    , floorPositionY = 200
    , heroPosition = Hero.Stationary { x = 100, y = 200 }
    , heroWidth = 50
    , heroHeight = 100
    , heroSpeedInPixelPerMillisecond = 0.75
    , machines =
        []
            |> Machine.create { x = 500, y = 200 }
            |> Machine.create { x = 1000, y = 200 }
            |> Machine.create { x = 1500, y = 200 }
            |> Dict.fromList
    , fallingObjects =
        []
            |> FallingObject.create 300
            |> FallingObject.create 800
            |> FallingObject.create 1300
            |> Dict.fromList
    , seed = seed
    }
