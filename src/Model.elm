module Model
    exposing
        ( Model
        , Window
        , initialModel
        , isGameOver
        )

import Coordinates exposing (Coordinates)
import Dict
import FallingObject
import Hero
import Machine
import Projector
import Random.Pcg exposing (Seed)
import Window


type alias Model =
    Window (World (Hero.Model (Machine.Model (FallingObject.Model {}))))


type alias Window a =
    { a
        | widthRatio : Float
        , heightRatio : Float
        , origin : Coordinates
        , windowSize : Window.Size
    }


type alias World a =
    { a
        | floorPositionY : Float
        , ceilingPositionY : Float
        , timestamp : Float
    }


initialModel : Seed -> Window.Size -> Model
initialModel seed windowSize =
    { widthRatio = Projector.widthRatio windowSize
    , heightRatio = Projector.heightRatio windowSize
    , origin = Projector.calculateOrigin windowSize
    , windowSize = windowSize
    , floorPositionY = 200
    , ceilingPositionY = 1050
    , hero = Hero.create
    , machines =
        []
            |> Machine.create { x = 500, y = 200 }
            |> Machine.create { x = 1000, y = 200 }
            |> Machine.create { x = 1500, y = 200 }
            |> Dict.fromList
    , producers =
        []
            |> FallingObject.create 300
            |> FallingObject.create 800
            |> FallingObject.create 1300
            |> Dict.fromList
    , seed = seed
    , timestamp = 0
    }


isGameOver : Model -> Bool
isGameOver model =
    if model.machines |> Dict.values |> List.all Machine.isKaboom then
        True
    else
        False
