module FallingObject exposing (FallingObjects, create, move, view)

import Coordinates exposing (Coordinates)
import Css exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msg exposing (Msg(..))
import Murmur3
import Projector
import Random.Pcg as Random exposing (Seed)


type alias FallingObjects a =
    { a
        | fallingObjects : Dict String FallingObject
        , widthRatio : Float
        , heightRatio : Float
        , seed : Seed
        , floorPositionY : Float
    }


type alias FallingObject =
    { width : Float
    , height : Float
    , yieldIntervalInMillisecond : Float
    , speedInPixelPerMillisecond : Float
    , probability : Int
    , state : State
    }


type State
    = Empty EmptyData
    | Falling FallingData


type alias EmptyData =
    { positionX : Float
    , yieldCounterInMillisecond : Float
    }


type alias FallingData =
    { position : Coordinates
    , kind : Kind
    }


type Kind
    = Good
    | Bad


create : Float -> List ( String, FallingObject ) -> List ( String, FallingObject )
create positionX list =
    ( toString <| Murmur3.hashString 2218777484 <| toString list
    , { state = Empty { positionX = positionX, yieldCounterInMillisecond = 0 }
      , width = 20
      , height = 20
      , yieldIntervalInMillisecond = 1000
      , speedInPixelPerMillisecond = 0.1
      , probability = 50
      }
    )
        :: list


view : FallingObjects a -> Html Msg
view model =
    div []
        (model.fallingObjects
            |> Dict.values
            |> List.map (fallingObjectView model)
        )


fallingObjectView : FallingObjects a -> FallingObject -> Html Msg
fallingObjectView model fallingObject =
    div
        [ style <|
            Css.asPairsDEPRECATED [ fallingObjectStyle model fallingObject ]
        ]
        []


fallingObjectStyle : FallingObjects a -> FallingObject -> Style
fallingObjectStyle model fallingObject =
    case fallingObject.state of
        Empty _ ->
            Css.batch [ display none ]

        Falling data ->
            let
                color =
                    case data.kind of
                        Good ->
                            hex "#0F0"

                        Bad ->
                            hex "#F00"
            in
            Css.batch
                [ Projector.width model fallingObject.width
                , Projector.height model fallingObject.height
                , Projector.project model data.position
                , position absolute
                , backgroundColor color
                ]


move : Float -> FallingObjects a -> FallingObjects a
move delta model =
    Dict.foldl
        (moveFallingObject delta)
        model
        model.fallingObjects


moveFallingObject : Float -> String -> FallingObject -> FallingObjects a -> FallingObjects a
moveFallingObject delta key fallingObject model =
    case fallingObject.state of
        Empty emptyData ->
            if emptyData.yieldCounterInMillisecond < fallingObject.yieldIntervalInMillisecond then
                fallingObject
                    |> updateYieldCounter (emptyData.yieldCounterInMillisecond + delta) emptyData
                    |> (\object -> { model | fallingObjects = Dict.insert key object model.fallingObjects })
            else
                generateNewRandomObject delta key fallingObject emptyData model

        Falling _ ->
            { model
                | fallingObjects = Dict.insert key (falling delta model fallingObject) model.fallingObjects
            }


updateYieldCounter : Float -> EmptyData -> FallingObject -> FallingObject
updateYieldCounter counter emptyData fallingObject =
    { fallingObject
        | state =
            Empty
                { emptyData
                    | yieldCounterInMillisecond = counter
                }
    }


generateNewRandomObject : Float -> String -> FallingObject -> EmptyData -> FallingObjects a -> FallingObjects a
generateNewRandomObject delta key fallingObject emptyData model =
    let
        ( randomNumber, newSeed ) =
            Random.step (Random.int 1 100) model.seed

        newFallingObject =
            if randomNumber < fallingObject.probability then
                fallingObject
                    |> falling delta model
            else
                fallingObject
                    |> updateYieldCounter 0 emptyData
    in
    { model
        | seed = newSeed
        , fallingObjects = Dict.insert key newFallingObject model.fallingObjects
    }


falling : Float -> FallingObjects a -> FallingObject -> FallingObject
falling delta model fallingObject =
    case fallingObject.state of
        Empty data ->
            { fallingObject
                | state =
                    Falling
                        { position =
                            { x = data.positionX
                            , y = 1080 + fallingObject.height
                            }
                        , kind = Good
                        }
            }

        Falling data ->
            let
                newPositionY =
                    data.position.y - (fallingObject.speedInPixelPerMillisecond * delta)
            in
            if newPositionY < model.floorPositionY then
                { fallingObject | state = Empty { positionX = data.position.x, yieldCounterInMillisecond = 0 } }
            else
                { fallingObject
                    | state =
                        Falling
                            { data
                                | position =
                                    { x = data.position.x
                                    , y = newPositionY
                                    }
                            }
                }
