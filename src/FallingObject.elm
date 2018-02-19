module FallingObject exposing (FallingObjects, create, move, view)

import Coordinates exposing (Coordinates)
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
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
    }


type alias FallingObject =
    { width : Float
    , height : Float
    , speedInPixelPerMillisecond : Float
    , probability : Int
    , state : State
    }


type State
    = Empty EmptyData
    | Falling FallingData


type alias EmptyData =
    { positionX : Float
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
    , { state = Empty { positionX = positionX }
      , width = 20
      , height = 20
      , speedInPixelPerMillisecond = 4
      , probability = 5
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
        [ css
            [ fallingObjectStyle model fallingObject ]
        ]
        []


fallingObjectStyle : FallingObjects a -> FallingObject -> Style
fallingObjectStyle model fallingObject =
    case fallingObject.state of
        Empty _ ->
            Css.batch [ display none ]

        Falling data ->
            Css.batch
                [ backgroundColor (hex "#F0F")
                , Projector.left model data.position.x
                , Projector.bottom model data.position.y
                , Projector.width model fallingObject.width
                , Projector.height model fallingObject.height
                , position absolute
                ]


move : Float -> FallingObjects a -> FallingObjects a
move delta model =
    let
        ( newSeed, newFallingObjects ) =
            Dict.foldl
                (moveFallingObject delta)
                ( model.seed, Dict.empty )
                model.fallingObjects
    in
    { model
        | fallingObjects = newFallingObjects
        , seed = newSeed
    }


moveFallingObject : Float -> String -> FallingObject -> ( Seed, Dict String FallingObject ) -> ( Seed, Dict String FallingObject )
moveFallingObject delta key fallingObject ( seed, dict ) =
    case fallingObject.state of
        Empty _ ->
            let
                ( randomNumber, newSeed ) =
                    Random.step (Random.int 1 100) seed

                newFallingObject =
                    if randomNumber < fallingObject.probability then
                        falling delta fallingObject
                    else
                        fallingObject
            in
            ( newSeed, Dict.insert key newFallingObject dict )

        Falling _ ->
            ( seed, Dict.insert key (falling delta fallingObject) dict )


falling : Float -> FallingObject -> FallingObject
falling delta fallingObject =
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
            { fallingObject
                | state =
                    Falling
                        { data
                            | position =
                                { x = data.position.x
                                , y = data.position.y - fallingObject.speedInPixelPerMillisecond
                                }
                        }
            }
