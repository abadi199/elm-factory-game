module FallingObject
    exposing
        ( FallingObjects
        , create
        , update
        , view
        )

import Coordinates exposing (Coordinates)
import Css exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msg exposing (Msg(..))
import Murmur3
import Projector
import Random.Pcg as Random exposing (Seed)
import Window


type alias FallingObjects a =
    { a
        | producers : Dict String Producer
        , widthRatio : Float
        , heightRatio : Float
        , seed : Seed
        , floorPositionY : Float
        , windowSize : Window.Size
    }


type alias Producer =
    { width : Float
    , height : Float
    , yieldIntervalInMillisecond : Float
    , speedInPixelPerMillisecond : Float
    , yieldProbability : Int
    , goodKindProbability : Int
    , positionX : Float
    , yieldCounterInMillisecond : Float
    , objects : List FallingObject
    }


type alias FallingObject =
    { position : Coordinates
    , kind : Kind
    }


type Kind
    = Good
    | Bad


create : Float -> List ( String, Producer ) -> List ( String, Producer )
create positionX list =
    ( toString <| Murmur3.hashString 2218777484 <| toString list
    , { objects = []
      , positionX = positionX
      , yieldCounterInMillisecond = 0
      , width = 20
      , height = 20
      , yieldIntervalInMillisecond = 5000
      , speedInPixelPerMillisecond = 0.1
      , yieldProbability = 50
      , goodKindProbability = 90
      }
    )
        :: list


view : FallingObjects a -> Html Msg
view model =
    div []
        (model.producers
            |> Dict.values
            |> List.map (producerView model)
        )


producerView : FallingObjects a -> Producer -> Html Msg
producerView model producer =
    div
        []
        (div [ style <| Css.asPairsDEPRECATED [ producerStyle model producer ] ]
            [ div [] [ text <| toString <| Basics.round <| producer.yieldCounterInMillisecond / 1000 ]
            , div [] [ text <| toString <| List.length producer.objects ]
            ]
            :: (producer.objects
                    |> List.map (fallingObjectView model producer)
               )
        )


producerStyle : FallingObjects a -> Producer -> Style
producerStyle model producer =
    Css.batch
        [ position absolute
        , backgroundColor (rgba 0 0 0 0.25)
        , Projector.width model 100
        , Projector.height model (Projector.toViewportY model (toFloat model.windowSize.height) - model.floorPositionY)
        , Projector.project model { x = producer.positionX, y = model.floorPositionY }
        ]


fallingObjectView : FallingObjects a -> Producer -> FallingObject -> Html Msg
fallingObjectView model producer fallingObject =
    div
        [ style <|
            Css.asPairsDEPRECATED [ fallingObjectStyle model producer fallingObject ]
        ]
        []


fallingObjectStyle : FallingObjects a -> Producer -> FallingObject -> Style
fallingObjectStyle model producer fallingObject =
    let
        color =
            case fallingObject.kind of
                Good ->
                    hex "#0F0"

                Bad ->
                    hex "#F00"
    in
    Css.batch
        [ Projector.width model producer.width
        , Projector.height model producer.height
        , Projector.project model fallingObject.position
        , position absolute
        , backgroundColor color
        ]


update : Float -> FallingObjects a -> FallingObjects a
update delta model =
    Dict.foldl
        (\key producer -> producer |> move delta model |> updateProducer delta key)
        model
        model.producers


move : Float -> FallingObjects a -> Producer -> Producer
move delta model producer =
    { producer | objects = List.filterMap (moveObject delta model producer) producer.objects }


moveObject : Float -> FallingObjects a -> Producer -> FallingObject -> Maybe FallingObject
moveObject delta model producer fallingObject =
    let
        position =
            fallingObject.position

        newPositionY =
            position.y - (producer.speedInPixelPerMillisecond * delta)
    in
    if newPositionY < model.floorPositionY then
        Nothing
    else
        Just
            { fallingObject
                | position = { position | y = newPositionY }
            }


updateProducer : Float -> String -> Producer -> FallingObjects a -> FallingObjects a
updateProducer delta key producer model =
    if producer.yieldCounterInMillisecond < producer.yieldIntervalInMillisecond then
        { model | producers = Dict.insert key (addYieldCounter delta producer) model.producers }
    else
        generateNewRandomObject delta key producer model


addYieldCounter : Float -> Producer -> Producer
addYieldCounter delta producer =
    { producer | yieldCounterInMillisecond = producer.yieldCounterInMillisecond + delta }


generateNewRandomObject : Float -> String -> Producer -> FallingObjects a -> FallingObjects a
generateNewRandomObject delta key producer model =
    let
        ( randomNumber, newSeed ) =
            Random.step (Random.int 1 100) model.seed

        newProducer =
            if randomNumber < producer.yieldProbability then
                { producer
                    | objects = newObject model producer :: producer.objects
                    , yieldCounterInMillisecond = 0
                }
            else
                { producer | yieldCounterInMillisecond = 0 }
    in
    { model
        | seed = newSeed
        , producers = Dict.insert key newProducer model.producers
    }


newObject : FallingObjects a -> Producer -> FallingObject
newObject model producer =
    { position = { x = producer.positionX, y = Projector.toViewportY model (toFloat model.windowSize.height) }
    , kind = Bad
    }
