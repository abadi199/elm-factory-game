module FallingObject
    exposing
        ( FallingObject
        , Kind(..)
        , Model
        , Producer
        , create
        , getObjects
        , removeObjects
        , update
        , view
        )

import Coordinates exposing (Coordinates)
import Css exposing (..)
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (..)
import Html.Attributes exposing (..)
import Msg exposing (Msg(..))
import Murmur3
import Projector
import Random.Pcg as Random exposing (Seed)
import Set exposing (Set)
import Window


type alias Model a =
    { a
        | producers : Dict String Producer
        , widthRatio : Float
        , heightRatio : Float
        , origin : Coordinates
        , seed : Seed
        , floorPositionY : Float
        , ceilingPositionY : Float
        , windowSize : Window.Size
        , timestamp : Float
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
    , objects : Dict String FallingObject
    }


type alias FallingObject =
    { position : Coordinates
    , kind : Kind
    , width : Float
    , height : Float
    }


type Kind
    = Good
    | Bad


create : Float -> List ( String, Producer ) -> List ( String, Producer )
create positionX list =
    ( toString <| Murmur3.hashString 2218777484 <| toString list
    , { objects = Dict.fromList []
      , positionX = positionX
      , yieldCounterInMillisecond = 0
      , width = 50
      , height = 50
      , yieldIntervalInMillisecond = 5000
      , speedInPixelPerMillisecond = 0.1
      , yieldProbability = 50
      , goodKindProbability = 90
      }
    )
        :: list


view : Model a -> Html Msg
view model =
    div []
        (model.producers
            |> Dict.values
            |> List.map (producerView model)
        )


producerView : Model a -> Producer -> Html Msg
producerView model producer =
    div
        []
        (div [ style <| Css.asPairsDEPRECATED [ producerStyle model producer ] ]
            [ div [] [ text <| toString <| Basics.round <| producer.yieldCounterInMillisecond / 1000 ]
            , div [] [ text <| toString <| Dict.size producer.objects ]
            ]
            :: (producer.objects
                    |> Dict.values
                    |> List.map (fallingObjectView model producer)
               )
        )


producerStyle : Model a -> Producer -> Style
producerStyle model producer =
    Css.batch
        [ position absolute
        , backgroundColor (rgba 0 0 0 0.25)
        , Projector.width model producer.width
        , Projector.height model (model.ceilingPositionY - model.floorPositionY)
        , Projector.project model { x = producer.positionX, y = model.floorPositionY }
        ]


fallingObjectView : Model a -> Producer -> FallingObject -> Html Msg
fallingObjectView model producer fallingObject =
    div
        [ style <|
            Css.asPairsDEPRECATED [ fallingObjectStyle model producer fallingObject ]
        ]
        []


fallingObjectStyle : Model a -> Producer -> FallingObject -> Style
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


update : Float -> Model a -> Model a
update delta model =
    Dict.foldl
        (\key producer updatedModel -> ( key, producer ) |> move delta updatedModel |> updateProducer delta updatedModel)
        model
        model.producers


move : Float -> Model a -> ( String, Producer ) -> ( String, Producer )
move delta model ( producerKey, producer ) =
    ( producerKey
    , { producer | objects = Dict.Extra.filterMap (moveObject delta model producer) producer.objects }
    )


moveObject : Float -> Model a -> Producer -> String -> FallingObject -> Maybe FallingObject
moveObject delta model producer key fallingObject =
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


updateProducer : Float -> Model a -> ( String, Producer ) -> Model a
updateProducer delta model ( key, producer ) =
    if producer.yieldCounterInMillisecond < producer.yieldIntervalInMillisecond then
        { model | producers = Dict.insert key (addYieldCounter delta producer) model.producers }
    else
        generateNewRandomObject delta key producer model


addYieldCounter : Float -> Producer -> Producer
addYieldCounter delta producer =
    { producer | yieldCounterInMillisecond = producer.yieldCounterInMillisecond + delta }


generateNewRandomObject : Float -> String -> Producer -> Model a -> Model a
generateNewRandomObject delta key producer model =
    let
        yieldObjectGenerator =
            Random.int 1 100
                |> Random.andThen yieldObject

        yieldObject randomNumber =
            if randomNumber < producer.yieldProbability then
                objectKindGenerator
                    |> Random.map
                        (\kind ->
                            { producer
                                | objects = newObject model key producer kind producer.objects
                                , yieldCounterInMillisecond = 0
                            }
                        )
            else
                Random.constant { producer | yieldCounterInMillisecond = 0 }

        objectKindGenerator =
            Random.int 1 100
                |> Random.map objectKind

        objectKind randomNumber =
            if Basics.round (toFloat randomNumber * (100 / toFloat producer.yieldProbability)) < producer.goodKindProbability then
                Good
            else
                Bad

        ( newProducer, newSeed ) =
            Random.step yieldObjectGenerator model.seed
    in
    { model
        | seed = newSeed
        , producers = Dict.insert key newProducer model.producers
    }


newObject : Model a -> String -> Producer -> Kind -> Dict String FallingObject -> Dict String FallingObject
newObject model producerKey producer kind objects =
    let
        objectKey =
            producerKey ++ toString model.timestamp

        object =
            { position = { x = producer.positionX, y = model.ceilingPositionY }
            , kind = kind
            , width = producer.width
            , height = producer.height
            }
    in
    objects
        |> Dict.insert objectKey object


removeObjects : Set String -> Model a -> Model a
removeObjects objects model =
    { model | producers = Dict.map (removeObjectsFromProducer objects) model.producers }


removeObjectsFromProducer : Set String -> String -> Producer -> Producer
removeObjectsFromProducer objects _ producer =
    { producer | objects = Dict.filter (\key _ -> not <| Set.member key objects) producer.objects }


getObjects : Set String -> Model a -> List FallingObject
getObjects objects model =
    model.producers
        |> Dict.map (getObjectsFromProducer objects)
        |> Dict.values
        |> List.concatMap identity


getObjectsFromProducer : Set String -> String -> Producer -> List FallingObject
getObjectsFromProducer objects _ producer =
    producer.objects
        |> Dict.Extra.keepOnly objects
        |> Dict.values
