module Hero
    exposing
        ( Hero
        , Model
        , Position(..)
        , captureObjects
        , create
        , moveTo
        , update
        , view
        )

import Collision
import Coordinates exposing (Coordinates)
import Css exposing (..)
import Dict exposing (Dict)
import FallingObject exposing (FallingObject)
import Html exposing (..)
import Html.Attributes exposing (..)
import Machine
import Msg exposing (Msg)
import Projector
import Set exposing (Set)
import Task


type alias Model a =
    { a
        | widthRatio : Float
        , heightRatio : Float
        , origin : Coordinates
        , hero : Hero
    }


type alias Hero =
    { position : Position
    , width : Float
    , height : Float
    , speedInPixelPerMillisecond : Float
    , load : List FallingObject.Kind
    , frameCount : Int
    }


type Position
    = Stationary Coordinates Facing
    | Moving { from : Coordinates, to : Coordinates, framePosition : Float }


type Facing
    = Left
    | Right


create : Hero
create =
    { position = Stationary { x = 100, y = 200 } Right
    , width = 175
    , height = 189
    , load = []
    , speedInPixelPerMillisecond = 0.75
    , frameCount = 3
    }


view : Model a -> Html Msg
view model =
    div []
        [ heroView model
        , targetView model
        ]


heroView : Model a -> Html Msg
heroView model =
    let
        draw coordinates framePosition facing =
            div [ style <| Css.asPairsDEPRECATED [ heroStyle coordinates framePosition facing model ] ]
                []
    in
    case model.hero.position of
        Stationary coordinates facing ->
            draw coordinates 0 facing

        Moving { from, to, framePosition } ->
            let
                facing =
                    if from.x + (model.hero.width / 2) >= to.x then
                        Left
                    else
                        Right
            in
            draw from framePosition facing


heroStyle : Coordinates -> Float -> Facing -> Model a -> Style
heroStyle coordinates framePosition facing model =
    let
        scaleX =
            case facing of
                Left ->
                    -1

                Right ->
                    1
    in
    Css.batch
        [ position absolute
        , Projector.projectWithScaleX model coordinates scaleX
        , Projector.width model model.hero.width
        , Projector.height model model.hero.height
        , backgroundImage (url "hero.svg")
        , backgroundSize2 (px (Projector.toWorldX model (model.hero.width * 3))) (px (Projector.toWorldY model model.hero.height))
        , backgroundPosition2 (px (Projector.toWorldX model (model.hero.width * (toFloat <| Basics.round framePosition)))) Css.zero
        ]


targetView : Model a -> Html Msg
targetView model =
    case model.hero.position of
        Stationary _ _ ->
            text ""

        Moving { to } ->
            div [ style <| Css.asPairsDEPRECATED [ targetStyle to model ] ] []


targetStyle : Coordinates -> Model a -> Style
targetStyle { x, y } model =
    Css.batch
        [ position absolute
        , Projector.project model { x = x - 10, y = y - 10 }
        , Projector.width model 20
        , Projector.height model 20
        , backgroundColor (rgba 0 0 0 0.2)
        ]


moveTo : Coordinates -> Model (Machine.Model a) -> Model (Machine.Model a)
moveTo coordinates model =
    let
        machines =
            model

        hero =
            model.hero

        moveToCoordinates =
            case model.hero.position of
                Stationary from facing ->
                    { model | hero = { hero | position = Moving { from = from, to = coordinates, framePosition = 0 } } }

                Moving movingData ->
                    { model | hero = { hero | position = Moving { movingData | to = coordinates } } }

        moveToMachine machine =
            case model.hero.position of
                Stationary from facing ->
                    { model | hero = { hero | position = Moving { from = from, to = machine.position, framePosition = 0 } } }

                Moving movingData ->
                    { model | hero = { hero | position = Moving { movingData | to = machine.position } } }
    in
    machines
        |> Machine.selected
        |> Maybe.map moveToMachine
        |> Maybe.withDefault moveToCoordinates


update : Float -> Model (Machine.Model (FallingObject.Model a)) -> ( Model (Machine.Model (FallingObject.Model a)), Cmd Msg )
update delta model =
    case model.hero.position of
        Stationary _ _ ->
            ( model, Cmd.none )
                |> checkObjectCollision delta

        Moving movingData ->
            model
                |> moving movingData delta model.hero.speedInPixelPerMillisecond
                |> checkObjectCollision delta


checkObjectCollision : Float -> ( Model (Machine.Model (FallingObject.Model a)), Cmd Msg ) -> ( Model (Machine.Model (FallingObject.Model a)), Cmd Msg )
checkObjectCollision delta ( model, cmd ) =
    let
        capturedObjects =
            model.producers
                |> Dict.values
                |> List.concatMap (.objects >> Dict.toList)
                |> List.filterMap (collidesWith model.hero)
                |> Set.fromList

        hero =
            model.hero
    in
    if Set.isEmpty capturedObjects then
        ( model, cmd )
    else
        ( model
        , Cmd.batch
            [ cmd
            , Task.succeed capturedObjects
                |> Task.perform Msg.ObjectCaptured
            ]
        )


collidesWith : Hero -> ( String, FallingObject ) -> Maybe String
collidesWith hero ( key, fallingObject ) =
    let
        heroPosition =
            case hero.position of
                Stationary position _ ->
                    position

                Moving { from } ->
                    from

        heroRect =
            { position = heroPosition, width = hero.width, height = hero.height }
    in
    if Collision.rectWithRect heroRect fallingObject then
        Just key
    else
        Nothing


moving : { from : Coordinates, to : Coordinates, framePosition : Float } -> Float -> Float -> Model (Machine.Model a) -> ( Model (Machine.Model a), Cmd Msg )
moving moveData delta speedPPms model =
    let
        machines =
            model

        toX =
            moveData.to.x - (model.hero.width / 2)

        travelDistance =
            if moveData.from.x > toX then
                -1 * speedPPms * delta
            else
                speedPPms * delta

        hasArrived =
            abs (moveData.from.x - toX) <= abs travelDistance

        updateHeroPosition hero =
            { hero | position = Moving { moveData | from = moveData.from |> Coordinates.addX travelDistance } }

        facing =
            if moveData.from.x > toX then
                Left
            else
                Right

        stopHero hero =
            { hero
                | position =
                    Stationary { x = toX, y = moveData.from.y } facing
            }
    in
    if hasArrived then
        ( { model | hero = model.hero |> stopHero }
        , machines
            |> Machine.selectedMachineId
            |> Maybe.map (Task.succeed >> Task.perform Msg.ResetMachineTimer)
            |> Maybe.withDefault Cmd.none
        )
    else
        ( { model
            | hero =
                model.hero
                    |> updateHeroPosition
                    |> updateFrameCounter delta
          }
        , Cmd.none
        )


updateFrameCounter : Float -> Hero -> Hero
updateFrameCounter delta hero =
    case hero.position of
        Stationary _ _ ->
            hero

        Moving movingData ->
            let
                framePosition =
                    movingData.framePosition
                        + (delta * hero.speedInPixelPerMillisecond / 100)
            in
            { hero
                | position =
                    Moving
                        { movingData
                            | framePosition =
                                if floor framePosition > hero.frameCount - 1 then
                                    0
                                else
                                    framePosition
                        }
            }


captureObjects : Set String -> Model (FallingObject.Model a) -> Model (FallingObject.Model a)
captureObjects objects model =
    let
        hero =
            model.hero

        newLoad =
            FallingObject.getObjects objects model
                |> List.map .kind
    in
    { model | hero = { hero | load = newLoad ++ hero.load } }
