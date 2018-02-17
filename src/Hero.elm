module Hero
    exposing
        ( Hero
        , Position(..)
        , move
        , moveTo
        , view
        )

import Coordinates exposing (Coordinates)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Machine exposing (Machines)
import Msg exposing (Msg)
import Projector
import Task


type alias Hero a =
    { a
        | heroPosition : Position
        , heroWidth : Float
        , heroHeight : Float
        , heroSpeedInPixelPerMillisecond : Float
        , widthRatio : Float
        , heightRatio : Float
    }


type Position
    = Stationary Coordinates
    | Moving { from : Coordinates, to : Coordinates }


view : Hero a -> Html Msg
view hero =
    div []
        [ heroView hero
        , targetView hero
        ]


heroView : Hero a -> Html Msg
heroView hero =
    let
        draw { x, y } =
            div [ css [ heroStyle x y hero ] ] []
    in
    case hero.heroPosition of
        Stationary coordinates ->
            draw coordinates

        Moving { from, to } ->
            draw from


heroStyle : Float -> Float -> Hero a -> Style
heroStyle x y hero =
    Css.batch
        [ position absolute
        , Projector.bottom hero y
        , Projector.left hero x
        , Projector.width hero hero.heroWidth
        , Projector.height hero hero.heroHeight
        , backgroundColor (hex "#000")
        ]


targetView : Hero a -> Html Msg
targetView hero =
    case hero.heroPosition of
        Stationary _ ->
            text ""

        Moving { to } ->
            div [ css [ targetStyle to hero ] ] []


targetStyle : Coordinates -> Hero a -> Style
targetStyle { x, y } hero =
    Css.batch
        [ position absolute
        , Projector.bottom hero (y - 10)
        , Projector.left hero (x - 10)
        , Projector.width hero 20
        , Projector.height hero 20
        , backgroundColor (rgba 0 0 0 0.2)
        ]


moveTo : Coordinates -> Hero (Machines a) -> Hero (Machines a)
moveTo coordinates hero =
    let
        machines =
            hero

        moveToCoordinates =
            case hero.heroPosition of
                Stationary from ->
                    { hero | heroPosition = Moving { from = from, to = coordinates } }

                Moving { from } ->
                    { hero | heroPosition = Moving { from = from, to = coordinates } }

        moveToMachine machine =
            case hero.heroPosition of
                Stationary from ->
                    { hero | heroPosition = Moving { from = from, to = machine.position } }

                Moving { from } ->
                    { hero | heroPosition = Moving { from = from, to = machine.position } }
    in
    machines
        |> Machine.selected
        |> Maybe.map moveToMachine
        |> Maybe.withDefault moveToCoordinates


move : Float -> Hero (Machines a) -> ( Hero (Machines a), Cmd Msg )
move delta hero =
    case hero.heroPosition of
        Stationary _ ->
            ( hero, Cmd.none )

        Moving { from, to } ->
            hero |> moving from to delta hero.heroSpeedInPixelPerMillisecond


moving : Coordinates -> Coordinates -> Float -> Float -> Hero (Machines a) -> ( Hero (Machines a), Cmd Msg )
moving from to delta speedPPms hero =
    let
        machines =
            hero

        toX =
            to.x - (hero.heroWidth / 2)

        travelDistance =
            if from.x > toX then
                -1 * speedPPms * delta
            else
                speedPPms * delta

        hasArrived =
            abs (from.x - toX) <= abs travelDistance
    in
    if hasArrived then
        ( { hero | heroPosition = Stationary { x = toX, y = from.y } }
        , machines
            |> Machine.selectedMachineId
            |> Maybe.map (Task.succeed >> Task.perform Msg.ResetMachineTimer)
            |> Maybe.withDefault Cmd.none
        )
    else
        ( { hero | heroPosition = Moving { from = from |> Coordinates.addX travelDistance, to = to } }, Cmd.none )
