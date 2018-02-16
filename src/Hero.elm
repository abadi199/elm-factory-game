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
import Msg exposing (Msg)


type alias Hero a =
    { a
        | heroPosition : Position
        , heroWidth : Float
        , heroHeight : Float
        , heroSpeedPPms : Float
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
            div
                [ css
                    [ position absolute
                    , top (px y)
                    , left (px x)
                    , Css.width (px hero.heroWidth)
                    , Css.height (px hero.heroHeight)
                    , backgroundColor (hex "#000")
                    ]
                ]
                []
    in
    case hero.heroPosition of
        Stationary coordinates ->
            draw coordinates

        Moving { from, to } ->
            draw from


targetView : Hero a -> Html Msg
targetView hero =
    case hero.heroPosition of
        Stationary _ ->
            text ""

        Moving { to } ->
            div
                [ css
                    [ position absolute
                    , top (px to.y)
                    , left (px to.x)
                    , Css.width (px 10)
                    , Css.height (px 10)
                    , backgroundColor (rgba 0 0 0 0.2)
                    ]
                ]
                []


moveTo : Coordinates -> Hero a -> Hero a
moveTo coordinates hero =
    case hero.heroPosition of
        Stationary from ->
            { hero | heroPosition = Moving { from = from, to = coordinates } }

        Moving { from } ->
            { hero | heroPosition = Moving { from = from, to = coordinates } }


move : Float -> Hero a -> Hero a
move delta hero =
    case hero.heroPosition of
        Stationary _ ->
            hero

        Moving { from, to } ->
            hero |> moving from to delta hero.heroSpeedPPms


moving : Coordinates -> Coordinates -> Float -> Float -> Hero a -> Hero a
moving from to delta speedPPms hero =
    let
        toX =
            to.x - (hero.heroWidth / 2)

        travelDistance =
            if from.x > toX then
                -1 * speedPPms * delta
            else
                speedPPms * delta
    in
    if abs (from.x - toX) <= abs travelDistance then
        { hero | heroPosition = Stationary { x = toX, y = from.y } }
    else
        { hero | heroPosition = Moving { from = from |> Coordinates.addX travelDistance, to = to } }
