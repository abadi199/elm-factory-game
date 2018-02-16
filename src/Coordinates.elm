module Coordinates
    exposing
        ( Coordinates
        , addX
        , addY
        , fromPosition
        )

import Mouse


type alias Coordinates =
    { x : Float, y : Float }


fromPosition : Mouse.Position -> Coordinates
fromPosition { x, y } =
    { x = toFloat x, y = toFloat y }


addX : Float -> Coordinates -> Coordinates
addX x coordinates =
    { coordinates | x = coordinates.x + x }


addY : Float -> Coordinates -> Coordinates
addY y coordinates =
    { coordinates | y = coordinates.y + y }
