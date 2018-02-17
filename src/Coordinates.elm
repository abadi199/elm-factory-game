module Coordinates
    exposing
        ( Coordinates
        , addX
        , addY
        , fromPosition
        )

import Mouse
import Projector
import Window


type alias Coordinates =
    { x : Float, y : Float }


fromPosition : { a | windowSize : Window.Size } -> Mouse.Position -> Coordinates
fromPosition { windowSize } { x, y } =
    { x =
        x
            |> toFloat
            |> Projector.toViewportX windowSize
    , y =
        y
            |> toFloat
            |> Projector.toViewportY windowSize
    }


addX : Float -> Coordinates -> Coordinates
addX x coordinates =
    { coordinates | x = coordinates.x + x }


addY : Float -> Coordinates -> Coordinates
addY y coordinates =
    { coordinates | y = coordinates.y + y }
