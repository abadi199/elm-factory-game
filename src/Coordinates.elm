module Coordinates
    exposing
        ( Coordinates
        , addX
        , addY
        , collidesWithRect
        , fromPosition
        )

import Mouse
import Projector
import Window


type alias Coordinates =
    { x : Float, y : Float }


fromPosition : { a | widthRatio : Float, heightRatio : Float, windowSize : Window.Size } -> Mouse.Position -> Coordinates
fromPosition model { x, y } =
    { x =
        x
            |> toFloat
            |> Projector.toViewportX model
    , y =
        model.windowSize.height
            - y
            |> toFloat
            |> Projector.toViewportY model
    }


addX : Float -> Coordinates -> Coordinates
addX x coordinates =
    { coordinates | x = coordinates.x + x }


addY : Float -> Coordinates -> Coordinates
addY y coordinates =
    { coordinates | y = coordinates.y + y }


collidesWithRect : { a | position : Coordinates, width : Float, height : Float } -> Coordinates -> Bool
collidesWithRect rect point =
    (rect.position.x <= point.x)
        && (rect.position.x + rect.width >= point.x)
        && (rect.position.y <= point.y)
        && (rect.position.y + rect.height >= point.y)
