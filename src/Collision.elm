module Collision
    exposing
        ( coordinatesWithRect
        , rectWithRect
        )

import Coordinates exposing (Coordinates)


type alias Rect a =
    { a | position : Coordinates, width : Float, height : Float }


rectWithRect : Rect a -> Rect b -> Bool
rectWithRect rect1 rect2 =
    (rect1.position.x < rect2.position.x + rect2.width)
        && (rect1.position.x + rect1.width > rect2.position.x)
        && (rect1.position.y < rect2.position.y + rect2.height)
        && (rect1.height + rect1.position.y > rect2.position.y)


coordinatesWithRect : Rect a -> Coordinates -> Bool
coordinatesWithRect rect point =
    (rect.position.x <= point.x)
        && (rect.position.x + rect.width >= point.x)
        && (rect.position.y <= point.y)
        && (rect.position.y + rect.height >= point.y)
