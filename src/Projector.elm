module Projector
    exposing
        ( calculateOrigin
        , height
        , heightRatio
        , project
        , toViewport
        , toViewportX
        , toViewportY
        , toWorldX
        , toWorldY
        , width
        , widthRatio
        )

import Coordinates exposing (Coordinates)
import Css
import Window


toWorldX : { a | widthRatio : Float } -> Float -> Float
toWorldX { widthRatio } x =
    if x == 0 then
        0
    else
        widthRatio * x


toWorldY : { a | heightRatio : Float } -> Float -> Float
toWorldY { heightRatio } y =
    if y == 0 then
        0
    else
        heightRatio * y


toViewportX : { a | origin : Coordinates, widthRatio : Float } -> Float -> Float
toViewportX { origin, widthRatio } x =
    if x == 0 then
        origin.x
    else
        (x / widthRatio) - origin.x


toViewportY : { a | origin : Coordinates, heightRatio : Float } -> Float -> Float
toViewportY { origin, heightRatio } y =
    if y == 0 then
        origin.y
    else
        (y / heightRatio) - origin.y


toViewport : { a | origin : Coordinates, widthRatio : Float, heightRatio : Float } -> Coordinates -> Coordinates
toViewport model { x, y } =
    { x = toViewportX model x
    , y = toViewportY model y
    }


project : { a | origin : Coordinates, widthRatio : Float, heightRatio : Float } -> { x : Float, y : Float } -> Css.Style
project model { x, y } =
    Css.batch
        [ Css.left Css.zero
        , Css.bottom Css.zero
        , Css.transforms
            [ Css.translateY (Css.px (-1 * toWorldY model (y + model.origin.y)))
            , Css.translateX (Css.px (toWorldX model (x + model.origin.x)))
            ]
        ]


width : { a | widthRatio : Float } -> Float -> Css.Style
width model value =
    Css.width (Css.px (toWorldX model value))


height : { a | heightRatio : Float } -> Float -> Css.Style
height model value =
    Css.height (Css.px (toWorldY model value))


viewport : { height : Float, width : Float }
viewport =
    { width = 1920, height = 1080 }


widthRatio : Window.Size -> Float
widthRatio windowSize =
    let
        width =
            if 1.777777778 > toFloat windowSize.width / toFloat windowSize.height then
                toFloat windowSize.width
            else
                toFloat windowSize.height * 1.777777778
    in
    width / viewport.width


heightRatio : Window.Size -> Float
heightRatio windowSize =
    let
        height =
            if 1.777777778 < toFloat windowSize.width / toFloat windowSize.height then
                toFloat windowSize.height
            else
                toFloat windowSize.width / 1.777777778
    in
    height / viewport.height


calculateOrigin : Window.Size -> Coordinates
calculateOrigin windowSize =
    let
        origin =
            { x = 0, y = 0 }
    in
    { x = (toViewportX { origin = origin, widthRatio = widthRatio windowSize } (toFloat windowSize.width) - viewport.width) / 2
    , y = (toViewportY { origin = origin, heightRatio = heightRatio windowSize } (toFloat windowSize.height) - viewport.height) / 2
    }
