module Projector
    exposing
        ( bottom
        , height
        , heightRatio
        , left
        , project
        , toViewportX
        , toViewportY
        , toWorldX
        , toWorldY
        , width
        , widthRatio
        )

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


toViewportX : { a | widthRatio : Float } -> Float -> Float
toViewportX { widthRatio } x =
    if x == 0 then
        0
    else
        x / widthRatio


toViewportY : { a | heightRatio : Float } -> Float -> Float
toViewportY { heightRatio } y =
    if y == 0 then
        0
    else
        y / heightRatio


left : { a | widthRatio : Float } -> Float -> Css.Style
left model value =
    Css.batch
        [ Css.left Css.zero
        , Css.transform <| Css.translateX (Css.px (toWorldX model value))
        ]


bottom : { a | heightRatio : Float } -> Float -> Css.Style
bottom model value =
    Css.batch
        [ Css.bottom Css.zero
        , Css.transform <| Css.translateY (Css.px (-1 * toWorldY model value))
        ]


project : { a | widthRatio : Float, heightRatio : Float } -> { x : Float, y : Float } -> Css.Style
project model { x, y } =
    Css.batch
        [ Css.left Css.zero
        , Css.bottom Css.zero
        , Css.transforms
            [ Css.translateY (Css.px (-1 * toWorldY model y))
            , Css.translateX (Css.px (toWorldX model x))
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
