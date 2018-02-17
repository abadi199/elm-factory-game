module Projector
    exposing
        ( bottom
        , height
        , left
        , toViewportX
        , toViewportY
        , toWorldX
        , toWorldY
        , width
        )

import Css
import Window


toWorldX : Window.Size -> Float -> Float
toWorldX windowSize x =
    if x == 0 then
        0
    else
        widthRatio windowSize
            * x


toWorldY : Window.Size -> Float -> Float
toWorldY windowSize y =
    if y == 0 then
        0
    else
        heightRatio windowSize
            * y


toViewportX : Window.Size -> Float -> Float
toViewportX windowSize x =
    if x == 0 then
        0
    else
        x / widthRatio windowSize


toViewportY : Window.Size -> Float -> Float
toViewportY windowSize y =
    if y == 0 then
        0
    else
        y / heightRatio windowSize


left : { a | windowSize : Window.Size } -> Float -> Css.Style
left { windowSize } value =
    Css.left (Css.px (toWorldX windowSize value))


bottom : { a | windowSize : Window.Size } -> Float -> Css.Style
bottom { windowSize } value =
    Css.bottom (Css.px (toWorldY windowSize value))


width : { a | windowSize : Window.Size } -> Float -> Css.Style
width { windowSize } value =
    Css.width (Css.px (toWorldX windowSize value))


height : { a | windowSize : Window.Size } -> Float -> Css.Style
height { windowSize } value =
    Css.height (Css.px (toWorldY windowSize value))


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
