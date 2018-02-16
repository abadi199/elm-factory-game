module Projector
    exposing
        ( bottom
        , height
        , left
        , width
        )

import Css
import Window


virtualWindow : { height : Float, width : Float }
virtualWindow =
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
    width / virtualWindow.width


heightRatio : Window.Size -> Float
heightRatio windowSize =
    let
        height =
            if 1.777777778 < toFloat windowSize.width / toFloat windowSize.height then
                toFloat windowSize.height
            else
                toFloat windowSize.width / 1.777777778
    in
    height / virtualWindow.height


left : Float -> { a | windowSize : Window.Size } -> Css.Style
left x { windowSize } =
    Css.left (Css.px (widthRatio windowSize * x))


bottom : Float -> { a | windowSize : Window.Size } -> Css.Style
bottom y { windowSize } =
    Css.bottom (Css.px (heightRatio windowSize * y))


width : Float -> { a | windowSize : Window.Size } -> Css.Style
width x { windowSize } =
    Css.width (Css.px (widthRatio windowSize * x))


height : Float -> { a | windowSize : Window.Size } -> Css.Style
height x { windowSize } =
    Css.height (Css.px (heightRatio windowSize * x))
