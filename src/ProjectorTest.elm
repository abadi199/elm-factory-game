module ProjectorTest exposing (suite)

import Expect exposing (..)
import Fuzz
import Projector
import Random
import Test exposing (..)


suite : Test
suite =
    describe "Projector Test"
        [ toWorldSuite
        , toViewportSuite
        , fuzz (Fuzz.map3 (,,) (Fuzz.intRange 0 Random.maxInt) (Fuzz.intRange 1 Random.maxInt) (Fuzz.intRange 1 Random.maxInt)) "toWorld and then toViewport should give the original value" <|
            \( x, width, height ) ->
                x
                    |> toFloat
                    |> Projector.toWorldX { width = width, height = height }
                    |> Projector.toViewportX { width = width, height = height }
                    |> within (Absolute 1) (toFloat x)
        ]


toWorldSuite : Test
toWorldSuite =
    describe "Projector.toWorldX and Projector.toWorldY"
        [ fuzz Fuzz.int "Projector.toWorldX when viewport and world is the same size should not change the value." <|
            \x ->
                x
                    |> toFloat
                    |> Projector.toWorldX { width = 1920, height = 1080 }
                    |> within (Absolute 1) (toFloat x)
        , fuzz Fuzz.int "Projector.toWorldY when viewport and world is the same size should not change the value." <|
            \y ->
                y
                    |> toFloat
                    |> Projector.toWorldY { width = 1920, height = 1080 }
                    |> within (Absolute 1) (toFloat y)
        , test "Projector.toWorldX of 1920 for 16x9" <|
            \_ ->
                1920
                    |> Projector.toWorldX { width = 16, height = 9 }
                    |> within (Absolute 1) (toFloat 16)
        , test "Projector.toWorldY of 1080 for 16x9" <|
            \_ ->
                1080
                    |> Projector.toWorldY { width = 16, height = 9 }
                    |> within (Absolute 1) (toFloat 9)
        , test "Projector.toWorldX of 960 for 800x600" <|
            \_ ->
                960
                    |> Projector.toWorldX { width = 800, height = 600 }
                    |> within (Absolute 1) (toFloat 400)
        , test "Projector.toWorldY of 540 for 800x600" <|
            \_ ->
                540
                    |> Projector.toWorldX { width = 800, height = 600 }
                    |> within (Absolute 1) (toFloat 225)
        ]


toViewportSuite : Test
toViewportSuite =
    describe "Projector.toViewportX and Projector.toViewportY"
        [ fuzz Fuzz.int "Projector.toViewportX when viewport and world is the same size should not change the value." <|
            \x ->
                x
                    |> toFloat
                    |> Projector.toViewportX { width = 1920, height = 1080 }
                    |> within (Absolute 1) (toFloat x)
        , fuzz Fuzz.int "Projector.toViewportY when viewport and world is the same size should not change the value." <|
            \y ->
                y
                    |> toFloat
                    |> Projector.toViewportY { width = 1920, height = 1080 }
                    |> within (Absolute 1) (toFloat y)
        , test "Projector.toViewportX of 16 for 16x9" <|
            \_ ->
                16
                    |> Projector.toViewportX { width = 16, height = 9 }
                    |> within (Absolute 1) (toFloat 1920)
        , test "Projector.toViewportY of 9 for 16x9" <|
            \_ ->
                9
                    |> Projector.toViewportY { width = 16, height = 9 }
                    |> within (Absolute 1) (toFloat 1080)
        , test "Projector.toViewportX of 400 for 800x600" <|
            \_ ->
                400
                    |> Projector.toViewportX { width = 800, height = 600 }
                    |> within (Absolute 1) (toFloat 960)
        , test "Projector.toViewportY of 225 for 800x600" <|
            \_ ->
                225
                    |> Projector.toViewportX { width = 800, height = 600 }
                    |> within (Absolute 1) (toFloat 540)
        ]
