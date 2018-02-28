module View exposing (view)

import Css exposing (..)
import FallingObject
import Hero
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Machine
import Model exposing (Model)
import Msg exposing (Msg(..))
import Projector


view : Model -> Html Msg
view model =
    world model
        [ Machine.view model
        , FallingObject.view model
        , Hero.view model
        ]


world : Model -> List (Html Msg) -> Html Msg
world model content =
    div
        [ style <|
            Css.asPairsDEPRECATED
                [ Css.width (vw 100)
                , Css.height (vh 100)
                , backgroundColor (hex "#87ceeb")
                , position relative
                ]
        ]
        (floor model :: content ++ [ ceiling model, gameOverView model ])


floor : Model -> Html Msg
floor model =
    div
        [ style <|
            Css.asPairsDEPRECATED
                [ position absolute
                , Projector.project model { x = -model.origin.x, y = -model.origin.y }
                , Css.width (vw 100)
                , Projector.height model (model.floorPositionY + model.origin.y)
                , backgroundImage (url "floor.svg")
                , backgroundRepeat repeat
                , backgroundSize (px (Projector.toWorldX model 50))
                ]
        ]
        [ div
            [ style <|
                Css.asPairsDEPRECATED
                    [ Css.width (pct 100)
                    , Css.height (px (Projector.toWorldX model 50))
                    , backgroundImage (url "floorTop.svg")
                    , backgroundRepeat repeatX
                    , backgroundSize (px (Projector.toWorldX model 50))
                    ]
            ]
            []
        ]


ceiling : Model -> Html Msg
ceiling model =
    div
        [ style <|
            Css.asPairsDEPRECATED
                [ position absolute
                , Projector.project model { x = -model.origin.x, y = model.ceilingPositionY }
                , Css.width (vw 100)
                , Projector.height model (Projector.toViewportY model (toFloat model.windowSize.height) - model.ceilingPositionY)
                , backgroundImage (url "floor.svg")
                , backgroundRepeat repeat
                , backgroundSize (px (Projector.toWorldX model 50))
                ]
        ]
        [ div
            [ style <|
                Css.asPairsDEPRECATED
                    [ position absolute
                    , left zero
                    , bottom zero
                    , Css.width (pct 100)
                    , Css.height (px (Projector.toWorldX model 50))
                    , backgroundImage (url "ceiling.svg")
                    , backgroundRepeat repeatX
                    , backgroundSize (px (Projector.toWorldX model 50))
                    , backgroundPosition bottom
                    ]
            ]
            []
        ]


gameOverView : Model -> Html Msg
gameOverView model =
    if Model.isGameOver model then
        div
            [ style <|
                Css.asPairsDEPRECATED
                    [ position absolute
                    , Css.width (vw 100)
                    , Css.height (vh 100)
                    , backgroundColor (rgba 255 255 255 0.5)
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , fontSize (Css.px 100)
                    , fontFamily sansSerif
                    , flexDirection column
                    ]
            ]
            [ text "Game Over"
            , button
                [ type_ "button"
                , onClick RetryClicked
                , style <|
                    Css.asPairsDEPRECATED
                        [ fontSize (Css.px 20) ]
                ]
                [ text "Try Again" ]
            ]
    else
        text ""
