module View exposing (view)

import Css exposing (..)
import FallingObject
import Hero
import Html exposing (..)
import Html.Attributes exposing (..)
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
        (floor model :: content)


floor : Model -> Html Msg
floor model =
    div
        [ style <|
            Css.asPairsDEPRECATED
                [ backgroundColor (hex "#aaa")
                , position absolute
                , Projector.project model { x = 0, y = 0 }
                , Css.width (vw 100)
                , Projector.height model model.floorPositionY
                ]
        ]
        []
