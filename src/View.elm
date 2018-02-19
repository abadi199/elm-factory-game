module View exposing (view)

import Css exposing (..)
import FallingObject
import Hero
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Machine
import Model exposing (Model)
import Msg exposing (Msg(..))


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
        [ css
            [ Css.width (vw 100)
            , Css.height (vh 100)
            , backgroundColor (hex "87ceeb")
            , position relative
            ]
        ]
        content
