module Main exposing (main)

import AnimationFrame
import Html exposing (..)
import Html.Styled
import Model exposing (Model)
import Msg exposing (Msg(..))
import Task
import Update exposing (update)
import View exposing (view)
import Window


main : Program Never AppState Msg
main =
    Html.program
        { init = init
        , view = initView
        , update = initUpdate
        , subscriptions = subscriptions
        }


type AppState
    = Initializing
    | Ready Model


subscriptions : AppState -> Sub Msg
subscriptions appState =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes WindowResized
        ]


initUpdate : Msg -> AppState -> ( AppState, Cmd Msg )
initUpdate msg appState =
    case appState of
        Initializing ->
            case msg of
                WindowResized windowSize ->
                    ( Ready (Model.initialModel windowSize), Cmd.none )

                _ ->
                    ( appState, Cmd.none )

        Ready model ->
            update msg model |> toAppState


toAppState : ( Model, Cmd Msg ) -> ( AppState, Cmd Msg )
toAppState ( model, cmd ) =
    ( Ready model, cmd )


initView : AppState -> Html Msg
initView appState =
    case appState of
        Initializing ->
            div [] []

        Ready model ->
            view model |> Html.Styled.toUnstyled


init : ( AppState, Cmd Msg )
init =
    ( Initializing, Window.size |> Task.perform WindowResized )