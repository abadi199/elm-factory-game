module Main exposing (main)

import AnimationFrame
import Html exposing (..)
import Html.Styled
import Model exposing (Model)
import Mouse
import Msg exposing (Msg(..))
import Random.Pcg as Random
import Task
import Time
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
    case appState of
        Initializing ->
            Sub.none

        Ready _ ->
            Sub.batch
                [ AnimationFrame.diffs Tick
                , Window.resizes WindowResized
                , Mouse.downs MouseDown
                ]


initUpdate : Msg -> AppState -> ( AppState, Cmd Msg )
initUpdate msg appState =
    case appState of
        Initializing ->
            case msg of
                Initialized time windowSize ->
                    ( Ready (Model.initialModel (Random.initialSeed <| round time) windowSize)
                    , Cmd.none
                    )

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
    ( Initializing
    , Task.map2 (,) Time.now Window.size
        |> Task.perform (\( time, windowSize ) -> Initialized time windowSize)
    )
