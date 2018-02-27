module Update exposing (update)

import Coordinates
import FallingObject
import Hero
import Machine
import Model exposing (Model)
import Mouse
import Msg exposing (Msg(..))
import Projector
import Set exposing (Set)
import Window


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Initialized _ _ ->
            ( model, Cmd.none )

        Tick delta ->
            tick delta model

        WindowResized windowSize ->
            ( updateWindowSize windowSize model
            , Cmd.none
            )

        MouseDown mousePosition ->
            ( mouseClicked mousePosition model
            , Cmd.none
            )

        ResetMachineTimer machineId ->
            ( resetMachineTimer machineId model, Cmd.none )

        ObjectCaptured objects ->
            objectCaptured objects model


resetMachineTimer : String -> Model -> Model
resetMachineTimer machineId model =
    model |> Machine.resetTimer machineId


mouseClicked : Mouse.Position -> Model -> Model
mouseClicked mousePosition model =
    let
        coordinates =
            Coordinates.fromPosition model mousePosition
                |> Projector.toViewport model
    in
    model
        |> Machine.select coordinates
        |> Hero.moveTo coordinates


tick : Float -> Model -> ( Model, Cmd Msg )
tick delta model =
    { model | timestamp = model.timestamp + delta }
        |> Machine.updateTimer delta
        |> FallingObject.update delta
        |> Hero.update delta


updateWindowSize : Window.Size -> Model -> Model
updateWindowSize windowSize model =
    { model
        | widthRatio = Projector.widthRatio windowSize
        , heightRatio = Projector.widthRatio windowSize
        , origin = Projector.calculateOrigin windowSize
        , windowSize = windowSize
    }


objectCaptured : Set String -> Model -> ( Model, Cmd Msg )
objectCaptured objects model =
    let
        _ =
            Debug.log "objectCaptured" objects
    in
    ( model
        |> Hero.captureObjects objects
        |> FallingObject.removeObjects objects
    , Cmd.none
    )
