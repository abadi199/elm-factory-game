module Update exposing (update)

import Coordinates
import Hero
import Machine
import Model exposing (Model)
import Mouse
import Msg exposing (Msg(..))
import Projector
import Window


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
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


resetMachineTimer : String -> Model -> Model
resetMachineTimer machineId model =
    model |> Machine.resetTimer machineId


mouseClicked : Mouse.Position -> Model -> Model
mouseClicked mousePosition model =
    let
        coordinates =
            Coordinates.fromPosition model mousePosition
    in
    model
        |> Machine.select coordinates
        |> Hero.moveTo coordinates


tick : Float -> Model -> ( Model, Cmd Msg )
tick delta model =
    model
        |> Machine.updateTimer delta
        |> animate delta


animate : Float -> Model -> ( Model, Cmd Msg )
animate delta model =
    model
        |> Hero.move delta


updateWindowSize : Window.Size -> Model -> Model
updateWindowSize windowSize model =
    { model
        | widthRatio = Projector.widthRatio windowSize
        , heightRatio = Projector.widthRatio windowSize
        , windowSize = windowSize
    }
