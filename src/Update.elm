module Update exposing (update)

import Coordinates
import Hero
import Model exposing (Model)
import Msg exposing (Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick delta ->
            ( animate delta model, Cmd.none )

        WindowResized windowSize ->
            ( { model | windowSize = windowSize }, Cmd.none )

        MouseDown mousePosition ->
            ( model
                |> Hero.moveTo (Coordinates.fromPosition model mousePosition)
            , Cmd.none
            )


animate : Float -> Model -> Model
animate delta model =
    model
        |> Hero.move delta
