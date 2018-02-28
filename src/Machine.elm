module Machine
    exposing
        ( Machine
        , Model
        , create
        , isKaboom
        , resetTimer
        , select
        , selected
        , selectedMachineId
        , updateTimer
        , view
        )

import Collision
import Coordinates exposing (Coordinates)
import Css exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msg exposing (Msg(..))
import Murmur3
import Projector


type alias Machine =
    { position : Coordinates
    , width : Float
    , height : Float
    , timerInMillisecond : Timer
    , maxTimeInMillisecond : Float
    , selected : Selected
    }


type Timer
    = Ticking Float
    | Kaboom


type Selected
    = Selected
    | NotSelected


type alias Model a =
    { a
        | machines : Dict String Machine
        , widthRatio : Float
        , heightRatio : Float
        , origin : Coordinates
    }


updateTimer : Float -> Model a -> Model a
updateTimer delta machines =
    let
        updateMachineTimer _ machine =
            case machine.timerInMillisecond of
                Ticking timerInMillisecond ->
                    if timerInMillisecond + delta >= machine.maxTimeInMillisecond then
                        { machine | timerInMillisecond = Kaboom }
                    else
                        { machine | timerInMillisecond = Ticking (timerInMillisecond + delta) }

                Kaboom ->
                    machine
    in
    { machines
        | machines =
            machines.machines
                |> Dict.map updateMachineTimer
    }


create : Coordinates -> List ( String, Machine ) -> List ( String, Machine )
create coordinates list =
    ( toString <| Murmur3.hashString 8821923 <| toString list
    , { position = coordinates
      , width = 100
      , height = 300
      , timerInMillisecond = Ticking 0
      , maxTimeInMillisecond = 10000
      , selected = NotSelected
      }
    )
        :: list


view : Model a -> Html Msg
view model =
    div [] (model.machines |> Dict.values |> List.map (machineView model))


machineView : Model a -> Machine -> Html Msg
machineView model machine =
    div [ style <| Css.asPairsDEPRECATED [ machineStyle model machine ] ]
        [ case machine.timerInMillisecond of
            Ticking timerInMillisecond ->
                text <| toString <| Basics.round <| timerInMillisecond / 1000

            Kaboom ->
                text "KABOOOOM!!!"
        ]


machineStyle : Model a -> Machine -> Style
machineStyle model machine =
    let
        backgroundColor =
            case machine.selected of
                Selected ->
                    hex "#F88"

                NotSelected ->
                    hex "#F00"
    in
    Css.batch
        [ Css.backgroundColor backgroundColor
        , position absolute
        , Projector.width model machine.width
        , Projector.height model machine.height
        , Projector.project model machine.position
        ]


isSelected : Machine -> Bool
isSelected machine =
    case machine.selected of
        Selected ->
            True

        NotSelected ->
            False


selectedMachineId : Model a -> Maybe String
selectedMachineId machines =
    machines.machines
        |> Dict.filter (\_ machine -> isSelected machine)
        |> Dict.keys
        |> List.head


selected : Model a -> Maybe Machine
selected machines =
    machines.machines
        |> Dict.values
        |> List.filter isSelected
        |> List.head


select : Coordinates -> Model a -> Model a
select coordinates machines =
    { machines | machines = machines.machines |> Dict.map (always (updateSelected coordinates)) }


updateSelected : Coordinates -> Machine -> Machine
updateSelected coordinates machine =
    if machine |> collidesWith coordinates then
        { machine | selected = Selected }
    else
        { machine | selected = NotSelected }


collidesWith : Coordinates -> Machine -> Bool
collidesWith coordinates machine =
    coordinates
        |> Collision.coordinatesWithRect machine


resetTimer : String -> Model a -> Model a
resetTimer machineId machines =
    let
        reset machine =
            case machine.timerInMillisecond of
                Ticking _ ->
                    { machine | timerInMillisecond = Ticking 0 }

                Kaboom ->
                    machine
    in
    { machines | machines = Dict.update machineId (Maybe.map reset) machines.machines }


isKaboom : Machine -> Bool
isKaboom machine =
    case machine.timerInMillisecond of
        Kaboom ->
            True

        Ticking _ ->
            False
