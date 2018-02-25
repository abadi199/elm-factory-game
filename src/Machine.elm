module Machine
    exposing
        ( Machines
        , create
        , resetTimer
        , select
        , selected
        , selectedMachineId
        , updateTimer
        , view
        )

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
    , timerInMillisecond : Float
    , maxTimeInMillisecond : Float
    , selected : Selected
    }


type Selected
    = Selected
    | NotSelected


type alias Machines a =
    { a
        | machines : Dict String Machine
        , widthRatio : Float
        , heightRatio : Float
        , origin : Coordinates
    }


updateTimer : Float -> Machines a -> Machines a
updateTimer delta machines =
    let
        updateMachineTimer _ machine =
            { machine | timerInMillisecond = machine.timerInMillisecond + delta }
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
      , timerInMillisecond = 0
      , maxTimeInMillisecond = 10000
      , selected = NotSelected
      }
    )
        :: list


view : Machines a -> Html Msg
view model =
    div [] (model.machines |> Dict.values |> List.map (machineView model))


machineView : Machines a -> Machine -> Html Msg
machineView model machine =
    div [ style <| Css.asPairsDEPRECATED [ machineStyle model machine ] ]
        [ text <| toString <| Basics.round <| machine.timerInMillisecond / 1000 ]


machineStyle : Machines a -> Machine -> Style
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


selectedMachineId : Machines a -> Maybe String
selectedMachineId machines =
    machines.machines
        |> Dict.filter (\_ machine -> isSelected machine)
        |> Dict.keys
        |> List.head


selected : Machines a -> Maybe Machine
selected machines =
    machines.machines
        |> Dict.values
        |> List.filter isSelected
        |> List.head


select : Coordinates -> Machines a -> Machines a
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
        |> Coordinates.collidesWithRect machine


resetTimer : String -> Machines a -> Machines a
resetTimer machineId machines =
    let
        reset machine =
            { machine | timerInMillisecond = 0 }
    in
    { machines | machines = Dict.update machineId (Maybe.map reset) machines.machines }
