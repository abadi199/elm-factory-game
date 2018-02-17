module Msg exposing (Msg(..))

import Mouse
import Window


type Msg
    = NoOp
    | Tick Float
    | WindowResized Window.Size
    | MouseDown Mouse.Position
    | ResetMachineTimer String
