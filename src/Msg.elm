module Msg exposing (Msg(..))

import Mouse
import Window


type Msg
    = NoOp
    | Initialized Float Window.Size
    | Tick Float
    | WindowResized Window.Size
    | MouseDown Mouse.Position
    | ResetMachineTimer String
