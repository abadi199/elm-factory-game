module Msg exposing (Msg(..))

import Window


type Msg
    = NoOp
    | Tick Float
    | WindowResized Window.Size
