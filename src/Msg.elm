module Msg exposing (..)

import Base exposing (..)
import Html exposing (time)


type Msg
    = BallMeet Dir
    | Tick Float
    | Empty
    | KeyPress Dir
    | KeyRelease Dir
    | OnView Visibility 
