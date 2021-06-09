module Msg exposing (..)

import Base exposing (..)
import Browser.Events exposing (Visibility)
import Html exposing (time)


type Msg
    = BallMeet Dir
    | Tick Float
    | Empty
    | KeyPress Dir
    | KeyRelease Dir
    | OnView Visibility
    | Continue