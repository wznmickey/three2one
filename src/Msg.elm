module Msg exposing (..)

import Dir exposing (..)
import Browser.Events exposing (Visibility)
import Time exposing(..)

type GameStatus
    = Play
    | Pause
    | Stop
    | WaitToContinue
    | Continue

type Msg
    = BallMeet Dir
    | Tick Float
    | Empty
    | KeyPress Dir
    | KeyRelease Dir
    | OnView Visibility
    | Checklife
    | Buy1
    | Buy2
    | Buy3
    | Game GameStatus
    | GetTime Posix
    | Init 
