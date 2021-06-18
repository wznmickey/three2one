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
    | Story Int
    | Skill Int

type Msg
    = BallMeet Dir
    | Tick Float
    | Empty
    | KeyPress Dir
    | KeyRelease Dir
    | OnView Visibility
    | Checklife
    | Buy Int
    | Game GameStatus
    | GetTime Posix
    | Init 
