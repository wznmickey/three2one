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
<<<<<<< HEAD
    | Continue
    | Checklife
    | Buy1
    | Buy2
    | Buy3
=======
    | Game GameStatus
    | GetTime Posix
    | Init 
>>>>>>> 275108592250cd55ff54a3d6d6a5403b3632327e
