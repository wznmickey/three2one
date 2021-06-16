module Dir exposing (..)


type Dir
    = Up
    | Down
    | Left
    | Right
    | NoDir


dir2Int : Dir -> Int
dir2Int a =
    case a of
        Up ->
            90

        Down ->
            270

        Left ->
            180

        Right ->
            0

        NoDir ->
            0


dir2Float : Dir -> Float
dir2Float a =
    case a of
        Up ->
            90

        Down ->
            270

        Left ->
            180

        Right ->
            0

        NoDir ->
            0
