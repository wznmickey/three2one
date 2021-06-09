module Color exposing (..)

import String exposing (fromInt)

--define the basic types of colors
type alias Color =
    { red : Int
    , blue : Int
    , green : Int
    }


getcolor : Color -> String
getcolor color =
    let
        r =
            fromInt color.red

        g =
            fromInt color.green

        b =
            fromInt color.blue
    in
    "rgb(" ++ r ++ "," ++ g ++ "," ++ b ++ ")"
