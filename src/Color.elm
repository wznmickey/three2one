module Color exposing (..)

import String exposing (fromInt)


type alias Color =
    { red : Int
    , blue : Int
    , green : Int
    }



{--Transfer color to showing code--}


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
