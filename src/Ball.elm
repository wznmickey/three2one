module Ball exposing (..)

import Circle exposing (..)
import Color exposing (..)
import Msg exposing (..)
import Svg exposing (Svg)


type alias Ball =
    { shape : Circle
    }


initBall : Ball
initBall =
    Ball (initCircle (Color 255 101 101) ( 200, 296 ) 4)


viewBall : Ball -> List (Svg Msg)
viewBall a =
    viewCircle a.shape
