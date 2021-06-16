module Ball exposing (..)

import Base exposing (..)
import Circle exposing (..)
import Color exposing (..)
import Msg exposing (..)
import Svg exposing (Svg)


type alias Ball =
    { shape : Circle
    , speed : Float
    , angle : Float
    , life : Int
    }


initBall : Ball
initBall =
    Ball (initCircle (Color 255 101 101) ( 200, 296 ) 4) 0.05 45 1


viewBall : Ball -> List (Svg Msg)
viewBall a =
    viewCircle a.shape


ballRun : Ball -> Float -> Ball
ballRun ball time =
    { ball | shape = moveCircle ball.shape ball.angle (ball.speed * time) }


ballRotate : Ball -> Dir -> Ball
ballRotate ball dir =
    case dir of
        Down ->
            { ball | angle = 360 - ball.angle }

        Up ->
            { ball | angle = 360 - ball.angle }

        Left ->
            { ball | angle = 180 - ball.angle }

        Right ->
            { ball | angle = 180 - ball.angle }

        NoDir ->
            ball
