module Ball exposing (..)

import Circle exposing (..)
import Color exposing (..)
import Dir exposing (..)
import Global exposing (..)
import Msg exposing (..)
import Paddle exposing (..)
import Svg exposing (Svg)
import Svg.Attributes exposing (limitingConeAngle)


type alias Ball =
    { shape : Circle
    , speed : ( Float, Float )
    }



{--Init ball--}


initBall : Ball
initBall =
    Ball (initCircle (Color 255 101 101) ( 200, 271 ) 4) ( 0.15, 0.15 )



{--Provide ball showing code--}


viewBall : Ball -> List (Svg Msg)
viewBall a =
    viewCircle a.shape



{--Handle ball moving--}


ballRun : Ball -> Float -> Ball
ballRun ball time =
    let
        ( x, y ) =
            ball.speed

        vy =
            y - time * g
    in
    { ball | shape = moveCircle ball.shape ( x, vy ) time, speed = ( x, vy ) }



{--Handle ball meeting bricks--}


ballRotateWithBrick : Ball -> Dir -> Ball
ballRotateWithBrick ball dir =
    let
        ( x, y ) =
            ball.speed

        angle =
            atan2 y x

        nangle =
            if angle < 0 then
                angle + 360

            else if angle > 360 then
                angle - 360

            else
                angle

        speed =
            sqrt ((x * x) + (y * y))

        nspeed =
            max (speed * 0.8) (min 0.1 speed)
    in
    case dir of
        NoDir ->
            ball

        Up ->
            { ball | speed = ( cos nangle * nspeed, sin nangle * nspeed ) }

        Down ->
            { ball | speed = ( cos (360 - nangle) * nspeed, sin (360 - nangle) * nspeed ) }

        Left ->
            { ball | speed = ( -(abs (cos (180 - nangle) * nspeed)), sin (180 - nangle) * nspeed ) }

        Right ->
            { ball | speed = ( abs (cos (180 - nangle) * nspeed), sin (180 - nangle) * nspeed ) }



{--Handle ball rotate var--}


ballRotate : Ball -> Dir -> Ball
ballRotate ball dir =
    case dir of
        NoDir ->
            ball

        _ ->
            let
                ( x, y ) =
                    ball.speed

                angle =
                    atan2 y x

                nangle =
                    if angle < 0 then
                        angle + 360

                    else if angle > 360 then
                        angle - 360

                    else
                        angle

                speed =
                    sqrt ((x * x) + (y * y))

                nnangle =
                    case dir of
                        Down ->
                            360 - nangle

                        Up ->
                            360 - nangle

                        Left ->
                            180 - nangle

                        Right ->
                            180 - nangle

                        _ ->
                            nangle

                nx =
                    cos nnangle * speed

                ny =
                    sin nnangle * speed
            in
            { ball | speed = ( nx, ny ) }



{--Give upward a--}


ballSpeedUp : Ball -> Ball
ballSpeedUp ball =
    let
        ( x, y ) =
            ball.speed
    in
    { ball | speed = ( x, y + 0.01 ) }



{--Give downward a--}


ballSpeedDown : Ball -> Ball
ballSpeedDown ball =
    let
        ( x, y ) =
            ball.speed
    in
    { ball | speed = ( x, y - 0.01 ) }



{--Get new ball--}


backBall : Circle -> Paddle -> Circle
backBall shape paddle =
    let
        ( x, y ) =
            getPaddleLocation paddle
    in
    { shape | position = ( x, y + shape.r ) }
