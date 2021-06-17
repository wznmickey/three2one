module Ball exposing (..)

import Dir exposing (..)
import Circle exposing (..)
import Color exposing (..)
import Msg exposing (..)
import Svg exposing (Svg)
import Global exposing(..)
import Svg.Attributes exposing (limitingConeAngle)

type alias Ball =
    { shape : Circle
    , speed : (Float,Float)
    }


initBall : Ball
initBall =
    Ball (initCircle (Color 255 101 101) ( 200, 296 ) 4) (0.1 , 0.1)



viewBall : Ball -> List (Svg Msg)
viewBall a =
    viewCircle a.shape


ballRun : Ball -> Float -> Ball
ballRun ball time =
    let 
        (x,y)=ball.speed
        vy =  y- (time) * g 
    in
    { ball | shape = moveCircle ball.shape (x,vy) time , speed=(x,vy)}


ballRotate : Ball -> Dir -> Ball
ballRotate ball dir =
    case dir of 
        NoDir -> 
            ball
        _ -> 
            let 
                (x,y)=ball.speed
                angle = atan2 y x
                nangle =
                    if (angle<0) then angle+360
                    else if (angle>360) then angle-360
                    else angle
                speed = sqrt ((x*x)+(y*y))
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
                        
                nx=(cos nnangle) * speed 
                ny=(sin nnangle) * speed
            in 
            {ball|speed=(nx,ny)}

ballSpeedUp : Ball -> Ball
ballSpeedUp ball =
    let 
        (x,y) = ball.speed
    in
    {ball|speed=(x,y+0.01)}

ballSpeedDown : Ball -> Ball
ballSpeedDown ball =
    let 
        (x,y) = ball.speed
    in
    {ball|speed=(x,y-0.01)}