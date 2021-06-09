module Meet exposing (..)

import Ball exposing (..)
import Base exposing (..)
import Brick exposing (..)
import Circle exposing (..)
import Paddle exposing (..)
import Rectangle exposing (..)



checkBallBrickMeet : Ball -> List Brick -> Dir
checkBallBrickMeet ball brick =
    Maybe.withDefault NoDir
        (List.head
            (List.filter
                (\dir ->
                    case dir of
                        NoDir ->
                            False

                        _ ->
                            True
                )
                (List.map (checkBallSingleBrickMeet ball) brick)
            )
        )


checkBallSingleBrickMeet : Ball -> Brick -> Dir
checkBallSingleBrickMeet ball brick =
    checkCircleRectangle ball.shape brick.shape


checkBallPaddleMeet : Ball -> Paddle -> Dir
checkBallPaddleMeet ball paddle =
    checkCircleRectangle ball.shape paddle.shape

--simulate the process of the ball hitting the bricks
checkCircleRectangle : Circle -> Rectangle -> Dir
checkCircleRectangle circle rectangle =
    if (abs (Tuple.first circle.position - Tuple.first rectangle.leftTopPosition) <= circle.r) && (Tuple.second circle.position >= Tuple.second rectangle.leftTopPosition && Tuple.second circle.position <= rectangle.height + Tuple.second rectangle.leftTopPosition) then
        Left

    else if (abs (Tuple.first circle.position - (Tuple.first rectangle.leftTopPosition + rectangle.width)) <= circle.r) && (Tuple.second circle.position >= Tuple.second rectangle.leftTopPosition && Tuple.second circle.position <= rectangle.height + Tuple.second rectangle.leftTopPosition) then
        Right

    else if (abs (Tuple.second circle.position - Tuple.second rectangle.leftTopPosition) <= circle.r) && (Tuple.first circle.position >= Tuple.first rectangle.leftTopPosition && Tuple.first circle.position <= rectangle.width + Tuple.first rectangle.leftTopPosition) then
        Up

    else if (abs (Tuple.second circle.position - (Tuple.second rectangle.leftTopPosition + rectangle.height)) <= circle.r) && (Tuple.first circle.position >= Tuple.first rectangle.leftTopPosition && Tuple.first circle.position <= rectangle.width + Tuple.first rectangle.leftTopPosition) then
        Down

    else
        NoDir

deleteBrick : Circle -> Brick -> Maybe ( Float, Float )
deleteBrick circle rectangle =

    if (abs (Tuple.first circle.position - Tuple.first rectangle.shape.leftTopPosition) <= circle.r) && (Tuple.second circle.position >= Tuple.second rectangle.shape.leftTopPosition && Tuple.second circle.position <= rectangle.shape.height + Tuple.second rectangle.shape.leftTopPosition) then
     Just rectangle.shape.leftTopPosition

    else if (abs (Tuple.first circle.position - (Tuple.first rectangle.shape.leftTopPosition + rectangle.shape.width)) <= circle.r) && (Tuple.second circle.position >= Tuple.second rectangle.shape.leftTopPosition && Tuple.second circle.position <= rectangle.shape.height + Tuple.second rectangle.shape.leftTopPosition) then
      Just  rectangle.shape.leftTopPosition

    else if (abs (Tuple.second circle.position - Tuple.second rectangle.shape.leftTopPosition) <= circle.r) && (Tuple.first circle.position >= Tuple.first rectangle.shape.leftTopPosition && Tuple.first circle.position <= rectangle.shape.width + Tuple.first rectangle.shape.leftTopPosition) then
      Just rectangle.shape.leftTopPosition

    else if (abs (Tuple.second circle.position - (Tuple.second rectangle.shape.leftTopPosition + rectangle.shape.height)) <= circle.r) && (Tuple.first circle.position >= Tuple.first rectangle.shape.leftTopPosition && Tuple.first circle.position <= rectangle.shape.width + Tuple.first rectangle.shape.leftTopPosition) then
       Just rectangle.shape.leftTopPosition

    else 
       Nothing 
        

checkBallBorderMeet : Ball -> ( Float, Float ) -> Dir
checkBallBorderMeet ball ( x, y ) =
    let
        r =
            ball.shape.r
    in
    if Tuple.first ball.shape.position <= r then
        Left

    else if Tuple.first ball.shape.position + r >= x then
        Right

    else if Tuple.second ball.shape.position <= r then
        Up

    else if Tuple.second ball.shape.position + r >= y then
        Down

    else
        NoDir
