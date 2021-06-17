module Meet exposing (..)

import Ball exposing (..)
import Brick exposing (..)
import Circle exposing (..)
import Dir exposing (..)
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
    if brick.show == "visible" then
        checkCircleRectangle ball.shape brick.shape

    else
        NoDir


checkBallPaddle1Meet : Ball -> Paddle -> Dir
checkBallPaddle1Meet ball paddle =
    checkCircleRectangle ball.shape (firstPaddle paddle)

checkBallPaddle2Meet : Ball -> Paddle -> Dir
checkBallPaddle2Meet ball paddle =
    checkCircleRectangle ball.shape (secondPaddle paddle)


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
deleteBrick circle brick =
    if brick.show == "hidden" then
        Nothing

    else if (abs (Tuple.first circle.position - Tuple.first brick.shape.leftTopPosition) <= circle.r) && (Tuple.second circle.position >= Tuple.second brick.shape.leftTopPosition && Tuple.second circle.position <= brick.shape.height + Tuple.second brick.shape.leftTopPosition) then
        Just brick.shape.leftTopPosition

    else if (abs (Tuple.first circle.position - (Tuple.first brick.shape.leftTopPosition + brick.shape.width)) <= circle.r) && (Tuple.second circle.position >= Tuple.second brick.shape.leftTopPosition && Tuple.second circle.position <= brick.shape.height + Tuple.second brick.shape.leftTopPosition) then
        Just brick.shape.leftTopPosition

    else if (abs (Tuple.second circle.position - Tuple.second brick.shape.leftTopPosition) <= circle.r) && (Tuple.first circle.position >= Tuple.first brick.shape.leftTopPosition && Tuple.first circle.position <= brick.shape.width + Tuple.first brick.shape.leftTopPosition) then
        Just brick.shape.leftTopPosition

    else if (abs (Tuple.second circle.position - (Tuple.second brick.shape.leftTopPosition + brick.shape.height)) <= circle.r) && (Tuple.first circle.position >= Tuple.first brick.shape.leftTopPosition && Tuple.first circle.position <= brick.shape.width + Tuple.first brick.shape.leftTopPosition) then
        Just brick.shape.leftTopPosition

    else
        Nothing


checkBallBorderMeet : Ball -> (Float,Float) -> (Ball,Int,Dir)
checkBallBorderMeet ball (x,y) =
    let
        r =
            ball.shape.r
    in
    if Tuple.first ball.shape.position <= r then
       ({ ball | shape = { r = ball.shape.r , color = ball.shape.color , position = ( Tuple.first ball.shape.position + 400 - r , Tuple.second ball.shape.position  ) } },0,NoDir)
    
    else if Tuple.first ball.shape.position + r >= x then
        ({ ball | shape = { r = ball.shape.r , color = ball.shape.color , position = ( Tuple.first ball.shape.position - 400 + r, Tuple.second ball.shape.position  ) } } ,0,NoDir)

    else if Tuple.second ball.shape.position <= r then
        (ball ,0, Up)

    else if Tuple.second ball.shape.position + r >= y then
        (ball,-1,NoDir)
    else
       (ball,0,NoDir)
