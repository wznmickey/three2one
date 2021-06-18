module Paddle exposing (..)

import Color exposing (..)
import Dir exposing (..)
import Global exposing (..)
import Html exposing (div, p, text)
import Html.Attributes exposing (style)
import Msg exposing (..)
import Rectangle exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr exposing (color, cx)


type alias Paddle =
    { shape : List Rectangle
    , speed : Float
    }



{--Get shape of first paddle--}


firstPaddle : Paddle -> Rectangle
firstPaddle pad =
    case List.head pad.shape of
        Nothing ->
            rec

        Just val ->
            val



{--Get shape of second paddle--}


secondPaddle : Paddle -> Rectangle
secondPaddle pad =
    case List.head (List.drop 1 pad.shape) of
        Nothing ->
            rec

        Just val ->
            val



{--Init paddle--}


initPaddle : Color -> ( Float, Float ) -> Float -> Float -> Paddle
initPaddle a ( x, y ) c d =
    { shape = [ Rectangle a ( x, y ) c d, Rectangle a ( x + wholeWidth, y ) c d ], speed = 1 }



{--Give paddle showing code--}


viewPaddle : Paddle -> List (Svg Msg)
viewPaddle a =
    List.map (\therec -> viewRectangle therec) a.shape



{--Check whether a paddle is visible--}


checkViewable : ( Float, Float ) -> Bool
checkViewable ( x, y ) =
    if (x > 0) && (x < wholeWidth) then
        True

    else
        False



{--Check Whether is left paddle--}


checkLPart : Float -> Bool
checkLPart x =
    if (x >= 5) && (x <= 150) then
        True

    else
        False



{--Check Whether is right paddle--}


checkRPart : Float -> Bool
checkRPart x =
    if (x >= 205) && (x <= 350) then
        True

    else
        False



{--Get new left paddle as first--}


newPadLF : Paddle -> Paddle
newPadLF opad =
    let
        secRec =
            secondPaddle opad

        firstRec =
            firstPaddle opad
    in
    { shape = [ firstRec ] ++ [ { secRec | leftTopPosition = ( Tuple.first firstRec.leftTopPosition + wholeWidth, Tuple.second (secondPaddle opad).leftTopPosition ) } ], speed = opad.speed }



{--Get new right paddle as first--}


newPadRF : Paddle -> Paddle
newPadRF opad =
    let
        secRec =
            secondPaddle opad

        firstRec =
            firstPaddle opad
    in
    { shape = [ firstRec ] ++ [ { secRec | leftTopPosition = ( Tuple.first firstRec.leftTopPosition - wholeWidth, Tuple.second (secondPaddle opad).leftTopPosition ) } ], speed = opad.speed }



{--Get new right paddle as second--}


newPadRS : Paddle -> Paddle
newPadRS opad =
    let
        secRec =
            secondPaddle opad

        firstRec =
            firstPaddle opad
    in
    { shape = [ { firstRec | leftTopPosition = ( Tuple.first secRec.leftTopPosition - wholeWidth, Tuple.second (secondPaddle opad).leftTopPosition ) } ] ++ [ secRec ], speed = opad.speed }



{--Get new left paddle as second--}


newPadLS : Paddle -> Paddle
newPadLS opad =
    let
        secRec =
            secondPaddle opad

        firstRec =
            firstPaddle opad
    in
    { shape = [ { firstRec | leftTopPosition = ( Tuple.first secRec.leftTopPosition + wholeWidth, Tuple.second (secondPaddle opad).leftTopPosition ) } ] ++ [ secRec ], speed = opad.speed }



{--Update 2 paddles--}


updateBothpaddle : Paddle -> Paddle
updateBothpaddle pAd =
    if checkViewable (firstPaddle pAd).leftTopPosition then
        if checkLPart (Tuple.first (firstPaddle pAd).leftTopPosition) then
            newPadLF pAd

        else if checkRPart (Tuple.first (firstPaddle pAd).leftTopPosition) then
            newPadRF pAd

        else
            pAd

    else if checkLPart (Tuple.first (secondPaddle pAd).leftTopPosition) then
        newPadLS pAd

    else if checkRPart (Tuple.first (secondPaddle pAd).leftTopPosition) then
        newPadRS pAd

    else
        pAd



{--Move paddles--}


movePaddle : Paddle -> Dir -> Paddle
movePaddle p d =
    if d == NoDir then
        p

    else
        { p | shape = List.map (\here -> moveRectangle here (dir2Float d) p.speed) p.shape }



{--New paddle--}


rec : Rectangle
rec =
    { color = { red = 1, blue = 2, green = 3 }, leftTopPosition = ( 1, 1 ), width = 1, height = 2 }



{--Get now visible paddle location--}


getPaddleLocation : Paddle -> ( Float, Float )
getPaddleLocation paddle =
    let
        ( x1, y1 ) =
            (firstPaddle paddle).leftTopPosition

        ( x2, y2 ) =
            (secondPaddle paddle).leftTopPosition

        w =
            (secondPaddle paddle).width

        cx1 =
            x1 + (w / 2)

        cx2 =
            x2 + (w / 2)
    in
    if cx1 >= 5 && cx1 <= 395 then
        ( cx1, y1 )

    else
        ( cx2, y2 )
