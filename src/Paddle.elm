module Paddle exposing (..)

import Color exposing (..)
import Dir exposing (..)
import Html exposing (div, p, text)
import Html.Attributes exposing (style)
import Msg exposing (..)
import Rectangle exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr exposing (color, cx)
import Global exposing(..)

type alias Paddle =
    { shape : List Rectangle
    , speed : Float
    }


secondPaddle : Paddle -> Rectangle
secondPaddle pad =
    case List.head (List.drop 1 pad.shape) of
        Nothing ->
            rec

        Just val ->
            val


firstPaddle : Paddle -> Rectangle
firstPaddle pad =
    case List.head pad.shape of
        Nothing ->
            rec

        Just val ->
            val


initPaddle : Color -> ( Float, Float ) -> Float -> Float -> Paddle
initPaddle a ( x, y ) c d =
    { shape = [ Rectangle a ( x, y ) c d, Rectangle a ( x + wholeWidth, y ) c d ], speed = 1 }


viewPaddle : Paddle -> List (Svg Msg)
viewPaddle a =
    List.map (\therec -> viewRectangle therec) a.shape


checkViewable : ( Float, Float ) -> Bool
checkViewable ( x, y ) =
    if (x > 0) && (x < wholeWidth) then
        True

    else
        False


checkLPart : Float -> Bool
checkLPart x =
    if (x >= 5) && (x <= 150) then
        True

    else
        False


checkRPart : Float -> Bool
checkRPart x =
    if (x >= 205) && (x <= 350) then
        True

    else
        False


newPadLF : Paddle -> Paddle
newPadLF opad =
    let
        secRec =
            secondPaddle opad

        firstRec =
            firstPaddle opad
    in
    { shape = [ firstRec ] ++ [ { secRec | leftTopPosition = ( Tuple.first firstRec.leftTopPosition + wholeWidth, Tuple.second (secondPaddle opad).leftTopPosition ) } ], speed = opad.speed }


newPadRF : Paddle -> Paddle
newPadRF opad =
    let
        secRec =
            secondPaddle opad

        firstRec =
            firstPaddle opad
    in
    { shape = [ firstRec ] ++ [ { secRec | leftTopPosition = ( Tuple.first firstRec.leftTopPosition - wholeWidth, Tuple.second (secondPaddle opad).leftTopPosition ) } ], speed = opad.speed }


newPadRS : Paddle -> Paddle
newPadRS opad =
    let
        secRec =
            secondPaddle opad

        firstRec =
            firstPaddle opad
    in
    { shape = [ { firstRec | leftTopPosition = ( Tuple.first secRec.leftTopPosition - wholeWidth, Tuple.second (secondPaddle opad).leftTopPosition ) } ] ++ [ secRec ], speed = opad.speed }


newPadLS : Paddle -> Paddle
newPadLS opad =
    let
        secRec =
            secondPaddle opad

        firstRec =
            firstPaddle opad
    in
    { shape = [ { firstRec | leftTopPosition = ( Tuple.first secRec.leftTopPosition + wholeWidth, Tuple.second (secondPaddle opad).leftTopPosition ) } ] ++ [ secRec ], speed = opad.speed }


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


movePaddle : Paddle -> Dir -> Paddle
movePaddle p d =
    if d == NoDir then
        p

    else
        { p | shape = List.map (\here -> moveRectangle here (dir2Float d) p.speed) p.shape }


rec : Rectangle
rec =
    { color = { red = 1, blue = 2, green = 3 }, leftTopPosition = ( 1, 1 ), width = 1, height = 2 }


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
    if (cx1 >= 5 && cx1 <= 395) then
        ( cx1, y1 )

    else
        ( cx2, y2 )
