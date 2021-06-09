module Paddle exposing (..)

import Base exposing (..)
import Color exposing (..)
import Html exposing (p)
import Msg exposing (..)
import Rectangle exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr exposing (color, cx)


type alias Paddle =
    { shape : Rectangle
    }


initPaddle : Color -> ( Float, Float ) -> Float -> Float -> Paddle
initPaddle a b c d =
    Paddle (Rectangle a b c d)


viewPaddle : Paddle -> List (Svg Msg)
viewPaddle a =
    viewRectangle a.shape


movePaddle : Paddle -> Dir -> Paddle
movePaddle p d =
    if d == NoDir then
        p

    else
        { p | shape = moveRectangle p.shape (dir2Float d) 1 }


checkPaddlePosition : Paddle -> ( Float, Float ) -> Paddle
checkPaddlePosition p ( x, y ) =
    { p | shape = checkRectanglePosition p.shape ( x, y ) }
