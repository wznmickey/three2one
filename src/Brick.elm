module Brick exposing (..)

import Color exposing (..)
import Debug exposing (toString)
import For exposing (..)
import Msg exposing (..)
import Rectangle exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


type alias Brick =
    { shape : Rectangle
    }


bricksizeX =
    40


bricksizeY =
    20


setBricks : Int -> Int -> List Brick -> List Brick
setBricks a b l =
    l ++ [ Brick (Rectangle (Color 101 101 101) ( toFloat a * bricksizeX, toFloat b * bricksizeY ) bricksizeX bricksizeY) ]


initBricks : Int -> Int -> List Brick
initBricks a b =
    for_nonconstant 0 a (for_constant 0 b setBricks) []


mapsize : ( Int, Int )
mapsize =
    ( 10, 5 )


viewBrick : Brick -> Svg Msg
viewBrick brick =
    Svg.rect
        [ SvgAttr.width (toString bricksizeX)
        , SvgAttr.height (toString bricksizeY)
        , SvgAttr.x (toString (Tuple.first brick.shape.leftTopPosition))
        , SvgAttr.y (toString (Tuple.second brick.shape.leftTopPosition))
        , SvgAttr.fill (getcolor brick.shape.color)
        , SvgAttr.stroke "black"
        ]
        []


viewBrickList : List Brick -> List (Svg Msg)
viewBrickList a =
    List.map viewBrick a
