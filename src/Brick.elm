module Brick exposing (..)

import Color exposing (..)
import Debug exposing (toString)
import Dir exposing (..)
import For exposing (..)
import Global exposing (..)
import Html exposing (i)
import Msg exposing (..)
import Random exposing (..)
import RandomNum exposing (getRandomFloat, getRandomInt)
import Rectangle exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr exposing (visibility)


type alias Brick =
    { shape : Rectangle
    , speed : Float
    , direction : Dir
    , show : String
    }

brickrealsizeX=
    30
brickrealsizeY=
    10

bricksizeX =
    50

bricksizeY =
    20


setBricks : { location : Int, speed : Float, dir : Dir } -> Int -> List Brick -> ( List Brick, { location : Int, speed : Float, dir : Dir } )
setBricks a b l =
    ( l ++ [ Brick (Rectangle (Color 101 101 101) ( toFloat b * bricksizeX, (toFloat a.location+2)* bricksizeY ) brickrealsizeX brickrealsizeY) a.speed a.dir "visible" ], a )


initBricks : Int -> Int -> Seed -> ( List Brick, Seed )
initBricks a b s =
    let
        ( l, ( ns, i ) ) =
            for_outer 0 a initBricksLevel ( [], ( s, b ) )
    in
    ( l, ns )


initBricksLevel : ( Seed, Int ) -> Int -> List Brick -> ( List Brick, ( Seed, Int ) )
initBricksLevel ( seed, j ) i l =
    let
        ( line, ns ) =
            setLine i seed

        ( nl, re ) =
            for_outer 0 j setBricks ( l, line )
    in
    ( nl, ( ns, j ) )


setLine : Int -> Seed -> ( { location : Int, speed : Float, dir : Dir }, Seed )
setLine i s =
    let
        ( nspeed, s1 ) =
            getRandomFloat 1 2 s

        ( dirInt, s2 ) =
            getRandomInt 1 2 s1

        ndir =
            if dirInt == 1 then
                Left

            else
                Right
    in
    ( { location = i, speed = nspeed, dir = ndir }, s2 )

viewBrick : Brick -> Svg Msg
viewBrick brick =
    Svg.rect
        [ SvgAttr.width (toString brick.shape.width)
        , SvgAttr.height (toString brick.shape.height)
        , SvgAttr.x (toString (Tuple.first brick.shape.leftTopPosition))
        , SvgAttr.y (toString (Tuple.second brick.shape.leftTopPosition))
        , SvgAttr.fill (getcolor brick.shape.color)
        , SvgAttr.stroke "pink"
        , SvgAttr.visibility brick.show
        
        ]
        []


viewBrickList : List Brick -> List (Svg Msg)
viewBrickList a =
    List.map viewBrick a


updateBricks : List Brick -> List Brick
updateBricks l =
    List.map brickMove l


brickMove : Brick -> Brick
brickMove b =
    let
        newShape1 =
            moveRectangle b.shape (dir2Float b.direction) (b.speed)

        ( newShape2, show ) =
            checkLocation newShape1
    in
    { b | shape = newShape2, show = Maybe.withDefault b.show show }


checkLocation : Rectangle -> ( Rectangle, Maybe String )
checkLocation shape =
    if Tuple.first shape.leftTopPosition + shape.width < -10 then
        ( { shape | leftTopPosition = ( Tuple.first shape.leftTopPosition + wholeWidth+bricksizeX, Tuple.second shape.leftTopPosition ) }, Just "visible" )

    else if Tuple.first shape.leftTopPosition > (10 + wholeWidth) then
        ( { shape | leftTopPosition = ( Tuple.first shape.leftTopPosition - wholeWidth, Tuple.second shape.leftTopPosition ) }, Just "visible" )

    else
        ( shape, Nothing )
