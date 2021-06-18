module Rectangle exposing (..)

import Color exposing (..)
import Msg exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Tuple exposing (first)


type alias Rectangle =
    { color : Color
    , leftTopPosition : ( Float, Float )
    , width : Float
    , height : Float
    }



{--Give rectangle showing code--}


viewRectangle : Rectangle -> Svg Msg
viewRectangle a =
    Svg.rect
        [ SvgAttr.width (String.fromFloat a.width ++ "px")
        , SvgAttr.height (String.fromFloat a.height ++ "px")
        , SvgAttr.x (String.fromFloat (Tuple.first a.leftTopPosition) ++ "px")
        , SvgAttr.y (String.fromFloat (Tuple.second a.leftTopPosition) ++ "px")
        , SvgAttr.fill (getcolor a.color)
        ]
        []



{--Move rectangle--}


moveRectangle : Rectangle -> Float -> Float -> Rectangle
moveRectangle shape angle length =
    let
        newx =
            Tuple.first shape.leftTopPosition + (cos (Basics.degrees angle) * length * 2)

        newy =
            Tuple.second shape.leftTopPosition - (sin (Basics.degrees angle) * length * 2)
    in
    { shape | leftTopPosition = ( newx, newy ) }



{--Check rectangle position--}


checkRectanglePosition : Rectangle -> ( Float, Float ) -> List Rectangle
checkRectanglePosition r ( x, y ) =
    let
        nx =
            if Tuple.first r.leftTopPosition <= 0 then
                0

            else if Tuple.first r.leftTopPosition + r.width >= x then
                x - r.width

            else
                Tuple.first r.leftTopPosition

        ny =
            if Tuple.second r.leftTopPosition <= 0 then
                0

            else if Tuple.second r.leftTopPosition + r.height >= y then
                y - r.height

            else
                Tuple.second r.leftTopPosition
    in
    [ { r | leftTopPosition = ( nx, ny ) } ]
