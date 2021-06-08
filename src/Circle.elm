module Circle exposing (..)

import Color exposing (..)
import Msg exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


type alias Circle =
    { color : Color
    , position : ( Float, Float )
    , r : Float
    }


initCircle : Color -> ( Float, Float ) -> Float -> Circle
initCircle a b c =
    Circle a b c


viewCircle : Circle -> List (Svg Msg)
viewCircle a =
    let
        ( cx, cy ) =
            a.position
    in
    [ Svg.circle
        [ SvgAttr.cx (String.fromFloat cx ++ "px")
        , SvgAttr.cy (String.fromFloat cy ++ "px")
        , SvgAttr.r (String.fromFloat a.r ++ "px")
        , SvgAttr.fill (getcolor a.color)
        ]
        []
    ]


moveCircle : Circle -> Float -> Float -> Circle
moveCircle c angle length =
    { c | position = ( Tuple.first c.position + (cos (Basics.degrees angle) * length), Tuple.second c.position - (sin (Basics.degrees angle) * length) ) }
